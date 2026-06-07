module Executor where

import Control.Monad (forM_, unless, when)
import System.Exit (ExitCode(..))
import System.IO (stderr, hPutStrLn)
import System.Posix.IO
import System.Posix.Process
import System.Posix.Types
import qualified System.Process as P
import qualified State as S
import Parser
import Builtins
import Jobs (handleJobs)

executeCommand :: [String] -> Redirection -> Maybe Handle -> Maybe Handle -> S.ShellM Int
executeCommand [] _ _ _ = pure 0
executeCommand args redir _ _ = do
  let cp0 = (P.proc (head args) (tail args))
      cp1 = maybe cp0 (\fp -> cp0 { P.std_out = if appendStdout redir then P.UseHandle =<< undefined else P.CreatePipe }) (redirectStdout redir)
  result <- S.liftShell $ P.rawSystem (head args) (tail args)
  pure $ case result of
    ExitSuccess -> 0
    ExitFailure n -> n

executePipeline :: String -> S.ShellM ()
executePipeline commandStr = do
  st <- S.getShell
  let commands = map strip $ splitPipe commandStr
      parsedCmds = map (parseRedirection . parseCommandWithState st) commands
  pids <- runStages parsedCmds Nothing []
  S.liftShell $ mapM_ (\pid -> getProcessStatus True False pid >> pure ()) pids
  pure ()

runStages :: [Either String Redirection] -> Maybe Fd -> [ProcessID] -> S.ShellM [ProcessID]
runStages [] prev acc = do
  maybe (pure ()) (S.liftShell . closeFd) prev
  pure (reverse acc)
runStages (x:xs) prev acc =
  case x of
    Left err -> do
      S.liftShell $ hPutStrLn stderr err
      maybe (pure ()) (S.liftShell . closeFd) prev
      pure (reverse acc)
    Right redir -> do
      let args = redirArgs redir
          isLast = null xs
      if null args
        then runStages xs prev acc
        else do
          pipeFds <- if isLast then pure Nothing else Just <$> S.liftShell createPipe
          pid <- S.liftShell $ forkProcess $ childRun prev pipeFds isLast args redir
          maybe (pure ()) (S.liftShell . closeFd) prev
          case pipeFds of
            Just (r,w) -> S.liftShell (closeFd w) >> runStages xs (Just r) (pid:acc)
            Nothing -> runStages xs Nothing (pid:acc)

childRun :: Maybe Fd -> Maybe (Fd, Fd) -> Bool -> [String] -> Redirection -> IO ()
childRun prev pipeFds isLast args redir = do
  case prev of
    Just fd -> dupTo fd stdInput >> closeFd fd
    Nothing -> pure ()
  case pipeFds of
    Just (r,w) ->
      if redirectStdout redir == Nothing
        then dupTo w stdOutput >> closeFd w >> closeFd r
        else closeFd w >> closeFd r
    Nothing -> pure ()
  case redirectStdout redir of
    Just fp -> do
      fd <- openFd fp WriteOnly (Just 0o644) defaultFileFlags { append = appendStdout redir, trunc = not (appendStdout redir), creat = True }
      dupTo fd stdOutput
      closeFd fd
    Nothing -> pure ()
  case redirectStderr redir of
    Just fp -> do
      fd <- openFd fp WriteOnly (Just 0o644) defaultFileFlags { append = appendStderr redir, trunc = not (appendStderr redir), creat = True }
      dupTo fd stdError
      closeFd fd
    Nothing -> pure ()
  runInChild args

runInChild :: [String] -> IO ()
runInChild [] = pure ()
runInChild args =
  case head args of
    "echo" -> putStrLn (unwords (tail args))
    "pwd" -> P.callCommand "pwd"
    "cd" -> pure ()
    "type" -> pure ()
    "history" -> pure ()
    "declare" -> pure ()
    "jobs" -> pure ()
    "exit" -> pure ()
    cmd -> executeFile cmd True (tail args) Nothing

splitPipe :: String -> [String]
splitPipe [] = [""]
splitPipe s = go s "" False False
  where
    go [] acc _ _ = [reverse acc]
    go (c:cs) acc sq dq
      | c == '|' && not sq && not dq = reverse acc : go cs "" sq dq
      | c == '\'' && not dq = go cs (c:acc) (not sq) dq
      | c == '"' && not sq = go cs (c:acc) sq (not dq)
      | otherwise = go cs (c:acc) sq dq

strip :: String -> String
strip = reverse . dropWhile (== ' ') . dropWhile (== '\t') . reverse . dropWhile (== ' ') . dropWhile (== '\t')