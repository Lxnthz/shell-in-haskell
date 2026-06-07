module Main where

import Control.Exception (IOException, catch)
import Control.Monad (unless, when)
import qualified Data.List as L
import qualified System.Console.Haskeline as H
import System.Environment (lookupEnv)
import System.Exit (exitSuccess, exitWith, ExitCode(..))
import System.IO (hFlush, stdout, stderr, hPutStrLn)

import qualified State as S
import Parser
import Executor
import Jobs
import Builtins
import Completion

loadHistory :: S.ShellM ()
loadHistory = do
  mh <- liftIO' $ lookupEnv "HISTFILE"
  S.setHistFile mh
  case mh of
    Nothing -> pure ()
    Just fp -> do
      content <- liftIO' $ readFile fp `catch` ignoreIO ""
      let entries = filter (not . null) (lines content)
      S.modifyShell $ \st -> st
        { S.manualHistory = entries
        , S.historyBaseForAppend = length entries
        }

saveHistory :: S.ShellM ()
saveHistory = do
  st <- S.getShell
  case S.histFile st of
    Nothing -> pure ()
    Just fp -> do
      let newCmds = drop (S.historyBaseForAppend st) (S.manualHistory st)
      unless (null newCmds) $ liftIO' $
        appendFile fp (unlines newCmds) `catch` ignoreIO ()

ignoreIO :: a -> IOException -> IO a
ignoreIO x _ = pure x

liftIO' :: IO a -> S.ShellM a
liftIO' = S.liftShell

runBuiltin :: [String] -> Redirection -> S.ShellM Bool
runBuiltin [] _ = pure True
runBuiltin args redir =
  case head args of
    "exit" -> do
      let code = case drop 1 args of
            (x:_) -> case reads x of
              [(n,"")] -> n
              _ -> 0
            _ -> 0
      liftIO' $ exitWith (if code == 0 then ExitSuccess else ExitFailure code)
      pure True
    "echo" -> handleEcho args redir >> pure True
    "pwd" -> handlePwd >> pure True
    "cd" -> handleCd args >> pure True
    "type" -> handleType args >> pure True
    "history" -> handleHistory args >> pure True
    "declare" -> handleDeclare args >> pure True
    "jobs" -> handleJobs args >> pure True
    "complete" -> handleComplete args >> pure True
    _ -> pure False

repl :: H.InputT S.ShellM ()
repl = do
  _ <- H.getInputLine "" -- warmup no-op impossible; kept structure simple
  pure ()

shellLoop :: H.InputT S.ShellM ()
shellLoop = do
  H.outputStr "$ "
  minput <- H.getInputLine ""
  case minput of
    Nothing -> H.outputStrLn ""
    Just line -> do
      H.lift $ reapJobs
      H.lift $ appendHistoryIfNeeded line
      if null (words line)
        then shellLoop
        else do
          H.lift $ processLine line
          shellLoop

appendHistoryIfNeeded :: String -> S.ShellM ()
appendHistoryIfNeeded line = when (not (null line)) $ do
  st <- S.getShell
  let lastLine = if null (S.manualHistory st) then Nothing else Just (last (S.manualHistory st))
  when (lastLine /= Just line) $ S.modifyShell $ \s -> s { S.manualHistory = S.manualHistory s ++ [line] }

processLine :: String -> S.ShellM ()
processLine raw = do
  let trimmed = rstrip raw
      background = not (null trimmed) && last trimmed == '&'
      line = if background then rstrip (init trimmed) else raw
  if '|' `elem` line
    then executePipeline line
    else do
      let args0 = parseCommand =<< pure line
      st <- S.getShell
      let args = parseCommandWithState st line
      case parseRedirection args of
        Left err -> liftIO' $ hPutStrLn stderr err
        Right parsed -> do
          let cmdArgs = redirArgs parsed
          unless (null cmdArgs) $
            if background
              then startBackgroundJob cmdArgs
              else do
                handled <- runBuiltin cmdArgs parsed
                unless handled $ do
                  _ <- executeCommand cmdArgs parsed Nothing Nothing
                  pure ()

rstrip :: String -> String
rstrip = reverse . dropWhile (`elem` [' ','\t','\n','\r']) . reverse

settings :: H.Settings S.ShellM
settings = H.Settings
  { H.complete = completeInput
  , H.historyFile = Nothing
  , H.autoAddHistory = False
  }

completeInput :: H.CompletionFunc S.ShellM
completeInput = haskelineCompletion

main :: IO ()
main = do
  st <- S.initialShellState
  _ <- S.runShellM st $ do
    loadHistory
    H.runInputT settings shellLoop
    saveHistory
  pure ()