module Jobs where

import Control.Monad (forM_, when)
import qualified Data.Map.Strict as M
import Data.List (sort)
import System.IO (stderr, hPutStrLn)
import System.Posix.Process
import System.Posix.Types (ProcessID)
import qualified State as S

nextJobNum :: M.Map Int a -> Int
nextJobNum mp = head $ filter (`M.notMember` mp) [1..]

startBackgroundJob :: [String] -> S.ShellM Int
startBackgroundJob [] = pure 0
startBackgroundJob args@(cmd:rest) = do
  st <- S.getShell
  pid <- S.liftShell forkProcessChild
  let jobNum = nextJobNum (S.jobs st)
    cmdStr = unwords args ++ " &"
  S.modifyShell $ \s -> s { S.jobs = M.insert jobNum (S.JobInfo pid cmdStr S.Running) (S.jobs s) }
  S.liftShell $ putStrLn ("[" ++ show jobNum ++ "] " ++ show pid)
  pure jobNum
  where
    forkProcessChild = forkProcess $ do
      createSession
      executeFile cmd True rest Nothing

updateJobStatus :: S.ShellM ()
updateJobStatus = do
  st <- S.getShell
  pairs <- mapM markOne (M.toList $ S.jobs st)
  S.modifyShell $ \s -> s { S.jobs = M.fromList pairs }
  where
    markOne (n, info) =
      case S.jobStatus info of
        S.Done -> pure (n, info)
        S.Running -> do
          res <- S.liftShell $ getProcessStatus False False (S.jobPid info)
          pure $ case res of
            Nothing -> (n, info)
            Just _ -> (n, info { S.jobStatus = S.Done })

reapJobs :: S.ShellM ()
reapJobs = do
  updateJobStatus
  st <- S.getShell
  let finished = [(n,i) | (n,i) <- M.toAscList (S.jobs st), S.jobStatus i == S.Done]
  forM_ finished $ \(n, info) -> do
    let cmd = dropAmp (S.jobCmd info)
    S.liftShell $ putStrLn ("[" ++ show n ++ "]+ Done " ++ cmd)
  S.modifyShell $ \s -> s { S.jobs = M.filter ((/= S.Done) . S.jobStatus) (S.jobs s) }

handleJobs :: [String] -> S.ShellM ()
handleJobs args = do
  updateJobStatus
  st <- S.getShell
  let nums = sort (M.keys $ S.jobs st)
  if null nums
    then pure ()
    else case drop 1 args of
      (target:_) ->
        let stripped = dropWhile (== '%') target
        in case reads stripped of
          [(n,"")] ->
            case M.lookup n (S.jobs st) of
              Nothing -> S.liftShell $ hPutStrLn stderr ("jobs: " ++ target ++ ": no such job")
              Just info -> printOne nums n info
          _ -> mapM_ (printExisting nums st) nums
      _ -> mapM_ (printExisting nums st) nums
  st2 <- S.getShell
  S.modifyShell $ \s -> s { S.jobs = M.filter ((/= S.Done) . S.jobStatus) (S.jobs st2) }
  where
    printExisting nums st n = case M.lookup n (S.jobs st) of
      Just info -> printOne nums n info
      Nothing -> pure ()

    printOne nums n info = do
      let marker = if n == last nums then "+" else if length nums > 1 && n == nums !! (length nums - 2) then "-" else " "
        status = case S.jobStatus info of
          S.Running -> "Running"
          S.Done -> "Done"
        cmd = if status == "Done" then dropAmp (S.jobCmd info) else S.jobCmd info
      S.liftShell $ putStrLn ("[" ++ show n ++ "]" ++ marker ++ " " ++ padRight 24 status ++ " " ++ cmd)

    padRight n s = take n (s ++ repeat ' ')
    dropAmp s = reverse . dropWhile (== ' ') . dropWhile (== '&') . dropWhile (== ' ') . reverse $ s