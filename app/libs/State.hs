module State where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import System.Posix.Types (ProcessID)

data JobStatus = Running | Done deriving (Eq, Show)

data JobInfo = JobInfo
  {
    jobPid :: ProcessID,
    jobCmd :: String,

  runShellM :: ShellState -> ShellM a -> IO (a, ShellState)
  runShellM st m = runStateT m st
    jobStatus :: JobStatus
  } deriving (Show)

data CompleteSpec = CompleteSpec
  {
    specFlags :: [(String, String)],
    specEnv :: M.Map String String
  } deriving (Show)

data ShellState = ShellState
  {
    builtinCommands :: [String],
    manualHistory :: [String],
    historyBaseForAppend :: Int,
    shellVariables :: M.Map String String,
    jobs :: M.Map Int JobInfo,
    completeSpecs :: M.Map String CompleteSpec,
    histFile :: Maybe FilePath
  } deriving (Show)

type ShellM = StateT ShellState IO

initialShellState :: IO ShellState

initialShellState = pure $ ShellState
  {
    builtinCommands = ["echo", "cd", "history", "type", "pwd", "cd", "jobs", "complete"],
    manualHistory = [],
    historyBaseForAppend = 0,
    shellVariables = M.empty,
    jobs = M.empty,
    completeSpecs = M.empty,
    histFile = Nothing
  }

runShellM :: ShellState -> ShellM a -> IO (a, ShellState)
runShellM = runStateT

getShell :: ShellM ShellState
getShell = get

modifyShell :: (ShellState -> ShellState) -> ShellM ()
modifyShell = modify

putShell :: ShellState -> ShellM ()
putShell = put

liftShell :: IO a -> ShellM a
liftShell = liftIO

setHistFile :: Maybe FilePath -> ShellM ()
setHistFile fp = modify $ \s -> s { histFile = fp }