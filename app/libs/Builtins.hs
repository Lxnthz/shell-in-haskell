module Builtins where

import Control.Monad (forM_, when)
import qualified Data.Map.Strict as M
import Data.Char (isAlphaNum)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.Environment (getEnv, lookupEnv, getExecutablePath)
import System.FilePath ((</>))
import System.IO (stderr, hPutStrLn)
import qualified State as S
import Parser (Redirection(..))

writeOut :: Redirection -> String -> S.ShellM ()
writeOut redir output = do
  case redirectStdout redir of
    Just fp -> S.liftShell $ writeFileMode (appendStdout redir) fp output
    Nothing -> S.liftShell $ putStr output
  case redirectStderr redir of
    Just fp -> S.liftShell $ writeFileMode (appendStderr redir) fp ""
    Nothing -> pure ()

writeFileMode :: Bool -> FilePath -> String -> IO ()
writeFileMode app fp content = if app then appendFile fp content else writeFile fp content

handleEcho :: [String] -> Redirection -> S.ShellM ()
handleEcho args redir = writeOut redir (unwords (drop 1 args) ++ "\n")

handlePwd :: S.ShellM ()
handlePwd = S.liftShell getCurrentDirectory >>= S.liftShell . putStrLn

handleCd :: [String] -> S.ShellM ()
handleCd args = do
  if length args < 2
    then S.liftShell $ hPutStrLn stderr "cd: missing argument"
    else do
      home <- S.liftShell $ lookupEnv "HOME"
      let raw = args !! 1
        path = if raw == "~" then maybe raw id home else raw
      S.liftShell (setCurrentDirectory path) 'catchShell' \_ -> S.liftShell $ hPutStrLn stderr ("cd: " ++ path ++ ": No such file or directory")

catchShell :: S.ShellM a -> (IOError -> S.ShellM a) -> S.ShellM a
catchShell action handler = do
  st <- S.getShell
  result <- S.liftShell $ (Right <$> S.runShellM st action) 'catch' (pure . Left)
  case result of
    Left e -> handler e
    Right (v, st') -> S.putShell st' >> pure v

handleType :: [String] -> S.ShellM ()
handleType args = do
  st <- S.getShell
  forM_ (drop 1 args) $ \name ->
    if name `elem` S.builtinCommands st
      then S.liftShell $ putStrLn (name ++ " is a shell builtin")
      else do
        found <- S.liftShell $ findInPath name
        case found of
          Just fp -> S.liftShell $ putStrLn (name ++ " is " ++ fp)
          Nothing -> S.liftShell $ hPutStrLn stderr (name ++ ": not found")

findInPath :: String -> IO (Maybe FilePath)
findInPath name = do
  mp <- lookupEnv "PATH"
  let dirs = maybe [] (splitBy ':') mp
  go dirs
  where
    go [] = pure Nothing
    go (d:ds) = do
      let fp = d </> name
      ex <- doesFileExist fp
      if ex then pure (Just fp) else go ds

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy c s = let (a,b) = break (== c) s in a : case b of
  [] -> []
  (_:xs) -> splitBy c xs

handleHistory :: [String] -> S.ShellM ()
handleHistory _ = do
  st <- S.getShell
  let entries = zip [1 :: Int ..] (S.manualHistory st)
  forM_ entries $ \(n, cmd) -> S.liftShell $ putStrLn (show n ++ "  " ++ cmd)

validName :: String -> Bool
validName [] = False
validName (x:xs) = (x == '_' || elem x ['a'..'z'] || elem x ['A'..'Z']) && all isVarChar xs
  where isVarChar c = c == '_' || isAlphaNum c

handleDeclare :: [String] -> S.ShellM ()
handleDeclare args = do
  st <- S.getShell
  case args of
    [_] -> printAllVars st
    [_, "-p"] -> printAllVars st
    [_, "-p", name] ->
      case M.lookup name (S.shellVariables st) of
        Just v -> S.liftShell $ putStrLn ("declare -- " ++ name ++ "=\"" ++ v ++ "\"")
        Nothing -> S.liftShell $ hPutStrLn stderr ("declare: " ++ name ++ ": not found")
    (_:rest) -> mapM_ handleToken rest
    _ -> pure ()
  where
    printAllVars st =
      if M.null (S.shellVariables st)
        then S.liftShell $ hPutStrLn stderr "declare: no variables set"
        else forM_ (M.toAscList $ S.shellVariables st) $ \(k,v) ->
          S.liftShell $ putStrLn ("declare -- " ++ k ++ "=\"" ++ v ++ "\"")

    handleToken tok =
      case break (== '=') tok of
        (name, '=':value) ->
          if validName name
            then S.modifyShell $ \s -> s { S.shellVariables = M.insert name value (S.shellVariables s) }
            else S.liftShell $ hPutStrLn stderr ("declare: `" ++ tok ++ "': not a valid identifier")
        (name, "") -> do
          st <- S.getShell
          if not (validName name)
            then S.liftShell $ hPutStrLn stderr ("declare: `" ++ tok ++ "': not a valid identifier")
            else case M.lookup name (S.shellVariables st) of
              Nothing -> S.liftShell $ hPutStrLn stderr ("declare: " ++ name ++ ": not found")
              Just v -> S.liftShell $ putStrLn ("declare -- " ++ name ++ "=\"" ++ v ++ "\"")
        _ -> pure ()