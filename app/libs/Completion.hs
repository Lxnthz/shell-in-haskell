module Completion where

import Control.Monad (forM)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import System.Directory (doesDirectoryExist, listDirectory, doesFileExist)
import System.Environment (lookupEnv, getEnvironment)
import System.FilePath ((</>), splitFileName)
import System.Process (readProcess)
import qualified System.Console.Haskeline as H
import qualified State as S

commonPrefix :: [String] -> String
commonPrefix [] = ""
commonPrefix (x:xs) = foldl step x xs
  where
    step acc s = map fst $ takeWhile (uncurry (==)) $ zip acc s

completePath :: String -> IO [String]
completePath prefix = do
  let (dir, partial) = splitFileName prefix
      base = if null dir then "." else init dir
  ok <- doesDirectoryExist base
  if not ok
    then pure []
    else do
      entries <- listDirectory base
      fmap L.sort $ forM (filter (L.isPrefixOf partial) entries) $ \entry -> do
        let full = if null dir then entry else dir ++ entry
        isDir <- doesDirectoryExist full
        pure $ if isDir then full ++ "/" else full

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy c s = let (a,b) = break (== c) s in a : case b of
  [] -> []
  (_:xs) -> splitBy c xs

completeCommands :: S.ShellState -> String -> IO [String]
completeCommands st text = do
  mp <- lookupEnv "PATH"
  let dirs = maybe [] (splitBy ':') mp
    builtins = filter (L.isPrefixOf text) (S.builtinCommands st)
  pathEntries <- fmap concat $ forM dirs $ \d -> do
    ok <- doesDirectoryExist d
    if not ok then pure [] else do
      xs <- listDirectory d
      pure [e | e <- xs, L.isPrefixOf text e]
  pure . L.nub . L.sort $ builtins ++ pathEntries

handleComplete :: [String] -> S.ShellM ()
handleComplete args = do
  st <- S.getShell
  case args of
    [_] -> printAll st
    [_, "-p"] -> printAll st
    [_, "-p", cmd] ->
      case M.lookup cmd (S.completeSpecs st) of
        Nothing -> S.liftShell $ putStrLn ("complete: " ++ cmd ++ ": no completion specification")
        Just spec -> S.liftShell $ putStrLn (formatSpec cmd spec)
    [_, "-r", cmd] -> S.modifyShell $ \s -> s { S.completeSpecs = M.delete cmd (S.completeSpecs s) }
    (_:rest) -> registerSpec rest
    _ -> pure ()
  where
    printAll st =
      if M.null (S.completeSpecs st)
        then S.liftShell $ putStrLn "complete: no programmable completions registered"
        else mapM_ (S.liftShell . putStrLn . uncurry formatSpec) (M.toAscList $ S.completeSpecs st)

    formatSpec cmd spec =
      let flagsStr = unwords [f ++ " '" ++ v ++ "'" | (f,v) <- S.specFlags spec]
      in unwords $ filter (not . null) ["complete", flagsStr, cmd]

    registerSpec xs = do
      let (flags, envs, cmds) = parseFlags xs [] M.empty []
        spec = S.CompleteSpec flags envs
      mapM_ (\cmd -> S.modifyShell $ \s -> s { S.completeSpecs = M.insert cmd spec (S.completeSpecs s) }) cmds

    parseFlags [] fs es cs = (reverse fs, es, reverse cs)
    parseFlags (f:v:rest) fs es cs | f `elem` ["-F","-C","-A","-W"] = parseFlags rest ((f,v):fs) es cs
    parseFlags (f:v:rest) fs es cs | f == "-e" || f == "--env" =
      let (k,eqv) = break (== '=') v
        val = drop 1 eqv
      in parseFlags rest fs (M.insert k val es) cs
    parseFlags (x:rest) fs es cs = parseFlags rest fs es (x:cs)

haskelineCompletion :: H.CompletionFunc S.ShellM
haskelineCompletion (left, right) = do
  st <- H.lift S.getShell
  let toks = words left
    current = reverse (takeWhile (/= ' ') (reverse left))
    cmdMode = null toks || (length toks == 1 && not (null left) && last left /= ' ')
  matches <- H.lift $ S.liftShell $
    if cmdMode
      then completeCommands st current
      else case toks of
        (cmd:_) | M.member cmd (S.completeSpecs st) -> programmableMatches st cmd toks current
        _ -> completePath current
  let prefix = if length matches > 1 then commonPrefix matches else current
    enriched = if length matches > 1 && prefix /= current then prefix : matches else matches
  pure (left, map H.simpleCompletion (L.nub enriched))

programmableMatches :: S.ShellState -> String -> [String] -> String -> IO [String]
programmableMatches st cmd toks text =
  case M.lookup cmd (S.completeSpecs st) of
    Nothing -> pure []
    Just spec -> do
      results <- fmap concat $ forM (S.specFlags spec) $ \(flag, value) ->
        case flag of
          "-A" | value == "file" -> completePath text
          "-A" | value == "command" -> completeCommands st text
          "-A" | value == "directory" -> filter (L.isSuffixOf "/") <$> completePath text
          "-W" -> pure [w | w <- words value, L.isPrefixOf text w]
          "-F" -> callExternal spec value
          "-C" -> callExternal spec value
          _ -> pure []
      pure $ L.sort . L.nub $ results
  where
    callExternal spec prog = do
      env <- getEnvironment
      let prevWord = if null text then last toks else if length toks >= 2 then toks !! (length toks - 2) else ""
        extra = M.toList (S.specEnv spec) ++ [("COMP_LINE", unwords toks), ("COMP_POINT", show (length (unwords toks)))]
      output <- readProcess prog [cmd, text, prevWord] ""
      pure [w | w <- words output, L.isPrefixOf text w]