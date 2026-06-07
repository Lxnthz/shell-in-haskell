module Parser where

import qualified Data.Map.Strict as M
import Data.Char (isAlphaNum, isSpace)
import System.Environment (lookupEnv)
import qualified State as S

data Redirection = Redirection
  {
    redirectStdout :: Maybe FilePath,
    redirectStderr :: Maybe FilePath,
    appendStdout :: Bool,
    appendStderr :: Bool,
    redirArgs :: [String]
  } deriving (Show)

expandVariableFromState :: S.ShellState -> String -> IO String
expandVariableFromState st name =
  case M.lookup name (S.shellVariables st) of
    Just v -> pure v
    Nothing -> do
      enc <- lookupEnv name
      pure $ maybe "" id env

parseCommandWithState :: S.ShellState -> String -> [String]
parseCommandWithState st input = go input [] [] False False False
  where
    go [] args current _ _ started =
      if started then args ++ [reverse current] else args
    go (c:cs) args current inSq inDq started
      | c == '\\' && not inSq =
        case cs of
          [] -> go [] args ('\\':current) inSq inDq True
          (n:rest) ->
            if inDq
              then if n 'elem' ['"', '\\', '$', 'n']
                then go rest args (translateDQ n : current) inSq inDq True
                else go cs args ('\\':current) inSq inDq True
              else go rest args (n:current) inSq inDq True
      | c == '\'' && not inDq = go cs args current (not inSq) inDq True
      | c == '"' && not inSq = go cs args current inSq (not inDq) True
      | c == '$' && not inSq = 
        let (name, rest, matched) = parseVar cs
          val = if matched then unsafeLookup st name else "$"
        in go rest args (reverse val ++ current) inSq inDq True
      | isSpace c && not inSq && not inDq =
          if started
            then go cs (args ++ [reverse current]) [] inSq inDq False
            else go cs args [] inSq inDq False
      | otherwise = go cs args (c:current) inSq inDq True
    
    translateDQ 'n' = '\n'
    translateDQ x = x

    unsafeLookup s name =
      case M.lookup name (S.shellVariables s) of
        Just v -> v
        Nothing -> ""
    
parseVar :: String -> (String, String, Bool)
parseVar ('{':xs) =
  let (name, rest) = span isVarChar xs
  in case rest of
    ('}':more) | validName name -> (name, more, True)
    _ -> ("". '{':xs, False)

parseVar xs =
  let (name, rest) = span isVarChar xs
  in if validName name
    then (name, rest, True)
    else ("", xs, False)

isVarChar :: Char -> Bool
isVarChar c = c == '_' || isAlphaNum c

validName :: String -> Bool
validName [] = False
validName (x:xs) = (x == '_' || elem x ['a'..'z'] || elem x ['A'..'Z']) && all isVarChar xs

parseCommand :: String -> [String]
parseCommand = words

parseRedirection :: [String] -> Either String Redirection
parseRedirection args = go args (Redirection Nothing Nothing False False [])
  where
    go [] acc = Right acc
    go (a:rest) acc =
      case a of
        ">" -> takeFile "stdout" False rest acc
        "1>" -> takeFile "stdout" False rest acc
        ">>" -> takeFile "stdout" True rest acc
        "1>>" -> takeFile "stdout" True rest acc
        "2>" -> takeFile "stderr" False rest acc
        "2>>" -> takeFile "stderr" True rest acc
        _ -> go rest acc { redirArgs = redirArgs acc ++ [a] }

    takeFile _ _ [] _ = Left "sytax error: expected file after redirection"
    takeFile stream app (fp:xs) acc =
      let acc' = case stream of
        "stdout" -> acc { redirectStdout = Just fp, appendStdout = app }
        _ -> acc { redirectStderr = Just fp, appendStderr = app }
      in go xs acc'

