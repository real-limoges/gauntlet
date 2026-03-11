module Benchmark.Config.Env
  ( parseEnvFile
  , loadEnvVars
  , interpolateEnv
  )
where

import Control.Exception (IOException, try)
import Data.Bifunctor (bimap)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)

{-| Parse a .env file into a map of key-value pairs.
Skips blank lines and lines starting with '#'.
Strips an optional 'export ' prefix; strips surrounding single or double quotes from values.
-}
parseEnvFile :: Text -> Map Text Text
parseEnvFile content = Map.fromList $ mapMaybe parseLine (T.lines content)
  where
    parseLine line =
      let stripped = T.strip line
       in if T.null stripped || T.isPrefixOf "#" stripped
            then Nothing
            else
              let noExport = maybe stripped T.stripStart (T.stripPrefix "export " stripped)
               in case T.breakOn "=" noExport of
                    (_, rest) | T.null rest -> Nothing
                    (key, valWithEq) ->
                      let key' = T.strip key
                          val' = stripQuotes (T.strip (T.drop 1 valWithEq))
                       in if T.null key' then Nothing else Just (key', val')
    stripQuotes t
      | T.length t >= 2 && T.head t == '"' && T.last t == '"' = T.init (T.tail t)
      | T.length t >= 2 && T.head t == '\'' && T.last t == '\'' = T.init (T.tail t)
      | otherwise = t

{-| Load environment variables from process env, .env, and .env.local.
Precedence (highest wins): .env.local > .env > process environment.
Missing files are silently ignored.
-}
loadEnvVars :: IO (Map Text Text)
loadEnvVars = do
  procEnv <- Map.fromList . map (bimap T.pack T.pack) <$> getEnvironment
  dotEnv <- readEnvFile ".env"
  dotEnvLocal <- readEnvFile ".env.local"
  return $ Map.unions [dotEnvLocal, dotEnv, procEnv]

readEnvFile :: FilePath -> IO (Map Text Text)
readEnvFile path = do
  exists <- doesFileExist path
  if exists
    then do
      result <- try (TIO.readFile path) :: IO (Either IOException Text)
      case result of
        Left _ -> return Map.empty
        Right content -> return (parseEnvFile content)
    else return Map.empty

{-| Interpolate \${VAR} patterns in the given text using the provided map.
Returns Left with an error message if a referenced variable is not defined.
An unclosed '\${' with no matching '}' is left as a literal.
-}
interpolateEnv :: Map Text Text -> Text -> Either String Text
interpolateEnv env = go mempty
  where
    go acc remaining =
      case T.breakOn "${" remaining of
        (before, rest)
          | T.null rest -> Right (acc <> before)
          | otherwise ->
              let afterOpen = T.drop 2 rest
               in case T.breakOn "}" afterOpen of
                    (_, noClose)
                      | T.null noClose ->
                          Right (acc <> before <> rest)
                    (varName, afterClose) ->
                      case Map.lookup varName env of
                        Nothing -> Left ("undefined environment variable: " <> T.unpack varName)
                        Just val -> go (acc <> before <> val) (T.drop 1 afterClose)
