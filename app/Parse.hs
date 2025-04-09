module Parse (parseArgs, Config (..), Options (..)) where

data Config = Config
    { options :: Options
    , pattern :: String
    , filepaths :: [String]
    }
    deriving (Show)

data Options = Options {highlightMatches :: Bool, caseInsensitive :: Bool} deriving (Show)

parseArgs :: [String] -> Maybe Config
parseArgs [] = Nothing
parseArgs args =
    let (options, args') = extractOptions args
     in case args' of
            (pattern : filepaths) -> Just Config{options, pattern, filepaths}
            unknownPattern -> error $ show unknownPattern

extractOptions :: [String] -> (Options, [String])
extractOptions [] = (Options{highlightMatches = False, caseInsensitive = False}, [])
extractOptions (arg : rest)
    | isOption arg = case arg of
        "--color" -> add $ options{highlightMatches = True}
        "--case-insensitive" -> add $ options{caseInsensitive = True}
        _ -> undefined
    | otherwise =
        let (resultOptions, resultArguments) = extractOptions rest
         in (resultOptions, arg : resultArguments)
  where
    isOption text = head text == '-'
    (options, args) = extractOptions rest
    add options' = (options', args)
