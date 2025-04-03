{-# LANGUAGE NamedFieldPuns #-}

module Parse (parseArgs, Config (..), Option (..)) where

data Config = Config
    { options :: [Option]
    , pattern :: String
    , filepaths :: [String]
    }
    deriving (Show)

data Option
    = ColoredOutput
    | ExitProgramNow
    deriving (Show)

parseArgs :: [String] -> Maybe Config
parseArgs [] = Nothing
parseArgs args =
    let (options, args') = extractOptions args
     in case args' of
            (pattern : filepaths) -> Just Config{options, pattern, filepaths}
            _ -> error $ show options

extractOptions :: [String] -> ([Option], [String])
extractOptions [] = ([], [])
extractOptions
    allArgs@(arg : rest)
        | isOption arg = case arg of
            "--color" -> insertOption ColoredOutput
            "--exit" -> insertOption ExitProgramNow
            _ -> undefined
        | otherwise = ([], allArgs)
      where
        isOption text = head text == '-'
        insertOption option' =
            let (options, rest') = extractOptions rest
             in (option' : options, rest')
