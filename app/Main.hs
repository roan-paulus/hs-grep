{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import qualified Ansi (red)
import qualified Constants (helpMsg)
import Control.Exception (SomeException, try)
import Parse (Config (..))
import qualified Parse
import Search
import qualified System.Environment as Env
import Types (FileBundle (..))

main :: IO ()
main = do
    config <- Parse.parseArgs <$> Env.getArgs
    case config of
        Just config' -> do
            files <- getFiles config'.filepaths
            let searchResults = concatMap (search config'.pattern) files
            if null searchResults
                then putStrLn "No matches found."
                else mapM_ (putStrLn . formatSearchResult) searchResults
        Nothing -> do
            putStrLn "Config could not be made"
            putStrLn Constants.helpMsg

-- Take a search result and return a formatted version of the line with the match highlighted within it.
formatSearchResult :: SearchResult -> String
formatSearchResult result =
    show (result.lineNumber + 1) ++ "," ++ show (firstMatch.startIndex + 1) ++ " :: " ++ formattedLine result
  where
    firstMatch = head result.matches

formattedLine :: SearchResult -> String
formattedLine (SearchResult{lineContent, matches = []}) = lineContent
formattedLine (SearchResult{path, lineContent, lineNumber, matches = (match : remainingMatches)}) =
    let start = match.startIndex
        end = match.endIndex
        beforeMatch = take start lineContent
        matchedWord = take (end - start + 1) $ drop start lineContent
        afterMatch = drop (end + 1) lineContent
        deletedSliceLength = length beforeMatch + length matchedWord
        offsettedRemaningMatches =
            map
                ( \m ->
                    Slice
                        { startIndex = m.startIndex - deletedSliceLength
                        , endIndex = m.endIndex - deletedSliceLength
                        }
                )
                remainingMatches
     in beforeMatch
            ++ Ansi.red matchedWord
            ++ formattedLine
                SearchResult
                    { path
                    , lineContent = afterMatch
                    , lineNumber = lineNumber -- TODO: Useless data to keep around.
                    , matches = offsettedRemaningMatches
                    }

getFiles :: [FilePath] -> IO [Types.FileBundle]
getFiles [] = pure []
getFiles (path : rest) = do
    result <- try $ readFile path :: IO (Either SomeException String)
    case result of
        Left ex -> do
            putStrLn $ "Caught exception: " ++ show ex
            getFiles rest
        Right contents -> do
            (FileBundle{path, contents = contents} :) <$> getFiles rest
