{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import qualified Ansi (red)
import qualified Constants (helpMsg)
import Control.Exception (SomeException, try)
import Parse (Config (..))
import qualified Parse
import qualified System.Environment as Env

main :: IO ()
main = do
    config <- Parse.parseArgs <$> Env.getArgs
    case config of
        Just config' -> do
            files <- getFiles config'.filepaths
            let searchResults = concatMap (search config'.pattern . fileContents) files
            if null searchResults
                then putStrLn "No matches found."
                else mapM_ (putStrLn . formatSearchResult) searchResults
        Nothing -> do
            putStrLn "Config could not be made"
            putStrLn Constants.helpMsg

type FileContents = String

data FileBundle = FileBundle {filepath :: FilePath, fileContents :: FileContents}

-- Take a search result and return a formatted version of the line with the match highlighted within it.
formatSearchResult :: SearchResult -> String
formatSearchResult result =
    show (result.lineNumber + 1) ++ "," ++ show (firstMatch.startIndex + 1) ++ " :: " ++ formattedLine result
  where
    firstMatch = head result.matches

formattedLine :: SearchResult -> String
formattedLine (SearchResult{lineContent, matches = []}) = lineContent
formattedLine (SearchResult{lineContent, lineNumber, matches = (match : remainingMatches)}) =
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
                    { lineContent = afterMatch
                    , lineNumber = lineNumber -- TODO: Useless data to keep around.
                    , matches = offsettedRemaningMatches
                    }

getFiles :: [FilePath] -> IO [FileBundle]
getFiles [] = pure []
getFiles (path : rest) = do
    result <- try $ readFile path :: IO (Either SomeException String)
    case result of
        Left ex -> do
            putStrLn $ "Caught exception: " ++ show ex
            getFiles rest
        Right contents -> do
            (FileBundle{filepath = path, fileContents = contents} :) <$> getFiles rest

data SearchResult = SearchResult {lineContent :: String, lineNumber :: Int, matches :: [Slice]}
data Slice = Slice {startIndex :: Int, endIndex :: Int}

type QueryString = String

search :: QueryString -> FileContents -> [SearchResult]
search query fileContents' = searchLines $ zip [0 ..] $ lines fileContents'
  where
    searchLines [] = []
    searchLines ((i, line) : rest)
        | (not . null) matches' =
            SearchResult{lineContent = line, lineNumber = i, matches = matches'} : searchLines rest
        | otherwise = searchLines rest
      where
        matches' = searchSubString line query

--- Try to find a substring and return the start and end indexes.
searchSubString :: String -> QueryString -> [Slice]
searchSubString _ [] = []
searchSubString text query = go $ zip [0 ..] text
  where
    go [] = []
    go ((index, char) : chars)
        | char == head query
            && isMatch =
            Slice{startIndex = index, endIndex = index + queryLength - 1} : go chars
        | otherwise = go chars
      where
        isMatch = char : take (queryLength - 1) (map snd chars) == query
        queryLength = length query
