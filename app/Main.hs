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
            let result = map (search config'.pattern . fileContents) files
            case head result of
                Just r -> do
                    putStrLn $ formatSearchResult r
                Nothing -> putStrLn "Nothing found"
        Nothing -> do
            putStrLn "Config could not be made"
            putStrLn Constants.helpMsg

type FileContents = String

data FileBundle = FileBundle {filepath :: FilePath, fileContents :: FileContents}

formatSearchResult :: SearchResult -> String
formatSearchResult result =
    show (result.lineNumber + 1) ++ "," ++ show (result.match.startIndex + 1) ++ " :: " ++ formattedLine
  where
    formattedLine =
        let start = result.match.startIndex
            end = result.match.endIndex
            beforeMatch = take start result.lineContent
            matchedWord = take (end - start + 1) $ drop start result.lineContent
            afterMatch = drop (end + 1) result.lineContent
         in beforeMatch ++ Ansi.red matchedWord ++ afterMatch

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

data SearchResult = SearchResult {lineContent :: String, lineNumber :: Int, match :: Slice}
data Slice = Slice {startIndex :: Int, endIndex :: Int}

type QueryString = String

search :: QueryString -> FileContents -> Maybe SearchResult
search query fileContents' = searchLines $ zip [0 ..] $ lines fileContents'
  where
    searchLines [] = Nothing
    searchLines ((i, line) : rest) =
        case searchSubString line query of
            Just slice -> Just SearchResult{lineContent = line, lineNumber = i, match = slice}
            Nothing -> searchLines rest

--- Try to find a substring and return the start and end indexes.
searchSubString :: String -> QueryString -> Maybe Slice
searchSubString _ [] = Nothing
searchSubString text query = go $ zip [0 ..] text
  where
    go [] = Nothing
    go ((index, char) : chars)
        | char == head query
            && char : take (queryLength - 1) (map snd chars) == query =
            Just (Slice{startIndex = index, endIndex = index + queryLength - 1})
        | otherwise = go chars
      where
        queryLength = length query
