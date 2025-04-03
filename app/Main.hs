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
                Just r -> putStrLn $ show (r.lineNumber + 1) ++ "," ++ show (r.columnNumber + 1) ++ " :: " ++ Ansi.red r.lineContent
                Nothing -> putStrLn "Nothing found"
        Nothing -> do
            putStrLn "Config could not be made"
            putStrLn Constants.helpMsg

type FileContents = String

data FileBundle = FileBundle {filepath :: FilePath, fileContents :: FileContents}

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

data SearchResult = SearchResult {lineContent :: String, lineNumber :: Int, columnNumber :: Int}

type QueryString = String

search :: QueryString -> FileContents -> Maybe SearchResult
search query fileContents' = searchLines $ zip [0 ..] $ lines fileContents'
  where
    searchLines [] = Nothing
    searchLines ((i, line) : rest) =
        case searchSubString line query of
            Just foundColumnNumber -> Just SearchResult{lineContent = line, lineNumber = i, columnNumber = foundColumnNumber}
            Nothing -> searchLines rest

searchSubString :: String -> QueryString -> Maybe Int
searchSubString _ [] = Nothing
searchSubString text query = go $ zip [0 ..] text
  where
    go [] = Nothing
    go ((index, char) : chars)
        | char == head query
            && char : take (length query - 1) (map snd chars) == query =
            Just index
        | otherwise = go chars
