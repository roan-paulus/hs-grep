{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import qualified Constants (helpMsg)
import Control.Exception (SomeException, try)
import qualified Format
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
                else mapM_ (putStrLn . Format.formatSearchResult) searchResults
        Nothing -> do
            putStrLn "Config could not be made"
            putStrLn Constants.helpMsg

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
