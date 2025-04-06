{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Search (SearchResult (..), Slice (..), search) where

import qualified Types (FileBundle (..))

data SearchResult = SearchResult {path :: String, lineContent :: String, lineNumber :: Int, matches :: [Slice]}
data Slice = Slice {startIndex :: Int, endIndex :: Int}

type QueryString = String

search :: QueryString -> Types.FileBundle -> [SearchResult]
search query fileBundle = searchLines $ zip [0 ..] $ lines fileBundle.contents
  where
    searchLines [] = []
    searchLines ((i, line) : rest)
        | (not . null) matches' =
            SearchResult{path = fileBundle.path, lineContent = line, lineNumber = i, matches = matches'} : searchLines rest
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
