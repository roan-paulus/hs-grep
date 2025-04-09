module Format (formatSearchResult) where

import Ansi qualified (red)
import Search
import Parse (Options(..))

-- Take a search result and return a formatted version of the line with the match highlighted within it.
formatSearchResult :: Options -> SearchResult -> String
formatSearchResult options result
    | options.highlightMatches = start ++ colorMatches result
    | otherwise = start ++ result.lineContent
    where
        start = 
            result.path
            ++ ":"
            ++ show (result.lineNumber + 1)
            ++ ","
            ++ show (firstMatch.startIndex + 1)
            ++ " :: "
        firstMatch = head result.matches

colorMatches :: SearchResult -> String
colorMatches (SearchResult{lineContent, matches = []}) = lineContent
colorMatches (SearchResult{path, lineContent, lineNumber, matches = (match : remainingMatches)}) =
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
            ++ colorMatches
                SearchResult
                    { path
                    , lineContent = afterMatch
                    , lineNumber = lineNumber -- TODO: Useless data to keep around.
                    , matches = offsettedRemaningMatches
                    }
