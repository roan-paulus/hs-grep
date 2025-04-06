module Format (formatSearchResult) where

import Ansi qualified (red)
import Search

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
