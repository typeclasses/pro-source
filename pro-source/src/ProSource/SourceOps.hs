{- |
Copyright: © 2019 James Alexander Feldman-Crough
License: MPL-2.0
-}
module ProSource.SourceOps (makeSource, getLocation, getSourceLine) where

import qualified Data.Text as T

import ProSource.Line
import ProSource.LineMap
import ProSource.Location
import ProSource.LocationOps
import ProSource.Offset
import ProSource.Source
import ProSource.SparseLocation

-- | Create a 'Source' from a descriptive name and a body. The source name is typically a 'FilePath', but this is not guaranteed. For instance, when read from standard-input, a common choice is to name the source @\<stdin\>@.
makeSource :: String -> LText -> Source
makeSource name (toStrictText -> body) = Source name body lineMap
  where
    lineMap = case T.foldl' lineMapFold (1, '\0', []) $ body of
        (_, _, acc) -> fromOffsets acc
    lineMapFold (ix, prev, acc) ch
        | ch == '\n' && prev == '\r' = (succ ix, ch, Offset ix : drop 1 acc)
        | ch == '\n' || ch == '\r'   = (succ ix, ch, Offset ix : acc)
        | otherwise                  = (succ ix, ch, acc)

-- | Convert an 'Offset' into a 'Location'.
getLocation :: Offset -> Source -> Maybe Location
getLocation offset src = do
    guard $ T.length (sourceText src) > fromEnum offset
    Just . enrichLocation $ SparseLocation src offset

-- | Fetch a single line from a source.
getSourceLine :: Line -> Source -> Maybe Text
getSourceLine line source = do
    start <- fromEnum <$> lineToOffset line lineMap
    let end = fromEnum <$> lineToOffset (succ line) lineMap
    Just . maybe id (T.take . (flip (-) start)) end . T.drop start $ sourceText
        source
    where lineMap = sourceLineMap source
