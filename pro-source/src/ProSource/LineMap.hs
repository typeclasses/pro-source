{- |
Description: Binary-search tree for finding the position of new lines.
Copyright: © 2019 James Alexander Feldman-Crough
License: MPL-2.0
-}
module ProSource.LineMap
    ( LineMap
    , lineOffsets
    , lineToOffset
    , offsetToLine
    , fromOffsets
    ) where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import ProSource.Units

-- | A dense vector containing offsets poiting to the start of each line. That is, the starting position of the third line of a file can be found at position 2.
newtype LineMap = LineMap (Vector Offset)
    deriving stock (Eq, Generic)
    deriving newtype (Show, NFData)

instance Hashable LineMap where
    hashWithSalt salt (LineMap v) = V.foldl' hashWithSalt salt v

fromOffsets :: Foldable f => f Offset -> LineMap
fromOffsets = LineMap . V.fromList . sort . toList

-- | Convert a 'LineMap' into a list of 'Offset's, corresponding to the first character of a line. Note that the initial offset is omitted-- the offset at index 0 will be the offset of the /second/ line.
lineOffsets :: LineMap -> [Offset]
lineOffsets (LineMap v) = V.toList v

-- | Fetch the 'Offset' for the given 'Line'. Evaluates to 'Nothing' if the given 'Line' does not appear in the LineMap
lineToOffset :: Line -> LineMap -> Maybe Offset
lineToOffset = \case
    Line 0 -> \_ -> Just $ Offset 0
    Line nth -> \(LineMap xs) -> xs V.!? fromIntegral (pred nth)

-- | Fetch the 'Line' number for a given 'Offset'. Newlines will be attributed the line that they terminate, rather than the line started immediately afterwards.
offsetToLine :: Offset -> LineMap -> Line
offsetToLine offset (LineMap xs) = Line $ fromIntegral $ go Nothing 0 (V.length xs)
  where
    go result min max | min >= max = maybe 0 succ result
    go result min max = case compare nthOffset offset of
        EQ -> succ nthIndex
        LT -> go (Just nthIndex) (nthIndex + 1) max
        GT -> go result min nthIndex
      where
        nthIndex  = ((max - min) `div` 2) + min
        nthOffset = xs V.! nthIndex
