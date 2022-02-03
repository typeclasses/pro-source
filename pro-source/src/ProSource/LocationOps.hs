{- |
Copyright: © 2019 James Alexander Feldman-Crough
License: MPL-2.0
-}
module ProSource.LocationOps (enrichLocation, stripLocation, sparse) where

import ProSource.Column
import ProSource.LineMap
import ProSource.Location
import ProSource.Offset
import ProSource.Source
import ProSource.SparseLocation

-- | Add lazily computed line and column number information to a 'SparseLocation'.
enrichLocation :: SparseLocation -> Location
enrichLocation sl = Location
    { locationSource = source
    , locationOffset = offset
    , locationLine   = line
    , locationColumn = column
    }
  where
    source                     = sparseLocationSource sl
    lineMap                    = sourceLineMap source
    offset@(~(Offset offsetN)) = sparseLocationOffset sl
    line                       = offsetToLine offset lineMap

    column = case lineToOffset line lineMap of
        Just (Offset n) -> Column (offsetN - n)
        Nothing         -> Column 0

-- | Remove line and column number information from a 'Location'.
stripLocation :: Location -> SparseLocation
stripLocation l = SparseLocation
  { sparseLocationSource = locationSource l
  , sparseLocationOffset = locationOffset l
  }

-- | An isomorphism between 'Location' and 'SparseLocation'. This is allowed because although a 'Location' has strictly more data than a 'SparseLocation', those values are denormalizations of values within 'SparseLocation'.
sparse :: Iso' Location SparseLocation
sparse = iso stripLocation enrichLocation
