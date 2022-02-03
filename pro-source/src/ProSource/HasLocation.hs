{- |
Copyright: © 2020 James Alexander Feldman-Crough
License: MPL-2.0
-}
module ProSource.HasLocation
    ( -- * Classy optics; implementable on all types with a location
      HasLocation(..), offset
      -- ** Read-only optics
    , column, line, source
    ) where

import ProSource.Location
import ProSource.Units
import ProSource.SparseLocation
import ProSource.Source
import ProSource.LocationOps

-- | A classy optic for selecting the 'Location' from a value. Note that 'location' is affine: a 'Location' can't be attached to a value which does not -- already have one, and not all values with an instance of 'HasLocation' have a location.
class HasLocation t where
    location :: AffineTraversal' t Location

instance HasLocation Location where
    location = castOptic simple

-- | Focus on the 'Offset' from a value parsed from a source file. If the 'Offset' is modified, note that the resulting 'column' and 'line' will /also/ be modified as they are denormalizations of this value.
offset :: HasLocation l => AffineTraversal' l Offset
offset = location % sparse % lens
    sparseLocationOffset
    (\sl x -> sl { sparseLocationOffset = x })

-- | Fetch the 'Column' from a value parsed from a source file. Modifications are not allowed as the 'offset' and 'line' may become inconsistent.
column :: HasLocation l => AffineFold l Column
column = location % to locationColumn

-- | Fetch the 'Line' from a value parsed from a source file. Modifications are not allowed as the 'offset' and 'column' may become inconsistent.
line :: HasLocation l => AffineFold l Line
line = location % to locationLine

-- | Fetch the 'Source' a value was parsed from. Modifications are not allowed as the 'line', 'offset', and 'column' may become inconsistent.
source :: HasLocation l => AffineFold l Source
source = location % to locationSource
