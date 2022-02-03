{- |
Description: Utilities for tracking source locations.
Copyright: © 2020 James Alexander Feldman-Crough
License: MPL-2.0
-}
module ProSource
    (
    -- * Source
      Source(..)
    , makeSource, getSourceLine, getLocation

    -- * Location
    , Location (..), SparseLocation(..)
    , enrichLocation, stripLocation, sparse

    -- * Units
    , Offset(..), Line(..), Column(..)

    -- * Line map
    , LineMap, lineOffsets, lineToOffset, offsetToLine

    -- * Polymorphic Location optics
    , HasLocation(..), offset, column, line, source

    ) where

import ProSource.HasLocation
import ProSource.LineMap
import ProSource.Location
import ProSource.LocationOps
import ProSource.Source
import ProSource.SourceOps
import ProSource.SparseLocation
import ProSource.Units
