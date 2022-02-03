{-# LANGUAGE StrictData #-}

{- |
Copyright: © 2019 James Alexander Feldman-Crough
License: MPL-2.0
-}
module ProSource.SparseLocation (SparseLocation (..)) where

import ProSource.Offset
import ProSource.Source

-- | A location in a 'Source'. The line and column numbers of this type are not attached to this type; convert to a 'Location' to access those values.
data SparseLocation = SparseLocation
    { sparseLocationSource :: Source
      -- ^ The 'Source' this location references.
    , sparseLocationOffset :: Offset
      -- ^ The position in the 'Source', counted by Unicode codepoints.
    }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (NFData, Hashable)
