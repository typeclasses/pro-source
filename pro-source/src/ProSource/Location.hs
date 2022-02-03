{-# LANGUAGE StrictData #-}

{- |
Copyright: © 2019 James Alexander Feldman-Crough
License: MPL-2.0
-}
module ProSource.Location (Location (..)) where

import ProSource.Source
import ProSource.Units

-- | A location in a 'Source', with the line and column number computed lazily.
data Location = Location
    { locationSource :: Source
      -- ^ The 'Source' this location references.
    , locationOffset :: Offset
      -- ^ The position in the 'Source', counted by Unicode codepoints.
    , locationLine   :: ~Line
      -- ^ The line number in the 'Source'.
    , locationColumn :: ~Column
      -- ^ The column number in the 'Source'.
    }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (NFData, Hashable)

instance Pretty Location where
    pretty loc = pretty (locationSource loc) <+> "@" <+> mconcat
        [pretty (locationLine loc), "×", pretty (locationColumn loc)]
