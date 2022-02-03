{- |
Copyright: © 2019 James Alexander Feldman-Crough
License: MPL-2.0
-}
module ProSource.Line (Line (..)) where

{- | A line number.

The 'Show' instance for 'Line' counts from one, while the internal implementation counts from zero.
-}
newtype Line = Line Word
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable, NFData)
  deriving newtype Enum

instance Pretty Line where
    pretty (Line n) = pretty $ succ n
