{- |
Copyright: © 2019 James Alexander Feldman-Crough
License: MPL-2.0
-}
module ProSource.Column (Column (..)) where

-- | A column number.
newtype Column = Column Word
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable, NFData)
  deriving newtype Enum

instance Pretty Column where
    pretty (Column n) = pretty $ succ n
