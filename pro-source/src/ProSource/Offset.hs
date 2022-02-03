{- |
Copyright: © 2019 James Alexander Feldman-Crough
License: MPL-2.0
-}
module ProSource.Offset (Offset (..)) where

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Data.Vector.Unboxed (MVector, Unbox, Vector)

-- | An offset into a 'Source', counted by UTF-8 codepoint.
newtype Offset = Offset Word
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable, NFData)
  deriving newtype Enum

instance Pretty Offset where
    pretty (Offset n) = "+" <> pretty n

newtype instance MVector s Offset = MV_Offset (MVector s Word)

instance VGM.MVector MVector Offset where
    basicLength (MV_Offset m) = VGM.basicLength m
    basicUnsafeSlice ix len (MV_Offset m) = MV_Offset $ VGM.basicUnsafeSlice ix len m
    basicOverlaps (MV_Offset x) (MV_Offset y) = VGM.basicOverlaps x y
    basicUnsafeNew len = MV_Offset <$> VGM.basicUnsafeNew len
    basicInitialize (MV_Offset v) = VGM.basicInitialize v
    basicUnsafeRead (MV_Offset v) = fmap Offset <$> VGM.basicUnsafeRead v
    basicUnsafeWrite (MV_Offset v) ix (Offset w) = VGM.basicUnsafeWrite v ix w

newtype instance Vector Offset = V_Offset (Vector Word)

instance VG.Vector Vector Offset where
    basicUnsafeFreeze (MV_Offset v) = V_Offset <$> VG.basicUnsafeFreeze v
    basicUnsafeThaw (V_Offset v) = MV_Offset <$> VG.basicUnsafeThaw v
    basicLength (V_Offset v) = VG.basicLength v
    basicUnsafeSlice ix len (V_Offset v) = V_Offset $ VG.basicUnsafeSlice ix len v
    basicUnsafeIndexM (V_Offset v) ix = Offset <$> VG.basicUnsafeIndexM v ix

instance Unbox Offset where
