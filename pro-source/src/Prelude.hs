{-# LANGUAGE NoImplicitPrelude #-}

module Prelude (module X) where

import BasePrelude as X (Enum, Eq, Functor (fmap), Maybe (..), Monoid, Ord (compare, (<), (>), (>=), (>=)), Ordering (..), Semigroup ((<>)), Show (..), String, Word, div, drop, flip, fromEnum, fromIntegral, id, maybe, mconcat, otherwise, pred, succ, ($), (&&), (+), (-), (.), (/=), (<$>), (==), (||))
import Control.DeepSeq as X (NFData)
import Control.Monad as X (guard)
import Data.Foldable as X (Foldable (toList))
import Data.Hashable as X (Hashable (..))
import Data.List as X (sort)
import Data.Text as X (Text)
import GHC.Generics as X (Generic)
import Optics.Core as X (AffineFold, AffineTraversal', Iso', castOptic, iso, lens, simple, to, (%))
import Prettyprinter as X (Pretty (..), (<+>))

import Text as X
