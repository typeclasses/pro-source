{-# language NoImplicitPrelude #-}

module Text where

import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy

type LText = Text.Lazy.Text

toStrictText :: LText -> Text
toStrictText = Text.Lazy.toStrict
