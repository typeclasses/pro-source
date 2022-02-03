{-# LANGUAGE StrictData #-}

{- |
Copyright: © 2019 James Alexander Feldman-Crough
License: MPL-2.0
-}
module ProSource.Source (Source (..)) where

import ProSource.LineMap

{- | Information about a source file.

The 'Show' instance for ths class does not include the 'LineMap' or 'Text' fields, as those are rather noisy.
-}
data Source = Source
  { sourceName :: String
    -- ^ The reported file-name of the 'Source'.
    --
    -- When read from file handles, a non-filepath description such as @"\<stdin\>"@ is typically chosen. This field doesn't have semantic meaning, and should only be used to enrich the output displayed to users.
  , sourceText :: Text
    -- ^ The full source, as 'Text'.
  , sourceLineMap :: LineMap
    -- ^ A mapping of the start position of each line in the 'Source'.
  }
  deriving stock (Eq, Generic)
  deriving anyclass (Hashable, NFData)

instance Show Source where
    show (Source fp _ _) = "Source " <> show fp

instance Pretty Source where
    pretty = pretty . sourceName
