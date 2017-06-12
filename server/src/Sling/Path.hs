module Sling.Path
    where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FP
import           Filesystem.Path.CurrentOS (FilePath)
import           Prelude hiding (FilePath)


encodeFP :: FilePath -> T.Text
encodeFP = T.pack . BS8.unpack . FP.encode
