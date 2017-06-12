module Sling.Prepush where

import           Filesystem.Path.CurrentOS (FilePath)
import           Prelude hiding (FilePath)

data PrepushLogs = PrepushLogs { prepushLogDir :: FilePath, prepushFullLogFilePath :: FilePath }
    deriving (Show)

