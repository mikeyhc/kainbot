module Kain.Travis where

import qualified Data.Map as M
import           Kain.Travis.Internal

newTravisState :: TravisState
newTravisState = TravisState M.empty
