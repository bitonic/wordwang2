module WordWang.Bwd (Bwd(..)) where

import           Data.Foldable (Foldable)
import           Data.Traversable (Traversable)

data Bwd a = B0 | !(Bwd a) :< !a
    deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)
