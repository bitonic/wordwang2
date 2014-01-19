module WordWang.Bwd (Bwd(..), null, fromList) where

import           Data.Foldable                         (Foldable)
import           Data.Monoid                           (Monoid(..))
import           Data.Traversable                      (Traversable)
import           Prelude                               hiding (null)

data Bwd a = B0 | !(Bwd a) :< !a
    deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance Monoid (Bwd a) where
    mempty = B0

    mappend xs B0        = xs
    mappend xs (ys :< y) = mappend xs ys :< y

null :: Bwd a -> Bool
null B0 = True
null _  = False

fromList :: [a] -> Bwd a
fromList = go B0
  where
    go !acc []       = acc
    go !acc (x : xs) = go (acc :< x) xs
