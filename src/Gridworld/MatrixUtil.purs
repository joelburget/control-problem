module Gridworld.MatrixUtil where

import Prelude
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Array (updateAt, length, (!!), zipWith, slice, range, concat)
import Data.Maybe
import Data.Matrix
import Data.TypeNat (class Sized, sized)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(Proxy))


newtype Mat rows cols a = Mat (Array a)


-- | Set an element of a matrix.
setElem :: forall rows cols a. (Sized rows, Sized cols) =>
           Int      -- ^ Row
        -> Int      -- ^ Column
        -> a
        -> Mat rows cols a     -- ^ Matrix
        -> Mat rows cols a
setElem i j a m@(Mat l) = Mat $ unsafePartial $ fromJust $
  let ix = i * sized (Proxy :: Proxy rows) + j
  in updateAt ix a l

-- | /O(1)/. Get an element of a matrix.
getElem :: forall rows cols a. (Sized rows, Sized cols) =>
           Int      -- ^ Row
        -> Int      -- ^ Column
        -> Mat rows cols a     -- ^ Matrix
        -> Maybe a
getElem i j m@(Mat l) = l !! (i * sized (Proxy :: Proxy rows) + j)

-- instance showMat2 :: (Show a) => Show (Mat Two a) where
--   show m = "Mat2x2 " <> show (columns m)

instance eqMat :: (Eq a) => Eq (Mat r c a) where
  eq (Mat l) (Mat r) = l == r

instance functorMat :: Functor (Mat r c) where
  map f (Mat l) = Mat (map f l)

-- instance applyMat :: Apply (Mat r c) where
--   apply (Mat f) (Mat a) = Mat (zipWith (\f' a' -> f' a') f a)

fromArray :: forall a rows cols. (Sized rows, Sized cols) => Array a -> Mat rows cols a
fromArray l =
  let rowSize = sized (Proxy :: Proxy rows)
      colSize = sized (Proxy :: Proxy cols)
  in case rowSize * colSize of
        i | i == length l -> Mat l
          | otherwise     -> unsafeThrow "Matrix>>fromArray: Wrong array length!"

toArray :: forall rows cols a. Mat rows cols a -> Array a
toArray (Mat a) = a
