module Random where

import Control.Monad
import System.Random
import System.Random.Mersenne.Pure64

data R a = R !a {-# UNPACK #-}!PureMT

newtype Rand a = Rand { runRand :: PureMT -> R a }

instance Monad Rand where
  {-# INLINE return #-}
  return a = Rand $ \s -> R a s

  {-# INLINE (>>=) #-}
  m >>= k = Rand  $ \s -> case runRand m s of
    R a s' -> runRand (k a) s'

  {-# INLINE (>>) #-}
  m >> k = Rand $ \s -> case runRand m s of
    R _ s' -> runRand k s'

instance Applicative Rand where
  pure = return
  (<*>) = ap

instance Functor Rand where
  fmap = liftM

runRandom :: Rand a -> PureMT -> (a, PureMT)
runRandom r g = case runRand r g of R x g' -> (x, g')

evalRandom :: Rand a -> PureMT -> a
evalRandom r g = case runRand r g of R x _ -> x

getDouble :: Rand Double
getDouble = Rand $ \s -> case randomDouble s of (w, s') -> R w s'

getDoubles :: Int -> Rand [(Double, Double)]
getDoubles n = getDoubles' n []
  where
    getDoubles' 0 acc = Rand $ \s -> case randomDouble s of (_, s') -> R acc s'
    getDoubles' left acc = do
      a <- getDouble
      b <- getDouble
      getDoubles' (left - 1) ((a, b) : acc)
