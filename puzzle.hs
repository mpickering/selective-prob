
{-# LANGUAGE RankNTypes, TemplateHaskell #-}
{-# LANGUAGE CPP, TupleSections, DeriveFunctor, GeneralizedNewtypeDeriving, FlexibleInstances, EmptyCase, BangPatterns #-}
module Puzzle where

import Language.Haskell.TH
import Data.Function


newtype Select f a = Select (forall g. Selective g => (forall x. f x -> g x) -> g a)

instance CFunctor (Select f) where
    cfmap f (Select x) = Select $ \k -> f `cfmap` x k

instance CApplicative (Select f) where
    cpure a                = Select $ \_ -> cpure a
    Select x <!*> Select y = Select $ \k -> x k <!*> y k

instance Selective (Select f) where
    select (Select x) (Select y) = Select $ \k -> x k <*? y k
    sfix f = _ -- Can this be defined?

runSelect :: Selective g => (forall x . f x -> g x) -> Select f a -> g a
runSelect k (Select f) = f k


-- This should terminate
test0 :: Code Q Int
test0 = sfix id

data Void a

void :: Void a -> b
void x = case x of

-- So should this
test1 :: Code Q Int
test1 = runSelect void (sfix id)

main = do
  let !x = test0
      !y = test1
  print "Complete"


instance Selective (Code Q) where
  select cond k = [|| case $$(cond) of
                        Left a -> $$(k) a
                        Right b -> b ||]
  -- Can be implemented for Code Q, like this
  sfix f = [|| fix $ (\a -> $$(f [|| a ||])) ||]

instance CApplicative (Code Q) where
   cpure x = x
   f <!*> a = [|| $$f $$a ||]

instance CFunctor (Code Q) where
  cfmap f m = [|| $$f $$m ||]

class CApplicative f => Selective f where
    select :: f (Either a b) -> f (a -> b) -> f b
    sfix :: (f a -> f a) -> f a

(<*?) :: Selective f => f (Either a b) -> f (a -> b) -> f b
(<*?) = select

class CFunctor f where
  cfmap :: Code Q (a -> b) -> f a -> f b

class CFunctor f => CApplicative f where
    cpure :: Code Q a -> f a
    (<!*>) :: f (a -> b) -> f a -> f b
