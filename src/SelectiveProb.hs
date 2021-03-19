{- |
Copyright: (c) 2020 Armando Santos
SPDX-License-Identifier: MIT
Maintainer: Armando Santos <armandoifsantos@gmail.com>

See README for more info
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module SelectiveProb where

import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Selective
import Data.Bifunctor
import Data.Bool
import Data.Foldable (toList)
import Data.Functor.Identity
import Data.IORef
import Data.List (group, maximumBy, sort)
import Data.Ord
import qualified Data.Vector as V
import Data.Sequence (Seq, singleton)
import GHC.Generics
import qualified System.Random.MWC.Probability as MWCP
import Language.Haskell.TH hiding (bindS)
import Language.Haskell.TH.Syntax

data BlockedRequest = forall a. BlockedRequest (Request a) (IORef (Status a))

data Status a = NotFetched | Fetched a

type Prob = Double

data Request a where
  Uniform     :: Lift x => [x] -> Code Q (x -> a) -> Request a
  Categorical :: Lift x => [(x, Prob)] -> Code Q (x -> a) -> Request a
  Normal      :: Double -> Double -> Code Q (Double -> a) -> Request a
  Beta        :: Double -> Double -> Code Q (Double -> a) -> Request a
  Gamma       :: Double -> Double -> Code Q (Double -> a) -> Request a

{-
instance Show a => Show (Request a) where
  show (Uniform l f)     = "Uniform " ++ show (map f l)
  show (Categorical l f) = "Categorical " ++ show (map (first f) l)
  show (Normal x y _)    = "Normal " ++ show x ++ " " ++ show y
  show (Beta x y _)      = "Beta " ++ show x ++ " " ++ show y
  show (Gamma x y _)     = "Gamma " ++ show x ++ " " ++ show y
  -}

-- A Haxl computation is either completed (Done) or Blocked on pending data requests
data Result a = Done a | Blocked (Seq BlockedRequest) (Fetch a) deriving Functor

newtype Fetch a = Fetch {unFetch :: IO (Result a)} deriving Functor
{-

instance Applicative Fetch where
  pure = return

  Fetch iof <*> Fetch iox = Fetch $ do
    rf <- iof
    rx <- iox
    return $ case (rf, rx) of
      (Done f, _)                  -> f <$> rx
      (_, Done x)                  -> ($ x) <$> rf
      (Blocked bf f, Blocked bx x) -> Blocked (bf <> bx) (f <*> x) -- parallelism

instance Selective Fetch where
  select (Fetch iox) (Fetch iof) = Fetch $ do
    rx <- iox
    rf <- iof
    return $ case (rx, rf) of
      (Done (Right b), _)          -> Done b -- abandon the second computation
      (Done (Left a), _)           -> ($ a) <$> rf
      (_, Done f)                  -> either f id <$> rx
      (Blocked bx x, Blocked bf f) -> Blocked (bx <> bf) (select x f) -- speculative execution

  return = Fetch . return . Done

requestSample :: Request a -> Fetch a
requestSample request = Fetch $ do
  box <- newIORef NotFetched
  let br   = BlockedRequest request box
      cont = Fetch $ do
        Fetched a <- readIORef box
        return (Done a)
  return (Blocked (singleton br) cont)

fetch :: [BlockedRequest] -> IO ()
fetch = mapConcurrently_ aux
  where
    aux (BlockedRequest r ref) = do
        threadDelay 100
        c <- MWCP.createSystemRandom
        case r of
          Uniform l f -> do
            i <- MWCP.sample (MWCP.uniformR (0, length l - 1)) c
            writeIORef ref (Fetched . f $ l !! i)
          Categorical l f -> do
            i <- MWCP.sample (MWCP.categorical (V.fromList . map snd $ l)) c
            writeIORef ref (Fetched . f . fst $ l !! i)
          Normal x y f -> do
            a <- MWCP.sample (MWCP.normal x y) c
            writeIORef ref (Fetched . f $ a)
          Beta x y f -> do
            a <- MWCP.sample (MWCP.beta x y) c
            writeIORef ref (Fetched . f $ a)
          Gamma x y f -> do
            a <- MWCP.sample (MWCP.gamma x y) c
            writeIORef ref (Fetched . f $ a)

runFetch :: Fetch a -> IO a
runFetch (Fetch h) = do
  r <- h
  case r of
    Done a -> return a
    Blocked br cont -> do
      fetch (toList br)
      runFetch cont
      -}

-- Probabilistic eDSL

newtype CodeF f a = CodeF { runCodeF :: (Code Q (f a)) }

instance Monad f => Selective (CodeF f) where
  select cond k = CodeF [|| do c <- $$(runCodeF cond)
                               case c of
                                 Left a -> $$(runCodeF k) <*> pure a
                                 Right b -> return b
                        ||]

instance Monad f => CApplicative (CodeF f) where
  cpure a = CodeF [|| pure $$a ||]
  f <!*> a = CodeF [|| $$(runCodeF f) <*> $$(runCodeF a) ||]

instance Monad f => CFunctor (CodeF f) where
  cfmap f m = CodeF [|| $$f <$> $$(runCodeF m) ||]


instance Selective (Code Q) where
  select cond k = [|| case $$(cond) of
                        Left a -> $$(k) a
                        Right b -> b ||]

instance CApplicative (Code Q) where
   cpure x = x
   f <!*> a = [|| $$f $$a ||]

instance CFunctor (Code Q) where
  cfmap f m = [|| $$f $$m ||]


type Dist a = Select Request a

uniform :: Lift a => [a] -> Dist a
uniform = liftSelect . flip Uniform ([|| id ||])

categorical :: Lift a => [(a, Double)] -> Dist a
categorical = liftSelect . flip Categorical ([|| id ||])

normal :: Double -> Double -> Dist Double
normal x y = liftSelect (Normal x y ([|| id ||]))

bernoulli :: Double -> Dist Bool
bernoulli 0.5 = uniform [False, True]
bernoulli x = categorical ([(True, x), (False, 1 - x)])





binomial :: Int -> Double -> Dist Int
binomial n p = [|| length . filter id ||]  <!$> csequenceA (replicate n (bernoulli p))

csequenceA :: CApplicative f => [f a] -> f [a]
csequenceA [] = cpure [|| [] ||]
csequenceA (x:xs) = ([|| (:) ||]) <!$> x <!*> csequenceA xs

-- What I want to generate here
{-

replicateC = loop k
  loop 0 = []
  loop n = f a <*> loop (n - 1)

-}


beta :: Double -> Double -> Dist Double
beta x y = liftSelect (Beta x y ([|| id ||]))

gamma :: Double -> Double -> Dist Double
gamma x y = liftSelect (Gamma x y ([|| id ||]))

condition :: Code Q (a -> Bool) -> Dist a -> Dist (Maybe a)
condition c = condS (cpure c) (cpure [|| (const Nothing) ||]) (cpure [|| Just ||])

-- Examples of Probabilistic Programs

ex1a :: Dist (Bool, Bool)
ex1a =
  let c1 = bernoulli 0.5
      c2 = bernoulli 0.5
   in [|| (,) ||] <!$> c1 <!*> c2

ex1b :: Dist (Maybe (Bool, Bool))
ex1b =
  let c1 = bernoulli 0.5
      c2 = bernoulli 0.5
      result = [|| (,) ||] <!$> c1 <!*> c2
   in condition ([|| (uncurry (||)) ||]) result


ex2 :: Dist Int
ex2 =
  let count = cpure  [|| 0 ||]
      c1 = bernoulli 0.5
      c2 = bernoulli 0.5
      cond = condition ([|| (uncurry (||)) ||]) ([|| (,) ||] <!$> c1 <!*> c2)
      count2 = ifS ([|| maybe False fst ||] <!$> cond) count ([|| (+ 1) ||] <!$> count)
      count3 = ifS ([|| maybe False snd ||] <!$> cond) count2 ([|| (+ 1) ||] <!$> count2)
   in count3

ex3 :: Dist Int
ex3 =
  let count = cpure [|| 0 ||]
      c1 = bernoulli 0.5
      c2 = bernoulli 0.5
      cond = [|| not . uncurry (||) ||] <!$> ([|| (,) ||] <!$> c1 <!*> c2)
      count2 = ifS c1 count ([|| (+ 1) ||] <!$> count)
      count3 = ifS c2 count2 ([|| (+ 1) ||] <!$> count2)
   in ifS cond count3 ([|| (+) ||]  <!$> count3 <!*> ex3)

ex4 :: Dist Bool
ex4 =
  let b = cpure [|| True ||]
      c = bernoulli 0.5
   in ifS ([|| not ||]  <!$> c) b ([|| not ||] <!$> ex4)

data Coin = Heads | Tails
  deriving (Show, Eq, Ord, Bounded, Enum, NFData, Generic)

-- Throw 2 coins
t2c :: Dist (Coin, Coin)
t2c =
  let c1 = [|| bool Heads Tails ||] <!$> bernoulli 0.5
      c2 = [|| bool Heads Tails ||] <!$> bernoulli 0.5
   in [|| (,) ||] <!$> c1 <!*> c2

-- Throw 2 coins with condition
t2c2 :: Dist (Maybe (Bool, Bool))
t2c2 =
  let c1 = bernoulli 0.5
      c2 = bernoulli 0.5
   in condition ([|| (uncurry (||)) ||]) ( [|| (,) ||] <!$> c1 <!*> c2)

-- | Throw coins until 'Heads' comes up
{-
-- Not sure this is a proper selective?
prog :: Dist [Coin]
prog =
  let toss = bernoulli 0.5
   in condS
        (cpure [|| (== Heads) ||])
        ([|| flip (:) ||] <!$> prog)
        (pure (: []))
        (bool Heads Tails <$> toss)
        -}

-- | bad toss
throw :: Int -> Dist [Bool]
throw 0 = cpure [|| [] ||]
throw n =
  let toss = bernoulli 0.5
   in ifS
        toss
        ([|| (:) ||] <!$> toss <!*> throw (n - 1))
        (cpure [|| [] ||] )

-- | This models a simple board game where, at each turn,
-- two dice are thrown and, if the value of the two dice is equal,
-- the face of the third dice is equal to the other dice,
-- otherwise the third die is thrown and one piece moves
-- the number of squares equal to the sum of all the dice.
diceThrow :: Dist Int
diceThrow =
  condS
    (cpure $ [|| uncurry (==) ||])
    ([|| (\c (a, b) -> a + b + c) ||] <!$> die) -- Speculative dice throw
    (cpure [|| (\(a, _) -> a + a + a) ||])
    ([|| (,) ||]  <!$> die <!*> die) -- Parallel dice throw

diceThrow2 :: Dist [Int]
diceThrow2 =
  condS
    (cpure $ [|| uncurry (==) ||])
    ([|| (\c (a, b) -> [a, b, c]) ||] <!$> die) -- Speculative dice throw
    (cpure ([|| (\(a, b) -> [a, b]) ||]))
    ([|| (,) ||] <!$> die <!*> die) -- Parallel dice throw

die :: Dist Int
die = uniform ([1 .. 6])

-- | Infering the weight of a coin.
--
-- The coin is fair with probability 0.8 and biased with probability 0.2.
weight :: Dist Prob
weight =
  ifS
    (bernoulli 0.8)
    (cpure [|| 0.5 ||])
    (beta 5 1)

-- Sampling/Inference Algorithms

sample :: Dist a -> Int -> Dist [a]
sample r n = csequenceA (replicate n r)

-- monte carlo sampling/inference
monteCarlo :: Ord a => Int -> Dist a -> Dist [(a, Double)]
monteCarlo n d =
  let r = sample d n
   in [|| map (\l -> (head l, fromIntegral (length l) / fromIntegral n)) . group . sort ||] <!$> r

-- Inefficient rejection sampling
rejection :: (Lift c, Lift b, Bounded c, Enum c, Eq c) => Code Q ([a] -> [b] -> Bool) -> [b] -> Dist c -> (c -> Dist a) -> Dist c
rejection predicate observed proposal model = loop
  where
    len = length observed
    loop =
      let parameters = proposal
          generated = sample (bindS parameters model) len
          cond = predicate <!$> generated <!*> cpure (liftTyped observed)
       in ifS
            cond
            parameters
            loop


unroll :: Int -> Code Q a -> (Int -> Code Q a) -> Code Q Int -> Code Q a
unroll 0 k gen i = [|| case $$i of
                        0 -> $$(gen 0)
                        _ -> $$k
                       ||]
unroll j k gen i = [|| if j == $$i then $$(gen j) else $$(unroll (j-1) k gen i) ||]


-- forward sampling
runToIO :: Dist a -> CodeF IO a
runToIO d = CodeF [|| do
    gen <- MWCP.createSystemRandom
    $$(runCodeF $ runSelect (interpret [|| gen ||]) d)
  ||]
  where
    interpret :: Code Q MWCP.GenIO -> Request a -> CodeF IO a
    interpret gen (Uniform l f) = CodeF [|| do
      i <- MWCP.sample (MWCP.uniformR (0 :: Int, $$(liftTyped $ length l) - 1)) $$(gen)
      return ($$(unroll (length l - 1) [|| error "bad" ||] (\i -> [|| $$f $$(liftTyped (l !! i)) ||]) [||i||])) ||]
    interpret gen (Categorical l f) = CodeF [|| do
      i <- MWCP.sample (MWCP.categorical (V.fromList $$(liftTyped $ map snd l))) $$(gen)
      return $$(unroll (length l - 1) [|| error "bad" ||] (\i -> [|| $$f $$(liftTyped (fst $ l !! i)) ||]) [|| i ||]) ||]
    interpret gen (Normal x y f) = CodeF [|| do
      $$f <$> MWCP.sample (MWCP.normal x y) $$(gen) ||]
    interpret gen (Beta x y f) = CodeF [|| do
      $$f <$> MWCP.sample (MWCP.beta x y) $$gen ||]
    interpret gen (Gamma x y f) = CodeF [|| do
      $$f <$> MWCP.sample (MWCP.gamma x y) $$gen ||]

--runToFetch :: Dist a -> Fetch a
--runToFetch = runSelect requestSample

--runToIO2 :: Dist a -> IO a
--runToIO2 = runFetch . runToFetch

distMean :: Dist a -> Code Q a
distMean = runSelect interpret
  where
    interpret (Uniform l f) = let meanIndex = (length l - 1) `div` 2
                              in [|| $$f $$(liftTyped $ l !! meanIndex) ||]
      where
    -- There's no sensible mean, so I just return the most probable value
    interpret (Categorical l f) =
      let maxi = snd $ maximumBy (comparing fst) (zip (map snd l) [0 ..])
      in [|| $$f $$(liftTyped $ fst $  l !! maxi) ||]
    interpret (Normal x _ f) = [|| $$f x ||]
    interpret (Beta x _ f) = [|| $$f x ||]
    interpret (Gamma x _ f) = [|| $$f x ||]

distStandardDeviation :: Dist a -> Code Q a
distStandardDeviation = runSelect interpret
  where
    interpret (Uniform l f) =
      let stdIndex = round . sqrt $ ((fromIntegral (length l) ^ 2) - 1) / 12
      in [|| $$f $$(liftTyped $ l !! stdIndex) ||]
    interpret (Categorical _ _) = error "No sensible value"
    interpret (Normal _ y f) = [|| $$f y ||]
    interpret (Beta _ y f) = [|| $$f y ||]
    interpret (Gamma _ y f) = [|| $$f y ||]

-- Selective Applicative Functor utilities

-- Guard function used in McCarthy's conditional

selector (b, x) = bool (Left x) (Right x) b
dup x = (x, x)

-- | It provides information about the outcome of testing @p@ on some input @a@,
-- encoded in terms of the coproduct injections without losing the input
-- @a@ itself.
grdS :: CApplicative f => f (a -> Bool) -> f a -> f (Either a a)
grdS f a = [|| selector ||] <!$> applyF f ([|| dup ||] <!$> a)
  where
    applyF fab faa = [|| bimap ||] <!$> fab <!*> cpure ([|| id ||])  <!*> faa

-- | McCarthy's conditional, denoted p -> f,g is a well-known functional
-- combinator, which suggests that, to reason about conditionals, one may
-- seek help in the algebra of coproducts.
--
-- This combinator is very similar to the very nature of the 'select'
-- operator and benefits from a series of properties and laws.
condS :: Selective f => f (b -> Bool) -> f (b -> c) -> f (b -> c) -> f b -> f c
condS p f g = (\r -> branch r f g) . grdS p

