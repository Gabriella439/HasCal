{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

module HasCal.Temporal where

import Control.Applicative (liftA2)
import Control.Category (Category(..))
import Control.Monad.Trans.State (State, StateT(..))
import Data.Hashable (Hashable(..))
import Data.Profunctor (Profunctor(..))
import GHC.Generics (Generic)
import Prelude hiding ((.))

import qualified Control.Monad             as Monad
import qualified Control.Monad.Trans.State as State
import qualified Data.HashSet              as HashSet
import qualified Data.HashMap.Strict       as HashMap

data Temporal a b =
        forall s . (Eq s, Hashable s, Universe s)
    =>  Temporal s (a -> State s b)

instance Functor (Temporal a) where
    fmap f (Temporal s k) = Temporal s (fmap f . k)

instance Applicative (Temporal a) where
    pure b = Temporal () (\_ -> pure b)

    Temporal sL kL <*> Temporal sR kR = Temporal s k
      where
        s = Pair sL sR

        k a = State.state step
          where
            step (Pair l r) = (f x, Pair l' r')
              where
                (f, l') = State.runState (kL a) l
                (x, r') = State.runState (kR a) r

instance Profunctor Temporal where
    lmap f (Temporal s k) = Temporal s (k . f)

    rmap = fmap

instance Semigroup b => Semigroup (Temporal a b) where
    (<>) = liftA2 (<>)

instance Monoid b => Monoid (Temporal a b) where
    mempty = pure mempty

instance Num b => Num (Temporal a b) where
    fromInteger = pure . fromInteger

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance Fractional b => Fractional (Temporal a b) where
    fromRational = pure . fromRational

    recip = fmap recip

    (/) = liftA2 (/)

instance Floating b => Floating (Temporal a b) where
    pi = pure pi

    exp   = fmap exp
    sqrt  = fmap sqrt
    log   = fmap log
    sin   = fmap sin
    tan   = fmap tan
    cos   = fmap cos
    asin  = fmap asin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    tanh  = fmap tanh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acosh

    (**)    = liftA2 (**)
    logBase = liftA2 logBase

instance Category Temporal where
    id = Temporal () pure

    Temporal sL kL . Temporal sR kR = Temporal s k
      where
        s = Pair sL sR

        k a = State.state step
          where
            step (Pair l r) = (c, Pair l' r')
              where
                (b, r') = State.runState (kR a) r
                (c, l') = State.runState (kL b) l

data Pair a b = Pair !a !b
    deriving stock (Eq, Generic)
    deriving anyclass (Hashable)

class Universe a where
    universe :: [a]

instance Universe () where
    universe = [()]

instance (Universe a, Universe b) => Universe (Pair a b) where
    universe = liftA2 Pair universe universe

instance Universe Bool where
    universe = [ False, True ]

eventually :: Temporal Bool Bool
eventually = Temporal False (\l -> State.state (\r -> let b = l || r in (b, b)))

always :: Temporal Bool Bool
always = Temporal True (\l -> State.state (\r -> let b = l && r in (b, b)))

runList :: Temporal a b -> [a] -> [b]
runList (Temporal s k) as =
    reverse (State.evalState (traverse k (reverse as)) s)

data Validation a b =
        forall s . (Eq s, Hashable s, Universe s)
    =>  Validation s (a -> StateT s [] b)

run :: Temporal a b -> Validation a b
run (Temporal s k) = Validation s k'
  where
    -- TODO: Memoize this function
    relation a = HashMap.fromListWith (<>) (fmap adapt universe)
      where
        -- TODO: Deduplicate [(b, old)]
        adapt old = (new, [(b, old)])
          where
            (b, new) = State.runState (k a) old

    k' a = StateT (\new -> HashMap.findWithDefault [] new (relation a))

validate :: Temporal a Bool -> [(a, Bool)] -> Bool
validate temporal =
    case run temporal of
        Validation finalState k -> loop (HashSet.fromList universe)
          where
            loop states [] =
                HashSet.member finalState states
            loop states ((a, bool) : pairs)
                | HashSet.null states = False
                | otherwise           = loop newStates pairs
              where
                newStates = HashSet.fromList do
                    state <- HashSet.toList states

                    (b, newState) <- State.runStateT (k a) state

                    Monad.guard (b == bool)

                    return newState
