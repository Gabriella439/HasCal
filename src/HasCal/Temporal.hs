{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}

module HasCal.Temporal
    ( Property
    , Check(..)
    , eventually
    , always
    , infer
    , check
    , checkList
    ) where

import Control.Applicative (liftA2)
import Control.Arrow (Arrow(..))
import Control.Category (Category(..))
import Control.Monad.Trans.State (State, StateT(..))
import Data.Hashable (Hashable(..))
import Data.Profunctor (Profunctor(..))
import GHC.Generics (Generic)
import Prelude hiding (id, (.))

import qualified Control.Monad             as Monad
import qualified Control.Monad.Trans.State as State
import qualified Data.HashSet              as HashSet
import qualified Data.HashMap.Strict       as HashMap

{-| A temporal `Property` is a stateful transformation from an @input@ to an
    @output@

    The type variables are:

    * @input@: The type of the input to the property
    * @output@: The type of the output that changes in response to the input

    You can create a `Property` using:

    * `eventually` - A `Property` that @output@s `True` if the @input@ will
       `eventually` be `True` (either now or in the future)
    * `always` - A `Property` that @output@s `True` if the @input@ will `always`
      be `True` (both now and in the future)
    * `id` - The trivial `Property` whose @output@ always matches the @input@
    * `pure` - A `Property` with a constant @output@ that ignores the @input@
    * `mempty` - A `Property` with a constant empty @output@
    * A numeric literal - A `Property` with a constant numeric @output@

    You can combine one or more `Property`s using:

    * (`.`) - You can compose two `Property`s end-to-end, feeding the @output@
      of one property as the @input@ to another `Property`
    * `Applicative` operations (or `do` notation if you enable `ApplicativeDo`)
      - this shares their @input@ and combines their @output@s pointwise
    * (`<>`) - This combines their @output@s pointwise using (`<>`)
    * Numeric operators - These operators combine their @output@s pointwise

    You can transform a `Property` using:

    * `fmap` - Transform the @output@
    * `lmap` - Transform the @input@

    You can consume a `Property` using:

    * `infer` -  Convert a `Property` to the equivalent transformation on
      lists that infers the outputs from the inputs
    * `check` - Convert a `Property` to a `Check` for checking that a
      sequence of outputs is consistent with the inputs
-}
data Property input output =
        forall state . (Eq state, Hashable state, Universe state)
    =>  Property state (input -> State state output)

instance Functor (Property a) where
    fmap f (Property s k) = Property s (fmap f . k)

instance Applicative (Property a) where
    pure b = Property () (\_ -> pure b)

    Property sL kL <*> Property sR kR = Property s k
      where
        s = Pair sL sR

        k a = State.state step
          where
            step (Pair l r) = (f x, Pair l' r')
              where
                (f, l') = State.runState (kL a) l
                (x, r') = State.runState (kR a) r

instance Semigroup b => Semigroup (Property a b) where
    (<>) = liftA2 (<>)

instance Monoid b => Monoid (Property a b) where
    mempty = pure mempty

instance Num b => Num (Property a b) where
    fromInteger = pure . fromInteger

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance Fractional b => Fractional (Property a b) where
    fromRational = pure . fromRational

    recip = fmap recip

    (/) = liftA2 (/)

instance Floating b => Floating (Property a b) where
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

instance Category Property where
    id = Property () pure

    Property sL kL . Property sR kR = Property s k
      where
        s = Pair sL sR

        k a = State.state step
          where
            step (Pair l r) = (c, Pair l' r')
              where
                (b, r') = State.runState (kR a) r
                (c, l') = State.runState (kL b) l

instance Arrow Property where
    arr f = fmap f id

    first (Property state step) = Property state step'
      where
        step' (a, b) =
            State.state (\s -> first (,b) (State.runState (step a) s))

    second (Property state step) = Property state step'
      where
        step' (a, b) =
            State.state (\s -> first (a,) (State.runState (step b) s))

instance Profunctor Property where
    lmap f (Property s k) = Property s (k . f)

    rmap = fmap

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

data Check a b =
        forall s . (Eq s, Hashable s, Universe s)
    =>  Check s (a -> StateT s [] b)

instance Functor (Check a) where
    fmap f (Check s k) = Check s (fmap f . k)

instance Applicative (Check a) where
    pure b = Check () (\_ -> pure b)

    Check sL kL <*> Check sR kR = Check s k
      where
        s = Pair sL sR

        k a = StateT step
          where
            step (Pair l r) = do
                (f, l') <- State.runStateT (kL a) l
                (x, r') <- State.runStateT (kR a) r
                return (f x, Pair l' r')

instance Semigroup b => Semigroup (Check a b) where
    (<>) = liftA2 (<>)

instance Monoid b => Monoid (Check a b) where
    mempty = pure mempty

instance Num b => Num (Check a b) where
    fromInteger = pure . fromInteger

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance Fractional b => Fractional (Check a b) where
    fromRational = pure . fromRational

    recip = fmap recip

    (/) = liftA2 (/)

instance Floating b => Floating (Check a b) where
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

instance Category Check where
    id = Check () pure

    Check sL kL . Check sR kR = Check s k
      where
        s = Pair sL sR

        k a = StateT step
          where
            step (Pair l r) = do
                (b, r') <- State.runStateT (kR a) r
                (c, l') <- State.runStateT (kL b) l
                return (c, Pair l' r')

instance Profunctor Check where
    lmap f (Check s k) = Check s (k . f)

    rmap = fmap

{-| This property outputs `True` if the current input or any future input is
    `True`, and outputs `False` otherwise
-}
eventually :: Property Bool Bool
eventually = Property False (\l -> State.state (\r -> let b = l || r in (b, b)))

{-| This property outputs `False` if the current input or any future input is
    `False`, and outputs `True` otherwise
-}
always :: Property Bool Bool
always = Property True (\l -> State.state (\r -> let b = l && r in (b, b)))

{-| Convert a `Property` into the equivalent list transformation

    >>> infer (arr even) [ 2, 3, 5 ]
    [True,False,False]
    >>> infer (eventually . always) [ False, True ]
    [True,True]
    >>> infer (eventually . always) [ True, False ]
    [False,False]

    Note that `infer` has to `reverse` the list twice in order to infer the
    outputs from the inputs, so `infer` does not run in constant space.  Use
    `check` or `checkList` if you want something that processes the input in a
    single forward pass.
-}
infer :: Property input output -> [input] -> [output]
infer (Property s k) as =
    reverse (State.evalState (traverse k (reverse as)) s)

{-| Convert a `Property` into a `Check`

    See the documentation for `Check` for more details on how to use a
    `Check`
-}
check :: Property input output -> Check input output
check (Property s k) = Check s k'
  where
    -- TODO: Memoize this function
    relation a = HashMap.fromListWith (<>) (fmap adapt universe)
      where
        -- TODO: Deduplicate [(b, old)]
        adapt old = (new, [(b, old)])
          where
            (b, new) = State.runState (k a) old

    k' a = StateT (\new -> HashMap.findWithDefault [] new (relation a))

{-| This function checks that a list of @input@ and @output@ pairs is
    consistent with the given temporal `Property`

    >>> checkList (arr even) [(2, True), (3, False), (5, False)]
    True
    >>> checkList (arr even) [(2, True), (3, False), (5, True)]
    False
    >>> checkList (eventually . always) [(True, False), (False, False)]
    True
    >>> checkList (eventually . always) [(False, True), (True, True)]
    True

    You can think of `checkList` as having the following definition:

    > checkList property pairs =
    >     transform property (map fst pairs) == map snd pairs

    … except that `checkList` processes the list in a single forward pass
    (unlike `transform`)
-}
checkList :: Eq output => Property input output -> [(input, output)] -> Bool
checkList temporal =
    case check temporal of
        Check finalState k -> loop (HashSet.fromList universe)
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
