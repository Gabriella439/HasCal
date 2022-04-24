{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}

{-| This module implements support for temporal logic

    Specifically, the module provides a temporal `Property` type and utilities
    for creating and checking `Property`s against sequences of inputs and
    outputs
-}

module HasCal.Property
    (
    -- * Property
      Property
    , eventually
    , always
    , (~>)
    , prime
    , following
    , infer

    -- * Check
    , Check(..)
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
import HasCal.Expression (Universe(..), (==>))
import Prelude hiding (id, (.))

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State as State
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

{- $setup

   >>> :m -Prelude
   >>> import Prelude hiding ((.), id)
   >>> import HasCal
-}

{-| A temporal `Property` is a stateful transformation from an @input@ to an
    @output@

    The type variables are:

    * @input@: The type of the input to the property
    * @output@: The type of the output that changes in response to the input

    You can create a `Property` using:

    * `arr` - Lift a pure function from an @input@ to an @output@ into the
      equivalent `Property`
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
    * `Applicative` operations (or @do@ notation if you enable @ApplicativeDo@)
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
{- Under the hood, a `Property` is essentially the exact same thing as the
   `Scan` type from the @foldl@ package, but with two differences:

   * The `Property` type is actually used to process the sequence of inputs
     in reverse, resulting in a sequence of outputs that are also reversed

   * We add more constraints to the existentially quantified @state@ type so
     that we can invert the scan when converting to the `Check` type
-}

instance Functor (Property a) where
    fmap f (Property s k) = Property s (fmap f . k)

instance Applicative (Property a) where
    pure b = Property () (\_ -> pure b)

    Property sL kL <*> Property sR kR = Property s k
      where
        s = Pair sL sR

        k a = State.state step
          where
            step !(Pair l r) = (f x, Pair l' r')
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
            step !(Pair l r) = (c, Pair l' r')
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

instance (Universe a, Universe b) => Universe (Pair a b) where
    universe = liftA2 Pair universe universe

{-| This property outputs `True` if the current input or any future input is
    `True`, and outputs `False` otherwise

    This is equivalent to the @<>@ temporal operator from TLA+

    >>> infer eventually [ False, True, False ]
    [True,True,False]
-}
eventually :: Property Bool Bool
eventually = Property False (\l -> State.state (\r -> let b = l || r in (b, b)))

{-| This property outputs `False` if the current input or any future input is
    `False`, and outputs `True` otherwise

    This is equivalent to the @[]@ temporal operator from TLA+

    >>> infer always [ True, False, True ]
    [False,False,True]
-}
always :: Property Bool Bool
always = Property True (\l -> State.state (\r -> let b = l && r in (b, b)))

{-| @f `~>` g@ returns `True` if every `True` output of @f@ is eventually
    followed by a `True` output from @g@

    > f ~> g = always . (liftA2 (==>) f (eventually . g))

    This is equivalent to the @~>@ temporal operator from TLA+

    >>> infer (arr even ~> arr odd) [ 1, 2, 3, 4 ]
    [False,False,False,False]
    >>> infer (arr even ~> arr odd) [ 0, 1, 2, 3 ]
    [True,True,True,True]
-}
(~>) :: Property a Bool -> Property a Bool -> Property a Bool
f ~> g = always . (liftA2 (==>) f (eventually . g))

{-| This property outputs each element with the following element (or `Nothing`
    if there is no following element)

    This is called \"prime\" as a reference to the TLA+ convention of referring
    to the next value of @x@ using @x'@ (i.e. \"@x@ prime\")

    >>> infer prime [ False, True, True ]
    [Just (False,True),Just (True,True),Nothing]
-}
prime :: (Eq a, Hashable a, Universe a) => Property a (Maybe (a, a))
prime = Property Zero step
  where
    step a0 = State.state f
      where
        f  Zero      = (Nothing, One a0)
        f (One a1  ) = (Just (a0, a1), Two a0 a1)
        f (Two a1 _) = (Just (a0, a1), Two a0 a1)

data Prime a = Zero | One !a | Two !a !a
    deriving (Eq, Generic, Hashable)

instance Universe a => Universe (Prime a) where
    universe = Zero : fmap One universe <> liftA2 Two universe universe

{-| This is a more ergonomic version of `prime` for the common case where you
    want to compare each temporal input against the next input using an
    equivalence relation

    >>> infer (following (/=)) [ False, False, True, False ]
    [False,True,True,True]
-}
following
    :: (Eq a, Hashable a, Universe a) => (a -> a -> Bool) -> Property a Bool
following (?) = arr adapt . prime
  where
    adapt  Nothing      = True
    adapt (Just (x, y)) = x ? y

{-| Convert a `Property` into the equivalent list transformation

    >>> infer (arr even) [ 2, 3, 5 ]
    [True,False,False]
    >>> infer eventually [ False, True, False ]
    [True,True,False]
    >>> infer always [ True, False, True ]
    [False,False,True]
    >>> infer (always . arr odd) [ 2, 3, 5 ]
    [False,True,True]
    >>> infer (eventually . arr even) [ 2, 3, 5 ]
    [True,False,False]
    >>> infer (eventually . always . arr odd) [ 2, 3, 5 ]
    [True,True,True]
    >>> infer (eventually . always . arr even) [ 2, 3, 5 ]
    [False,False,False]

    Note that `infer` has to `reverse` the list twice in order to infer the
    outputs from the inputs, so `infer` does not run in constant space.  Use
    `check` or `checkList` if you want something that processes the input in a
    single forward pass.
-}
infer :: Property input output -> [input] -> [output]
infer (Property s k) as =
    reverse (State.evalState (traverse k (reverse as)) s)

{-| A `Check` is like a temporal `Property` except that you can check a sequence
    of @input@s against a corresponding sequence of @output@s in a single
    forward pass in constant space.  This `Check` has the following two
    performance properties:

    * The `Check` can terminate early with a negative result if it determines
      that the temporal `Property` can no longer hold, regardless of future
      @input@ / @output@ values

    * The `Check` cannot terminate early if the @input@ / @output@ pairs satisfy
      the original temporal `Property`.  The entire sequence must be traversed
      in order to establish an affirmative result

    Unlike `Property`, a `Check` cannot infer the @output@s from the @input@s
    because doing so would require knowing in advance what the future @input@s
    would be, and that is incompatible with traversing the @input@s in a single
    forward pass.  For example, @`check` `always`@ cannot necessarily tell if
    the current @output@ should return `True` until it has seen all future
    @input@s, too.

    Other than the difference in algorithmic complexity, the `Check` type is
    similar to the `Property` type, meaning that they both share the same type
    parameters and the same instances.  However, you generally should prefer to
    use the instances for the `Property` type because those are more efficient.

    The main difference between `Property` and `Check`  is that the `Property`
    type is abstract, whereas the `Check` type is not  That means that you can
    pattern match on the `Check` type in order to obtain two values:

    * A stateful step function that you feed an inputs to get a list of
      acceptable outputs
    * An expected final state

    Study the source code for the `checkList` utility if you want to see an
    example for how you would use these two values to validate a list of
    @input@ / @output@ pairs against a temporal `Property`.
-}
data Check input output =
        forall state . (Eq state, Hashable state, Universe state)
    =>  Check
            state
            -- ^ Expected final state
            (input -> StateT state [] output)
            -- ^ Given an @input@ and an old @state@, return a list of possible
            --   new @(output, state)@ pairs

instance Functor (Check a) where
    fmap f (Check s k) = Check s (fmap f . k)

instance Applicative (Check a) where
    pure b = Check () (\_ -> pure b)

    Check sL kL <*> Check sR kR = Check s k
      where
        s = Pair sL sR

        k a = StateT step
          where
            step !(Pair l r) = do
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
            step !(Pair l r) = do
                (b, r') <- State.runStateT (kR a) r
                (c, l') <- State.runStateT (kL b) l
                return (c, Pair l' r')

instance Profunctor Check where
    lmap f (Check s k) = Check s (k . f)

    rmap = fmap

{-| Convert a `Property` into a `Check`

    See the documentation for `Check` for more details on how to use a
    `Check`

    This is mostly used to implement `HasCal.Coroutine.model`, but you
    can use this to implement your own efficient temporal property
    checker
-}
check :: Property input output -> Check input output
check (Property s k) = Check s k'
  where
    relation a = HashMap.fromListWith (<>) (fmap adapt universe)
      where
        adapt old = (new, [(b, old)])
          where
            (b, new) = State.runState (k a) old

    k' a = StateT (\new -> HashMap.findWithDefault [] new (relation a))

{-| This function checks that a list of @input@ and @output@ pairs is
    consistent with the given temporal `Property`

    You can think of `checkList` as having the following definition:

    > checkList property pairs = infer property inputs == outputs
    >   where
    >     (inputs, outputs) = unzip pairs

    … except that `checkList` processes the list in a single forward pass
    (unlike `infer`)

    >>> checkList eventually [(False, True), (True, True), (False, False)]
    True
-}
checkList :: Eq output => Property input output -> [(input, output)] -> Bool
checkList temporal =
    case check temporal of
        Check finalState k -> loop (HashSet.fromList universe)
          where
            loop states [] =
                HashSet.member finalState states
            loop states ((input, output) : pairs)
                | HashSet.null states = False
                | otherwise           = loop newStates pairs
              where
                newStates = HashSet.fromList do
                    state <- HashSet.toList states

                    (output', newState) <- State.runStateT (k input) state

                    Monad.guard (output == output')

                    return newState
