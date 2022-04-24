{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications  #-}

{-| This module contains utilities that correspond to TLA+ expressions

    Note that most TLA+ functionality is covered either by Haskell's standard
    library (for non-temporal expressions) and the "Temporal.Property" module
    (for temporal expression).  This module only adds a few missing utilities
    not covered by either of those two for parity with TLA+ and also to use
    names more familiar to TLA+ users.
-}
module HasCal.Expression
    ( -- * TLA+ expressions
      forall_
    , exists_
    , (==>)
    , (<=>)
    , boolean
    , (-->)
    , (|->)
    , domain
    , range
    , subset
    , choose

    -- * Universe
    , Universe(..)
    ) where

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)

import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List

{-| Verify that all elements satisfy the given predicate, like @\\A@ in TLA+

    `forall_` is like `all` but with the arguments `flip`ped.
-}
forall_ :: Foldable list => list a -> (a -> Bool) -> Bool
forall_ = flip all

{-| Verify that any element satisfies the given predicate, like @\\E@ in TLA+

    `forall_` is  like `any` but with the arguments `flip`ped
-}
exists_ :: Foldable list => list a -> (a -> Bool) -> Bool
exists_ = flip any

{-| Logical implication, like @=>@ in TLA+

    @p `==>` q@ is the same as \"if @p@ then @q@\"
-}
(==>) :: Bool -> Bool -> Bool
p ==> q = not p || q

{-| Bidirectional logical implication, like @<=>@ in TLA+

    @p `<=>` q@ is the same as \"if and only if @p@ then @q@\"
-}
(<=>) :: Bool -> Bool -> Bool
p <=> q = (p ==> q) && (q ==> p)

infixr 1 ==>, <=>

{-| All possible boolean values, like the @BOOLEAN@ set in TLA+

@
`boolean` = `universe` @`Bool`
@
-}
boolean :: [Bool]
boolean = universe @Bool

-- | A function set, like the @->@ operator in TLA+
(-->)
    :: (Traversable domain, Applicative range, Eq key, Hashable key)
    => domain key -> range value -> range (HashMap key value)
keys --> values =
    fmap (HashMap.fromList . Foldable.toList) (traverse process keys)
  where
    process key = fmap ((,) key) values

-- | A function set, like the @|->@ operator in TLA+
(|->)
    :: (Foldable list, Functor list, Eq key, Hashable key)
    => list key -> (key -> value) -> HashMap key value
keys |-> function = HashMap.fromList (Foldable.toList (fmap adapt keys))
  where
    adapt key = (key, function key)

{-| The domain of a function set, like the @DOMAIN@ function in TLA+

    `domain` is a synonym for `HashMap.keys`.
-}
domain :: HashMap key value -> [key]
domain = HashMap.keys

{-| The range of a function set, like the @RANGE@ function that projects
    commonly define

    `range` is a synonym for `HashMap.elems`.
-}
range :: HashMap key value -> [value]
range = HashMap.elems

-- | The powerset of a list, like the @SUBSET@ function in TLA+
subset :: [a] -> [[a]]
subset = Monad.filterM (\_ -> [False, True])

{-| Find the first matching element, like the @CHOOSE@ function in TLA+ except
    that this will return a `Nothing` instead of throwing an exception

    `choose` is like `List.find`, but with the arguments `flip`ped.
-}
choose :: Foldable list => list a -> (a -> Bool) -> Maybe a
choose = flip List.find

{-| A type whose values can be enumerated

    Note that `universe` should be the same thing as
    @[ `minBound` .. `maxBound` ]@ for a type that implements `Bounded` and
    `Enum`, but sometimes it's easier or more efficient to define instances of
    this class directly

    For most types, the easiest way to implement `Universe` is to
    @derive (`Bounded`, `Enum`, `Universe`)@ if you enable the @DeriveAnyClass@
    extension
-}
class Universe a where
    universe :: [a]
    default universe :: (Bounded a, Enum a) => [a]
    universe = [ minBound .. maxBound ]

instance Universe () where
    universe = [()]

instance Universe Bool where
    universe = [ False, True ]
