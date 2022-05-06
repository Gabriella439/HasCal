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
    , Boolean(..)
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

{- $setup

   >>> import Data.Functor.Identity (Identity(..))
   >>> import Data.List.NonEmpty (NonEmpty(..))
-}

{-| Verify that all elements satisfy the given predicate, like @\\A@ in TLA+

    `forall_` is like `all` but with the arguments `flip`ped.

    For example, the following TLA+ code:

    > \A i \in 1 .. 3 : 0 < i

    … would correspond to this Haskell code:

    >>> forall_ [ 1 .. 3 ] (\i -> 0 < i)
    True

    `forall_` obeys the following laws:

@
`forall_` [] f = `True`

`forall_` (xs `++` ys) f = `forall_` xs f `&&` `forall_` ys f

`forall_` [ x ] f = f x
@
-}
forall_ :: Foldable list => list a -> (a -> Bool) -> Bool
forall_ = flip all

{-| Verify that any element satisfies the given predicate, like @\\E@ in TLA+

    `exists_` is  like `any` but with the arguments `flip`ped

    For example, the following TLA+ code:

    > \E i in 1 .. 3 : i = 3

    … would correspond to this Haskell code:

    >>> exists_ [ 1 .. 3 ] (\i -> i == 3)
    True

    `exists_` obeys the following laws:

@
`exists_` [] f = `False`

`exists_` (xs `++` ys) f = `exists_` xs f `||` exists_ ys f

`exists_` [ x ] f = f x
@
-}
exists_ :: Foldable list => list a -> (a -> Bool) -> Bool
exists_ = flip any


{-| A class for types that support boolean algebra

    Laws:

@
(x `/\` y) `/\` z = x `/\` (y `/\` z)

x `/\` `true` = x

`true` `/\` x = x

(x `\/` y) `\/` z = x `\/` (y `\/` z)

x `\/` `false` = x

`false` `\/` x = x

`false` `/\` x = `false`

x `/\` `false` = `false`

`true` `/\` x = `true`

x `/\` `true` = `true`
@
-}
class Boolean a where
    -- | Generalizes `&&`
    (/\) :: a -> a -> a

    -- | Generalizes `||`
    (\/) :: a -> a -> a

    -- | Generalizes `True`
    true :: a

    -- | Generalizes `False`
    false :: a

instance Boolean Bool where
    (/\) = (&&)

    (\/) = (||)

    true = True

    false = False

{-| Logical implication, like @=>@ in TLA+

    @p `==>` q@ is the same as \"if @p@ then @q@\"

@
p `==>` q = `not` p `||` q
@
-}
(==>) :: Bool -> Bool -> Bool
p ==> q = not p || q

{-| Bidirectional logical implication, like @<=>@ in TLA+

    @p `<=>` q@ is the same as \"if and only if @p@ then @q@\"

@
p `<=>` q = (p `==>` q) `&&` (q `==>` p)
@
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

{-| A function set, like the @->@ operator in TLA+

    >>> [ 1, 2 ] --> [ False, True ]
    [fromList [(1,False),(2,False)],fromList [(1,False),(2,True)],fromList [(1,True),(2,False)],fromList [(1,True),(2,True)]]

    This operator also supports ranges and domains other than lists.  For
    example, you can limit the domain or range to only one value by using
    `Data.Functor.Identity.Identity` instead of a list:

    >>> [ 1, 2 ] --> Identity True
    Identity (fromList [(1,True),(2,True)])

    >>> Identity 1 --> [ False, True ]
    [fromList [(1,False)],fromList [(1,True)]]

    >>> Identity 1 --> Identity True
    Identity (fromList [(1,True)])

    … and if the range has only one value then there will only be one
    \"function\" in the function set.

    This operator also works with `Maybe` for the domain or range:

    >>> [ 1, 2 ] --> Just True
    Just (fromList [(1,True),(2,True)])

    >>> [ 1, 2 ] --> Nothing
    Nothing

    >>>  Just 1 --> [ True, False ]
    [fromList [(1,True)],fromList [(1,False)]]

    >>> Nothing --> [ True, False ]
    [fromList []]

    … and also `Data.List.NonEmpty.NonEmpty` lists:

    >>> [ 1, 2 ] --> (False :| [ True ])
    fromList [(1,False),(2,False)] :| [fromList [(1,False),(2,True)],fromList [(1,True),(2,False)],fromList [(1,True),(2,True)]]

    >>> (1 :| [ 2 ]) --> [ False, True ]
    [fromList [(1,False),(2,False)],fromList [(1,False),(2,True)],fromList [(1,True),(2,False)],fromList [(1,True),(2,True)]]
-}
(-->)
    :: (Traversable domain, Applicative range, Eq key, Hashable key)
    => domain key
    -- ^ Domain
    -> range value
    -- ^ Range
    -> range (HashMap key value)
keys --> values =
    fmap (HashMap.fromList . Foldable.toList) (traverse process keys)
  where
    process key = fmap ((,) key) values

{-| A function set, like the @|->@ operator in TLA+

    For example, the following TLA+ code:

    > [ i \in 1 .. 3 |-> i + 1 ]

    … would correspond to this Haskell code:

    >>> [ 1 .. 3 ] |-> \i -> i + 1
    fromList [(1,2),(2,3),(3,4)]
-}
(|->)
    :: (Foldable list, Functor list, Eq key, Hashable key)
    => list key -> (key -> value) -> HashMap key value
keys |-> function = HashMap.fromList (Foldable.toList (fmap adapt keys))
  where
    adapt key = (key, function key)

{-| The domain of a function set, like the @DOMAIN@ function in TLA+

    `domain` is a synonym for @"Data.HashMap.Strict".`HashMap.keys`@.
-}
domain :: HashMap key value -> [key]
domain = HashMap.keys

{-| The range of a function set, like the @RANGE@ function that TLA+ projects
    commonly define

    `range` is a synonym for @"Data.HashMap.Strict".`HashMap.elems`@.
-}
range :: HashMap key value -> [value]
range = HashMap.elems

{-| The powerset of a list, like the @SUBSET@ function in TLA+

    >>> subset [ 1, 2, 3 ]
    [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
-}
subset :: [a] -> [[a]]
subset = Monad.filterM (\_ -> [False, True])

{-| Find the first matching element, like the @CHOOSE@ function in TLA+ except
    that this will return a `Nothing` instead of throwing an exception

    `choose` is like `List.find`, but with the arguments `flip`ped.

    For example, the following TLA+ code:

    > CHOOSE i \in 1 .. 3 : 1 < i

    … would correspond to this Haskell code:

    >>> choose [ 1 .. 3 ] (\i -> 1 < i )
    Just 2

    `choose` obeys the following laws:

@
`choose` `Control.Applicative.empty` f = `Control.Applicative.empty`

`choose` (xs `Control.Applicative.<|>` ys) f = `choose` xs f `Control.Applicative.<|>` `choose` ys f
@

    … or equivalently:

@
`choose` [] f = `Nothing`

`choose` (xs `++` ys) f = `choose` xs f `Control.Applicative.<|>` `choose` ys f
@

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
