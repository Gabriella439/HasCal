{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module HasCal
    ( -- * Types
      Process
    , Coroutine(..)
    , Status(..)
    , ModelException(..)

    -- * Model checking
    , check
    , debug

    -- * Lenses
    , global
    , local

    -- * PlusCal Statements
    , yield
    , skip
    , end
    , either
    , with
    , while
    , await
    , assert
    , print

    -- * TLA+ expressions
    , forall_
    , exists_
    , (==>)
    , (<=>)
    , boolean
    , (~>)
    , domain
    , range
    , choose

    -- * Classes
    , ToDocs(..)

    -- * Re-exports
    , module Lens.Micro.Platform
    , Hashable
    , MonadIO(..)
    , Pretty(..)
    , HashMap
    ) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Exception.Safe (Exception)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Strict (MonadState(..), StateT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Void (Void)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)
import Lens.Micro.Platform
import List.Transformer (ListT)
import Numeric.Natural (Natural)
import Prelude hiding (either, print)
import Prettyprinter (Doc, Pretty(..))

import qualified Control.Applicative as Applicative
import qualified Control.Exception.Safe as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Void as Void
import qualified List.Transformer as List
import qualified Prelude
import qualified Prettyprinter as Pretty
import qualified Prettyprinter.Render.String as Pretty.String
import qualified Prettyprinter.Render.Text as Pretty.Text
import qualified Text.Show as Show

-- TODO: Add highlighting to pretty-printed output
-- TODO: Associate local state with process names
-- TODO: Explicitly enumerate re-exports
-- TODO: Upstream orphan instance for `MonadThrow (ListT m)`

instance MonadThrow m => MonadThrow (ListT m) where
    throwM e = lift (throwM e)

{-| A `Process` represents a sequence of @PlusCal@ statements.  You can think of
    a `Process` as a non-deterministic finite automaton:

    * The `Process` transitions atomically between labeled states
    * The `Process` may explore multiple state transitions in parallel because
      is it non-deterministic

    The only caveat is that a `Process` does not include a starting state (which
    is only later specified upon conversion to a `Coroutine`).

    The type variables are:

    * @global@: The type of the global state shared by every `Process`
    * @local@: The type of the process-local state unique to this `Process`
    * @label@: The type of labels that this `Process` emits
    * @result@: The return value of the `Process`

    Processes support the following core functionality:

    * `yield` - Yield control alongside a label for the current state, ending
      an atomic transition
    * `pure` / `return` - Promote a value to a `Process` which does nothing
      and returns the value
    * `empty` - Terminate a `Process`
    * `liftIO` - Run an arbitrary `IO` action inside a `Process`
    * `throwM` - Throw an exception inside a `Process`, causing model checking
       to fail
    * `get` / `put` - Get and set the current `Process` state
    * `mempty` - A `Process` which does nothing

    Additionally, the utilities in the \"PlusCal utilities\" section wrap the
    above functionality to use more PlusCal-friendly names.

    You can combine one or more `Process`es using:

    * @do@ notation - Run `Process`es sequentially
    * (`<|>`) - Explore two `Process`es in parallel
    * (`<>`) - Run two `Process`es sequentially and combine their return values

    Finally, you will need to convert a `Process` into a `Coroutine` by wrapping
    the `Process` in the `Begin` constructor.  Note that the process needs to
    terminate (e.g. using `empty` / `end`) in order to become a `Coroutine`.
-}
newtype Process global local label result
    = Choice
        { possibilities
            :: StateT (Status global local) (ListT IO)
                (Step global local label result)
        }
    deriving stock (Functor)

instance Applicative (Process global local label) where
    pure result = Choice (pure (Done result))

    (<*>) = Monad.ap

instance Monad (Process global local label) where
    Choice ps >>= f = Choice do
        p <- ps
        case p of
            Yield label rest -> do
                return (Yield label (rest >>= f))
            Done result -> possibilities (f result)

instance Alternative (Process global local label) where
    empty = Choice empty

    Choice psL <|> Choice psR = Choice (psL <|> psR)

instance Semigroup result => Semigroup (Process global local label result) where
    (<>) = liftA2 (<>)

instance Monoid result => Monoid (Process global local label result) where
    mempty = pure mempty

instance MonadState (Status global local) (Process global local label) where
    get = Choice (fmap Done get)

    put s = Choice (fmap Done (put s))

    state k = Choice (fmap Done (state k))

instance MonadThrow (Process global local label) where
    throwM e = Choice (fmap Done (throwM e))

instance MonadIO (Process global local label) where
    liftIO io = Choice (fmap Done (liftIO io))

data Step global local label result
    = Yield label (Process global local label result)
    | Done result
    deriving stock (Functor)

{-| The `Status` type represents the state for every `Process`, which has two
    components:

    * @global@ - The global state shared by all `Process`es
    * @local@ - The local state unique to this `Process`
-}
data Status global local = Status
    { _global :: global
      -- ^ Shared global state
    , _local :: local
      -- ^ `Process`-local state
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (Hashable)

instance (Pretty global, ToDocs local) => Pretty (Status global local) where
    pretty Status{ _global, _local } = Pretty.group (Pretty.flatAlt long short)
      where
        short
            | null localDocs =
                pretty _global
            | otherwise =
                    "Global: "
                <>  pretty _global
                <>  ", Local: "
                <>  commas localDocs

        long
            | null localDocs =
                pretty _global
            | otherwise =
                Pretty.align
                    (   "Global:"
                    <>  Pretty.hardline
                    <>  "  "
                    <>  pretty _global
                    <>  Pretty.hardline
                    <>  "Local:"
                    <>  Pretty.hardline
                    <>  "  "
                    <>  bullets localDocs
                    )

        localDocs = toDocs _local

-- | `Lens'` for accessing the @global@ state of a `Process`
global :: Lens' (Status global local) global
global k (Status g l) = fmap (\g' -> Status g' l) (k g)

-- | `Lens'` for accessing the @local@ state of a `Process`
local :: Lens' (Status global local) local
local k (Status g l) = fmap (\l' -> Status g l') (k l)

{-| A `Coroutine` wraps a `Process` alongside a starting label and starting
    process-local state.  Including the starting state makes a `Coroutine` a
    complete non-deterministic finite automaton that can be combined with
    other `Coroutine`s using `Applicative` operations.  Combining `Coroutine`s
    in this way is the same as taking the Cartesian product of their
    equivalent non-deterministic finite automata.  For a more detailed
    explanation, see:

    * <https://www.haskellforall.com/2022/03/modeling-pluscal-in-haskell-using.html Modeling PlusCal in Haskell using Cartesian products of NFAs>

    The type variables are:

    * @global@: The type of the global state shared by every `Coroutine`
    * @label@: The type of labels that this `Coroutine` emits

    Carefully note that the `Process`-local state (i.e. the @local@ variable)
    is hidden upon conversion to a `Coroutine`, in order to ensure that the
    `Process`-local state of each `Coroutine` is isolated from one another.

    You can create a `Coroutine` in the following ways:

    * The `Begin` constructor - Wrap a `Process` with a starting state and label
      to create a `Coroutine`
    * `pure` - A `Coroutine` with a single state and no valid transitions
    * `mempty` - A `Coroutine` that does nothing

    You can combine `Coroutine`s using:

    * `Functor` and `Applicative` utilities (e.g. (`<$>`) and (`<*>`))
    * @do@ notation, if you enable @ApplicativeDo@
    * (`<>`) - Run two `Coroutine`s in parallel and combine their results
-}
data Coroutine global label =
        forall local
    .   (Eq local, Hashable local, ToDocs local, Show local)
    =>  Begin
            { startingLabel :: label
            , startingLocal :: local
            , process       :: Process global local label Void
            }

instance Functor (Coroutine global) where
    fmap = Applicative.liftA

instance Applicative (Coroutine global) where
    pure label = Begin label () empty

    Begin label0F sF fs0 <*> Begin label0X sX xs0 = Begin label0FX (sF, sX) fxs
      where
        (label0FX, fxs) = loop (label0F, fs0) (label0X, xs0)

        loop (label1F, Choice fs) (label1X, Choice xs) =
            (   label1F label1X
            ,       Choice (fmap adaptF (zoom onLeft fs))
                <|> Choice (fmap adaptX (zoom onRight xs))
            )
          where
            adaptF (Done result) = Done result
            adaptF (Yield labelF restF) = Yield labelFX restFX
              where
                (labelFX, restFX) = loop (labelF, restF) (label1X, Choice xs)

            adaptX (Done result) = Done result
            adaptX (Yield labelX restX) = Yield labelFX restFX
              where
                (labelFX, restFX) = loop (label1F, Choice fs) (labelX, restX)

        onLeft :: Lens' (Status global (l, r)) (Status global l)
        onLeft k (Status g (l, r)) = fmap adapt (k (Status g l))
          where
            adapt (Status g' l') = Status g' (l', r)

        onRight :: Lens' (Status global (l, r)) (Status global r)
        onRight k (Status g (l, r)) = fmap adapt (k (Status g r))
          where
            adapt (Status g' r') = Status g' (l, r')

instance Semigroup label => Semigroup (Coroutine global label) where
    (<>) = liftA2 (<>)

instance Monoid label => Monoid (Coroutine global label) where
    mempty = pure mempty

{-| End the current atomic transition alongside a label for the current state.
    If the exact same label and state have been reached before then the
    model checker will fail with a `NonTermination` error

    This potentially yields control to other `Process`es
-}
yield :: label -> Process global local label ()
yield label = Choice (pure (Yield label mempty))

{-| A `Process` which does nothing, like the @skip@ statement in PlusCal

    This is a synonym for `mempty`, but with a `Process`-specific type
    signature.

    Note that you do not need to use `skip` in Haskell as often as in PlusCal.
    For example, consider the following PlusCal code:

    > A:
    >   skip;
    > B:
    >   either
    >     skip;
    >   or
    >     C:
    >       skip;
    >   end either;

    The Haskell code can elide many of those `skip`s:

@
example = do
    `yield` "A"
    `yield` "B"
    `either`
        [ `skip`
        , `yield` "C"
        ]
@

    … because `skip` literally does nothing and therefore can be omitted in
    most cases.
-}
skip :: Process global local label ()
skip = mempty

{-| Terminate the current `Process`, which slightly resembles the @end process@
    keyword in PlusCal

    This is a synonym for `empty`, but with a `Process`-specific type signature.

    Note that this does not behave exactly the same as @end process@ in PlusCal.
    This is closer in spirit to @`await` `False`@, meaning that you can call
    `end` anywhere within a `Process` and that will stop execution for the
    current process branch.
-}
end :: Process global local label a
end = empty

{-| Non-deterministically simulate multiple subroutines, like an @either@
    statement in PlusCal

    This is a synonym for `Foldable.asum`, but with a `Process`-specific type
    signature.

    The model checker will explore all branches, succeeding only if all branches
    succeed.

@
`either` [] = `end`

`either` [ a ] = a

`either` [ a, b ] = a `<|>` b
@
-}
either
    :: Foldable list
    => list (Process global local label result)
    -- ^ Subroutines to non-deterministically select from
    -> Process global local label result
either = Foldable.asum

{-| Non-deterministically select from one of multiple possible values, like
    a @with@ statement in PlusCal

    @with@ is the same thing as using @either@ to select from one of multiple
    `pure` subroutines:

@
`with` results = `either` (`fmap` `pure` results)
@

@
`with` [] = `end`

`with` [ a ] = `pure` a

`with` (as `<|>` bs) = `with` as `<|>` `with` bs

`with` `empty` = `empty`
@
-}
with
    :: (Foldable list, Functor list)
    => list result -> Process global local label result
with results = either (fmap pure results)

{-| Run a loop so long as the loop condition does not return `True`, like a
    @while@ statement in PlusCal

@
`while` (`pure` `True`) body = `Monad.forever` body

`while` (`pure` `False`) body = skip
@
-}
while
    :: Process global local label Bool
    -- ^ Condition
    -> Process global local label ()
    -- ^ Body of the loop
    -> Process global local label ()
while condition body = do
    bool <- condition
    Monad.when bool do
        body
        while condition body

{-| Only permit the current state transition if the predicate is `True`, like
    an @await@ statement in PlusCal

    This is a synonym for `Monad.guard`, but with a `Process`-specific type
    signature.

@
`await` `False` = `end`
`await` `True`  = `skip`

`await` (a `||` b) = `await` a <|> `await` b
`await` (a `&&` b) = `await` a <> `await` b

`await` `False` = `empty`
`await` `True`  = `mempty`
@
-}
await :: Bool -> Process global local label ()
await = Monad.guard

{-| Throw an exception if the condition does not evaluate to `True`, like an
    @assert@ statement in PlusCal

    The model checker will fail if any branch throws an exception (with `assert`
    or otherwise), but exceptions thrown using `assert` will also automatically
    include the current `Process` state

@
`assert` `True` = `skip`
@
-}
assert
    :: (ToDocs local, Pretty global, Show local, Show global)
    => Bool
    -- ^ Condition
    -> Process global local label ()
assert True  = skip
assert False = do
    status <- get
    Exception.throw AssertionFailed{ status }

{-| Print a value to the console for debugging purposes, like a @print@
    statement in PlusCal

    This is the same as @"Prelude".`Prelude.print`@, except wrapped in a
    `liftIO`
-}
print :: Show a => a -> Process global local label ()
print a = liftIO (Prelude.print a)

{-| Verify that all elements satisfy the given predicate, like @\A@ in TLA+

    `forall_` is like `all` but with the arguments `flip`ped.
-}
forall_ :: Foldable list => list a -> (a -> Bool) -> Bool
forall_ = flip all

{-| Verify that any element satisfies the given predicate, like @\E@ in TLA+

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

-- | All possible boolean values, like the @BOOLEAN@ set in TLA+
boolean :: NonEmpty Bool
boolean = False :| [ True ]

-- | A function set, like @->@ in TLA+
(~>)
    :: (Eq key, Hashable key)
    => NonEmpty key -> NonEmpty value -> NonEmpty (HashMap key value)
keys ~> values =
    fmap (HashMap.fromList . Foldable.toList) (traverse process keys)
  where
    process key = fmap ((,) key) values

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

{-| Find the first matching element, like the @CHOOSE@ function in TLA+ except
    that this will return a `Nothing` instead of throwing an exception

    `choose` is like `List.find`, but with the arguments `flip`ped.
-}
choose :: Foldable list =>  list a -> (a -> Bool) -> Maybe a
choose = flip List.find

{-| The `ModelException` type represents all of the ways in which the model
    checker can fail
-}
data ModelException =
          forall label status
      .   ( Show label
          , Show status
          , Pretty label
          , Pretty status
          )
      =>  Nontermination { history :: [(label, status)] }
          -- ^ The process does not necessarily terminate because at least one
          --   branch of execution permits an infinite cycle
          --
          --   NOTE: The `history` field stores old states in reverse
          --   chronological order, for efficiency
    |     forall local . (Pretty local, Show local)
      =>  AssertionFailed { status :: local }
          -- The process failed to satisfy an `assert` statement

instance Show ModelException where
    showsPrec _ Nontermination{ history } =
          showString "Nontermination {history = "
        . Show.showListWith shows history
        . showString "}"
    showsPrec _ AssertionFailed{ status } =
          showString "AssertionFailed {status = "
        . shows status
        . showString "}"

instance Exception ModelException where
    displayException exception =
        Pretty.String.renderString
            (Pretty.layoutPretty Pretty.defaultLayoutOptions (pretty exception))

instance Pretty ModelException where
    pretty Nontermination{ history } = Pretty.group (Pretty.flatAlt long short)
      where
        short = "Non-termination" <> suffix
          where
            suffix = case history of
                [] ->
                    mempty

                seens  ->
                    " - History: " <> bars (map adapt (reverse seens))
                  where
                    adapt (label, status) =
                        commas [ pretty label, pretty status ]

        long = Pretty.align ("Non-termination" <> suffix)
          where
            suffix = case history of
                [] ->
                    mempty
                seens ->
                        Pretty.hardline
                    <>  Pretty.hardline
                    <>  "History:"
                    <>  Pretty.hardline
                    <>  foldMap outer (reverse seens)
                  where
                    outer (label, status) =
                            "- "
                        <>  Pretty.align
                            (   "Label: "
                            <>  pretty label
                            <>  Pretty.hardline
                            <>  "State: "
                            <>  pretty status
                            )
                        <>  Pretty.hardline

    pretty AssertionFailed{ status } =
        Pretty.align
            (   "Assertion failed"
            <>  Pretty.hardline
            <>  Pretty.hardline
            <>  "State: "
            <>  pretty status
            )

{-| Run the model checker on a `Coroutine` by supplying a `NonEmpty` list of
    starting states

    If you want to check more than one `Coroutine`, then combine those
    `Coroutine`s using `Applicative` operations or @ApplicativeDo@ notation
-}
check
    :: ( Eq global
       , Eq label
       , Hashable global
       , Hashable label
       , Pretty label
       , Pretty global
       , Show global
       , Show label
       )
    => Coroutine global label
    -- ^ `Coroutine` to check
    -> NonEmpty global
    -- ^ Starting global state
    -> IO ()
check Begin{ startingLabel, startingLocal, process } startingGlobals = do
    List.runListT (State.evalStateT action startingStatus)
  where
    action = loop [] startingSet do
        startingGlobal <- with (Foldable.toList startingGlobals)

        global .= startingGlobal

        process

    startingStatus =
        Status
            { -- The starting value of `_global` doesn't matter here since we
              -- will override at the beginning of @action@.  We could use
              -- @undefined@ here, but as a precaution we use the beginning of
              -- the `NonEmpty` list instead
              _global = NonEmpty.head startingGlobals
            , _local = startingLocal
            }

    startingSet = HashSet.singleton (startingLabel, startingStatus)

    loop history !seen (Choice steps) = do
        step <- steps

        case step of
            Done void -> do
                Void.absurd void

            Yield label rest -> do
                status <- get

                let key = (label, status)

                let newHistory = key : history

                if HashSet.member key seen
                    then do
                        Exception.throw (Nontermination newHistory)
                    else do
                        let newSeen = HashSet.insert key seen

                        loop newHistory newSeen rest

{-| `debug` is like `check` except that it will catch and pretty-print
     `ModelException`s
-}
debug
    :: ( Eq global
       , Eq label
       , Hashable global
       , Hashable label
       , Pretty label
       , Pretty global
       , Show global
       , Show label
       )
    => Coroutine global label
    -- ^ `Coroutine` to check
    -> NonEmpty global
    -- ^ Starting global state
    -> IO ()
debug coroutine startingGlobals = do
    result <- Exception.try (check coroutine startingGlobals)

    case result of
        Left  exception ->
            Pretty.Text.putDoc (pretty (exception :: ModelException))
        Right () ->
            mempty

{-| Class used for pretty-printing the local state

    The reason for not using the `Pretty` class is because the local state is
    internally represented as nested 2-tuples, which will look ugly when
    pretty-printed using the `Pretty` class
-}
class ToDocs a where
    toDocs :: a -> [Doc ann]
    default toDocs :: Pretty a => a -> [Doc ann]
    toDocs a = [ pretty a ]

instance ToDocs () where
    toDocs () = []

instance (ToDocs a, ToDocs b) => ToDocs (a, b) where
    toDocs (a, b) = toDocs a <> toDocs b

instance ToDocs Bool
instance ToDocs Char
instance ToDocs Double
instance ToDocs Float
instance ToDocs Int
instance ToDocs Int8
instance ToDocs Int16
instance ToDocs Int32
instance ToDocs Int64
instance ToDocs Integer
instance ToDocs Natural
instance ToDocs Word
instance ToDocs Word8
instance ToDocs Word16
instance ToDocs Word32
instance ToDocs Word64
instance ToDocs Void

instance (Pretty key, Pretty value) => ToDocs (HashMap key value) where
    toDocs m = fmap adapt (HashMap.toList m)
      where
        adapt (key, value) = pretty key <> ": " <> pretty value

sepBy :: [Doc ann] -> Doc ann -> Doc ann
[]           `sepBy` _   = mempty
(doc :   []) `sepBy` _   = doc
(doc : docs) `sepBy` sep = doc <> sep <> docs `sepBy` sep

commas :: [Doc ann] -> Doc ann
commas docs = docs `sepBy` ", "

bars :: [Doc ann] -> Doc ann
bars docs = docs `sepBy` " | "

bullets :: [Doc ann] -> Doc ann
bullets docs = map ("• " <>) docs `sepBy` Pretty.hardline
