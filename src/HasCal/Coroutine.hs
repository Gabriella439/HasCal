{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}

{-| This module provides the `Process` and `Coroutine` types and associated
    utilities for working with them
-}
module HasCal.Coroutine
    ( -- * Concurrent processes
      Process
    , Coroutine(..)

      -- * State management
    , Status(..)
    , global
    , local

    -- * PlusCal Statements
    -- $statements
    , yield
    , skip
    , either
    , with
    , while
    , await
    , assert
    , die
    , print

    -- * TLA+ expressions
    , forall_
    , exists_
    , (==>)
    , (<=>)
    , boolean
    , (-->)
    , (|->)
    , domain
    , range
    , choose

    -- * Model checking
    , Options(..)
    , defaultOptions
    , model

      -- * Error handling
    , ModelException(..)
    , PropertyFailedReason(..)

    -- * Classes
    , Pretties(..)
    ) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Exception.Safe (Exception)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Strict (MonadState(..), StateT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import Data.Void (Void)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)
import HasCal.Property (Check(..), Property, Universe(..))
import Lens.Micro.Platform (Lens')
import List.Transformer (ListT)
import Numeric.Natural (Natural)
import Prelude hiding (either, print)
import Prettyprinter (Doc, Pretty(..))

import qualified Control.Applicative as Applicative
import qualified Control.Exception.Safe as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.State.Lazy as State.Lazy
import qualified Control.Monad.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import qualified HasCal.Property as Property
import qualified Lens.Micro.Platform as Lens
import qualified List.Transformer as List
import qualified Prelude
import qualified Prettyprinter as Pretty
import qualified Prettyprinter.Render.String as Pretty.String
import qualified Prettyprinter.Render.Text as Pretty.Text
import qualified Text.Show as Show

hoistListT
    :: Functor m
    => (m (List.Step n a) -> n (List.Step n a)) -> ListT m a -> ListT n a
hoistListT nat (List.ListT x) = List.ListT (nat (fmap (hoistStep nat) x))

hoistStep
    :: Functor m
    => (m (List.Step n a) -> n (List.Step n a))
    -> List.Step m a -> List.Step n a
hoistStep nat (List.Cons a as) = List.Cons a (hoistListT nat as)
hoistStep _    List.Nil        = List.Nil

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

    You can combine multiple `Process`es using:

    * @do@ notation - Run `Process`es sequentially
    * (`<|>`) - Explore two `Process`es in parallel
    * (`<>`) - Run two `Process`es sequentially and combine their return values

    Finally, you will need to convert a `Process` into a `Coroutine` by wrapping
    the `Process` in the `Begin` constructor
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

instance MonadFail (Process global local label) where
    fail _ = empty

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
    throwM e = Choice (fmap Done (liftIO (throwM e)))

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
    { _global :: !global
      -- ^ Shared global state
    , _local :: !local
      -- ^ `Process`-local state
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (Hashable)

instance (Pretty global, Pretties local) => Pretty (Status global local) where
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
                    <>  Pretty.align (bullets localDocs)
                    )

        localDocs = pretties _local

-- | A lens for accessing the global state of a `Process`
global :: Lens' (Status global local) global
global k (Status a b) = fmap (\a' -> Status a' b) (k a)

-- | A lens for accessing the local state of a `Process`
local :: Lens' (Status global local) local
local k (Status a b) = fmap (\b' -> Status a b') (k b)

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
    .   (Eq local, Hashable local, Pretties local, Show local)
    =>  Begin
            { startingLabel  :: label
            , startingLocals :: [local]
            , process        :: Process global local label ()
            }

instance Functor (Coroutine global) where
    fmap = Applicative.liftA

instance Applicative (Coroutine global) where
    pure label = Begin label [()] empty

    Begin label0F sF fs0 <*> Begin label0X sX xs0 = Begin label0FX s fxs
      where
        (label0FX, fxs) = loop (label0F, fs0) (label0X, xs0)

        s = liftA2 (,) sF sX

        loop (label1F, Choice fs) (label1X, Choice xs) =
            (   label1F label1X
            ,       Choice (fmap adaptF (Lens.zoom onLeft fs))
                <|> Choice (fmap adaptX (Lens.zoom onRight xs))
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

{- $statements
   This section provides commands that correspond as closely as possible to the
   equivalent PlusCal commands of the same name.  These commands do not
   represent the full extent of what you can do within Haskell processes, but
   they do provide feature parity with PlusCal (with the exception of the
   @goto@ command, which is not supported here).

   Many of these commands are synonyms for utilities from Haskell's standard
   library.  For example, `await` is just a synonym for `Monad.guard`.  These
   synonyms exist primarily for educational purposes, to illustrate how
   PlusCal idioms correspond to Haskell idioms, but you can still use these
   synonyms if you want the Haskell code to resemble PlusCal as much as
   possible.
-}

{-| End the current atomic transition alongside a label for the current state.
    This potentially yields control to other `Process`es.

    If the exact same label and state have been reached before then the
    model checker behavior depends on whether you enable the `termination`
    check:

    * If you enable the `termination` check then the model checker will
      `Exception.throw` a `Nontermination` exception since revisiting the same
      state indicates a simulation path that permits an infinite loop

    * If you disable the `termination` check then the model checker will still
      end the current simulation branch since it has already visited this
      state before

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

    The equivalent Haskell code can elide many of those `skip`s:

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

{-| Non-deterministically simulate multiple subroutines, like an @either@
    statement in PlusCal

    This is a synonym for `Foldable.asum`, but with a `Process`-specific type
    signature.

    The model checker will explore all branches, succeeding only if all branches
    succeed.

    `either` obeys the following laws:

@
`either` [ a ] = a

`either` (as `<|>` bs) = `either` as `<|>` `either` bs

`either` `empty` = `empty`
@

    … or equivalently:

@
`either` (as `++` bs) = `either` as `<|>` `either` bs

`either` [] = `empty`
@

    Those rules also imply that:

@
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

    `with` is the same thing as using `either` to select from a list of `pure`
    subroutines:

@
`with` results = `either` (`fmap` `pure` results)
@

    `with` obeys the following laws:

@
`with` (as `<|>` bs) = `with` as `<|>` `with` bs

`with` `empty` = `empty`

`with` (`pure` a) = `pure` a
@

    … or equivalently:

@
`with` (as `++` bs) = `with` as `<|>` `with` bs

`with` [] = `empty`

`with` [a] = `pure` a
@

-}
with
    :: (Foldable list, Functor list)
    => list result -> Process global local label result
with results = either (fmap pure results)

{-| Run a loop so long as the loop condition does not return `True`, like a
    @while@ statement in PlusCal

    You will typically /not/ want to use this and instead you will more likely
    want to use one of the utilities from "Control.Monad".  This is only
    provided for parity with PlusCal.

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
`await` `False` = `empty`
`await` `True`  = `mempty`

`await` (a `||` b) = `await` a `<|>` `await` b
`await` (a `&&` b) = `await` a `<>` `await` b
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
    :: (Pretties local, Pretty global, Show local, Show global)
    => Bool
    -- ^ Condition
    -> Process global local label ()
assert True  = skip
assert False = do
    _status <- get
    Exception.throw AssertionFailed{ _status }

-- | Die with an error message
die :: Text -> Process global local label result
die _message = Exception.throw Failure{ _message }

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
boolean :: [Bool]
boolean = universe

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

{-| Find the first matching element, like the @CHOOSE@ function in TLA+ except
    that this will return a `Nothing` instead of throwing an exception

    `choose` is like `List.find`, but with the arguments `flip`ped.
-}
choose :: Foldable list => list a -> (a -> Bool) -> Maybe a
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
      =>  Nontermination { _history :: [(label, status)] }
          -- ^ The process does not necessarily terminate because at least one
          --   branch of execution permits an infinite cycle
    |     Deadlock
          -- ^ The process deadlocked, meaning that no branch of execution
          --   successfully ran to completion
    |     forall local . (Pretty local, Show local)
      =>  AssertionFailed { _status :: local }
          -- ^ The process failed to satisfy an `assert` statement
    |     forall global label
      .   (Pretty global, Pretty label, Show global, Show label)
      =>  PropertyFailed
               { _propertyHistory :: [(global, label)]
               , _reason :: PropertyFailedReason
               }
          -- ^ At least one branch of execution failed to satisfy the specified
          -- `Property`
    |     Failure { _message :: Text }
          -- ^ Used by the `fail` method

-- | The reason why a `PropertyFailed` exception was thrown
data PropertyFailedReason
    = Unsatisfiable
    -- ^ The `Property` can no longer satisfied, regardless of future input
    | UnsatisfyingConclusion
    -- ^ We could not satisfy the `Property` before reaching the end of the
    --   input sequence.  For example, you would get this error if you check the
    --   `Property.eventually` `Property` against an input sequence where every
    --   input was `False`
    deriving (Show)

instance Show ModelException where
    showsPrec _ Nontermination{ _history } =
          showString "Nontermination {_history = "
        . Show.showListWith shows _history
        . showString "}"
    showsPrec _ Deadlock =
          showString "Deadlock"
    showsPrec _ AssertionFailed{ _status } =
          showString "AssertionFailed {_status = "
        . shows _status
        . showString "}"
    showsPrec _ PropertyFailed{ _propertyHistory, _reason } =
          showString "PropertyFailed {_propertyHistory = "
        . shows _propertyHistory
        . showString ", _reason = "
        . shows _reason
        . showString "}"
    showsPrec _ Failure{ _message } =
          showString "Failure {_message = "
        . shows _message
        . showString "}"

instance Exception ModelException where
    displayException exception =
        Pretty.String.renderString
            (Pretty.layoutPretty Pretty.defaultLayoutOptions (pretty exception))

instance Pretty ModelException where
    pretty Nontermination{ _history } = Pretty.group (Pretty.flatAlt long short)
      where
        short = "Non-termination" <> suffix
          where
            suffix = case _history of
                [] ->
                    mempty

                seens  ->
                    " - History: " <> bars (map adapt (reverse seens))
                  where
                    adapt (label, _status) =
                        commas [ pretty label, pretty _status ]

        long = Pretty.align ("Non-termination" <> suffix)
          where
            suffix = case _history of
                [] ->
                    mempty
                seens ->
                        Pretty.hardline
                    <>  Pretty.hardline
                    <>  "History:"
                    <>  Pretty.hardline
                    <>  foldMap outer (reverse seens)
                  where
                    outer (label, _status) =
                            "- "
                        <>  Pretty.align
                            (   "Label: "
                            <>  pretty label
                            <>  Pretty.hardline
                            <>  "State: "
                            <>  pretty _status
                            )
                        <>  Pretty.hardline

    pretty Deadlock =
        "Deadlock"

    pretty AssertionFailed{ _status } =
        Pretty.align
            (   "Assertion failed"
            <>  Pretty.hardline
            <>  Pretty.hardline
            <>  "State: "
            <>  pretty _status
            )

    pretty PropertyFailed{ _propertyHistory, _reason } =
        Pretty.group (Pretty.flatAlt long short)
      where
        reason = case _reason of
            Unsatisfiable          -> "unsatisfiable"
            UnsatisfyingConclusion -> "unsatisfying conclusion"

        short = "Property failed: " <> reason <> suffix
          where
            suffix = case _propertyHistory of
                [] ->
                    mempty

                seens  ->
                    " - History: " <> bars (map adapt (reverse seens))
                  where
                    adapt (_global, label) =
                        commas [ pretty label, pretty _global ]

        long = Pretty.align ("Property failed: " <> reason <> suffix)
          where
            suffix = case _propertyHistory of
                [] -> mempty
                seens ->
                        Pretty.hardline
                    <>  Pretty.hardline
                    <>  "History:"
                    <>  Pretty.hardline
                    <>  foldMap outer (reverse seens)
                  where
                    outer (_global, label) =
                            "- "
                        <>  Pretty.align
                            (   "Label: "
                            <>  pretty label
                            <>  Pretty.hardline
                            <>  "State: "
                            <>  pretty _global
                            )
                        <>  Pretty.hardline

    pretty Failure{ _message } = "Failure: " <> pretty _message

-- | Model-checking options
data Options = Options
    { termination :: Bool
      -- ^ When `True`, throw a `Nontermination` exception if any cycles are
      -- detected or a `Deadlock` exception if no execution branch terminates
    , debug :: Bool
      -- ^ When `True`, pretty-print any exception before throwing the
      --   exception
    }

{-| Default model-checking options

> defaultOptions = Options{ termination = True, debug = False }
-}
defaultOptions :: Options
defaultOptions = Options{ termination = True, debug = False }

{-  This type is used internally within the `model` function to keep track of
    state specific to one \"timeline\" of the model checker (i.e. one possible
    branch of execution)

    This `Timeline` state is isolated from other branches of execution
-}
data Timeline global local label status = Timeline
    { _processStatus  :: !(Status global local)
      -- ^ This stores the internal state of the `Process`
    , _history        :: [(label, Status global local)]
      -- ^ This is kept for error reporting so that if things go wrong we can
      --   report to the user what sequence of events led up to the problem
    , _historySet     :: !(HashSet (label, Status global local))
      -- ^ This always the same as @`HashSet.fromList` _history@,
      --   but kept as a separate field for efficiently updating and querying
      --   which states we've seen so far in order to detect cycles
    , _propertyStatus :: !(HashSet status)
      -- ^ This stores the internal state of the temporal `Property`
    , _propertyHistory :: [(global, label)]
    }

-- A lens for accessing the process status of a `Timeline`
processStatus
    :: Lens' (Timeline global local label status) (Status global local)
processStatus k (Timeline a b c d e) = fmap (\a' -> Timeline a' b c d e) (k a)

{-| Run the model checker on a `Coroutine` by supplying a list of starting
    states

    If you want to check more than one `Coroutine`, then combine those
    `Coroutine`s using `Applicative` operations or @ApplicativeDo@ notation
-}
model
    :: ( Eq global
       , Eq label
       , Hashable global
       , Hashable label
       , Pretty label
       , Pretty global
       , Show global
       , Show label
       )
    => Options
    -- ^ Model checking options
    -> Coroutine global label
    -- ^ `Coroutine` to check
    -> Property (global, label) Bool
    -- ^ `Property` to check
    -> [global]
    -- ^ Starting global state
    -> IO ()
model
    Options{ debug, termination }
    Begin{ startingLabel, startingLocals, process }
    property
    startingGlobals =
    case Property.check property of
        Check finalPropertyStatus stepProperty -> do
            successfulBranches <- handler
                (State.evalStateT
                    (List.fold (\n _ -> n + 1) (0 :: Natural) id
                        (State.evalStateT action uninitializedTimeline)
                    )
                    startingSet
                )

            Monad.unless (termination ==> 0 < successfulBranches) do
                Exception.throw Deadlock
          where
            uninitializedTimeline =
                error "Internal error - Uninitialized timeline"

            action = do
                startingGlobal <- lift (List.select startingGlobals)

                startingLocal <- lift (List.select startingLocals)

                let startingProcessStatus = Status
                        { _global = startingGlobal
                        , _local  = startingLocal
                        }

                let startingPropertyInput = (startingGlobal, startingLabel)

                let _propertyHistory = [ startingPropertyInput ]

                let _propertyStatus = HashSet.fromList do
                        s <- universe

                        -- The temporal `Property` only needs to return `True`
                        -- for the first output, indicating that the property
                        -- holds for the entire sequence
                        (True, s') <- State.Lazy.runStateT (stepProperty startingPropertyInput) s

                        return s'

                Monad.when (HashSet.null _propertyStatus) do
                    let _reason = Unsatisfiable

                    liftIO (Exception.throw PropertyFailed{ _propertyHistory, _reason })

                let historyKey = (startingLabel, startingProcessStatus)

                put $! Timeline
                    { _processStatus   = startingProcessStatus
                    , _history         = [ historyKey ]
                    , _historySet      = HashSet.singleton historyKey
                    , _propertyStatus
                    , _propertyHistory
                    }

                loop process

            startingSet = HashSet.empty

            loop (Choice steps) = do
                step <- Lens.zoom processStatus (State.mapStateT (hoistListT lift) steps)

                case step of
                    Done () -> do
                        Timeline{ _propertyStatus, _propertyHistory } <- get

                        Monad.unless (HashSet.member finalPropertyStatus _propertyStatus) do
                            let _reason = UnsatisfyingConclusion

                            liftIO (Exception.throw PropertyFailed{ _propertyHistory, _reason })

                    Yield label rest -> do
                        Timeline{ _processStatus, _history, _historySet, _propertyStatus, _propertyHistory } <- get

                        let Status{ _global } = _processStatus

                        let propertyInput = (_global, label)

                        let newPropertyStatus = HashSet.fromList do
                                s <- HashSet.toList _propertyStatus

                                -- We're uninterested in the output of the
                                -- temporal `Property` for subsequent outputs
                                -- because we don't care if the temporal
                                -- `Property` holds for a suffix of the behavior
                                State.Lazy.execStateT (stepProperty propertyInput) s

                        let seenKey = (label, _processStatus, newPropertyStatus)

                        let historyKey = (label, _processStatus)

                        let newHistory = historyKey : _history

                        let newPropertyHistory =
                                propertyInput : _propertyHistory

                        Monad.when (HashSet.null newPropertyStatus) do
                            let _reason = Unsatisfiable
                            liftIO (Exception.throw PropertyFailed{ _propertyHistory = newPropertyHistory, _reason })

                        seen <- lift get

                        Monad.when (HashSet.member historyKey _historySet && termination) do
                            liftIO (Exception.throw (Nontermination newHistory))

                        Monad.when (HashSet.member seenKey seen) empty

                        lift (State.put $! HashSet.insert seenKey seen)

                        put $! Timeline
                            { _processStatus
                            , _history = newHistory
                            , _historySet = HashSet.insert historyKey _historySet
                            , _propertyStatus = newPropertyStatus
                            , _propertyHistory = newPropertyHistory
                            }

                        loop rest

            handler :: IO a -> IO a
            handler
                | debug     = Exception.handle display
                | otherwise = id
              where
                display :: ModelException -> IO a
                display exception = do
                    Pretty.Text.putDoc (pretty (exception :: ModelException) <> "\n")
                    Exception.throwIO exception

{-| Class used for pretty-printing the local state

    The reason for not using the `Pretty` class is because the local state is
    internally represented as nested 2-tuples, which will look ugly when
    pretty-printed using the `Pretty` class
-}
class Pretties a where
    pretties :: a -> [Doc ann]
    default pretties :: Pretty a => a -> [Doc ann]
    pretties a = [ pretty a ]

instance Pretties () where
    pretties () = []

instance (Pretties a, Pretties b) => Pretties (a, b) where
    pretties (a, b) = pretties a <> pretties b

instance Pretties Bool
instance Pretties Char
instance Pretties Double
instance Pretties Float
instance Pretties Int
instance Pretties Int8
instance Pretties Int16
instance Pretties Int32
instance Pretties Int64
instance Pretties Integer
instance Pretties Natural
instance Pretties Word
instance Pretties Word8
instance Pretties Word16
instance Pretties Word32
instance Pretties Word64
instance Pretties Void

instance (Pretty key, Pretty value) => Pretties (HashMap key value) where
    pretties m = fmap adapt (HashMap.toList m)
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
bullets docs = map ("- " <>) docs `sepBy` Pretty.hardline
