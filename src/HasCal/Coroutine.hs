{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeApplications          #-}

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

    -- * Model checking
    , Model(..)
    , defaultModel
    , model

    -- * Model input
    , Input(..)
    , state
    , label

      -- * Error handling
    , ModelException(..)
    , PropertyFailedReason(..)
    ) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Exception.Safe (Exception)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logic (LogicT(..))
import Control.Monad.State.Strict (MonadState(get,put), StateT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Aeson (ToJSON(..), Value(..))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap (KeyMap)
import Data.Algorithm.Diff (PolyDiff(..))
import Data.HashSet (HashSet)
import Data.Hashable (Hashable(..))
import Data.Monoid (Any(..), Ap)
import Data.Text (Text)
import GHC.Generics (Generic)
import HasCal.Expression (Boolean(..), Universe(..))
import HasCal.Property (Check(..), Property)
import Lens.Micro.Platform (Lens')
import Numeric.Natural (Natural)
import Prelude hiding (either, print)
import Prettyprinter (Doc, Pretty(..))
import Prettyprinter.Render.Terminal (AnsiStyle, Color(..))

import qualified Control.Applicative as Applicative
import qualified Control.Exception.Safe as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.Logic as Logic
import qualified Control.Monad.State.Lazy as State.Lazy
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Writer as Writer
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Algorithm.Diff as Diff
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.HashSet as HashSet
import qualified Data.HashTable.IO as HashTable
import qualified Data.HashTable.ST.Cuckoo as Cuckoo
import qualified Data.IORef as IORef
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified HasCal.Property as Property
import qualified Lens.Micro.Platform as Lens
import qualified Prelude
import qualified Prettyprinter as Pretty
import qualified Prettyprinter.Render.String as Pretty.String
import qualified Prettyprinter.Render.Terminal as Pretty.Terminal
import qualified Prettyprinter.Render.Text as Pretty.Text
import qualified System.Console.ANSI as ANSI
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Show as Show

{- $setup

   >>> :m -Prelude
   >>> import Prelude hiding ((.), id)
   >>> import HasCal
-}

-- | Convert a list to the equivalent `LogicT`
select :: [a] -> LogicT m a
select as = LogicT (\cons nil -> foldr cons nil as)

{-| Assert that a `LogicT` makes progress by throwing a `Deadlock` exception if
    the `LogicT` is empty
-}
progressive :: LogicT IO a -> LogicT IO a
progressive (LogicT k) = LogicT k'
  where
    k' cons nil = do
        (nonEmpty, s) <- k cons' nil'
        if nonEmpty
            then return s
            else Exception.throw Deadlock
      where
        cons' a m = fmap ((,) True) (cons a (fmap snd m))

        nil' = fmap ((,) False) nil

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
    the `Process` in the `Coroutine` constructor
-}
newtype Process global local label result
    = Choice
        { possibilities
            :: StateT (Status global local) (LogicT IO)
                (Step global local label result)
        }
    deriving stock (Functor)
    deriving (Boolean, Semigroup, Monoid)
      via (Ap (Process global local label) result)

instance Applicative (Process global local label) where
    pure result = Choice (pure (Done result))

    (<*>) = Monad.ap

instance Monad (Process global local label) where
    Choice ps >>= f = Choice do
        p <- ps
        case p of
            Yield _label rest -> do
                return (Yield _label (rest >>= f))
            Done result -> possibilities (f result)

instance MonadFail (Process global local label) where
    fail _ = empty

instance Alternative (Process global local label) where
    empty = Choice empty

    Choice psL <|> Choice psR = Choice (psL <|> psR)

instance MonadState (Status global local) (Process global local label) where
    get = Choice (fmap Done get)

    put s = Choice (fmap Done (put s))

    state k = Choice (fmap Done (State.state k))

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
      deriving anyclass (Hashable, ToJSON)

-- | A lens for accessing the global state of a `Process`
global :: Lens' (Status global local) global
global k (Status a b) = fmap (\a' -> Status a' b) (k a)
{-# INLINABLE global #-}

-- | A lens for accessing the local state of a `Process`
local :: Lens' (Status global local) local
local k (Status a b) = fmap (\b' -> Status a b') (k b)
{-# INLINABLE local #-}

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

    * The `Coroutine`` constructor - Wrap a `Process` with a starting state and
      label to create a `Coroutine`
    * `pure` - A `Coroutine` with a single state and no valid transitions
    * `mempty` - A `Coroutine` that does nothing

    You can combine `Coroutine`s using:

    * `Functor` and `Applicative` utilities (e.g. (`<$>`) and (`<*>`))
    * @do@ notation, if you enable @ApplicativeDo@
    * (`<>`) - Run two `Coroutine`s in parallel and combine their results
-}
data Coroutine global label =
        forall local
    .   (Eq local, Hashable local, ToJSON local, Show local)
    =>  Coroutine
            { startingLabel  :: label
            , startingLocals :: [local]
            , process        :: Process global local label ()
            }
    deriving (Boolean, Semigroup, Monoid)
      via (Ap (Coroutine global) label)

instance Functor (Coroutine global) where
    fmap = Applicative.liftA

instance Applicative (Coroutine global) where
    pure _label = Coroutine _label (pure Unit) empty

    Coroutine label0F sF fs0 <*> Coroutine label0X sX xs0 =
        Coroutine label0FX s fxs
      where
        (label0FX, fxs) = loop (label0F, fs0) (label0X, xs0)

        s = liftA2 Pair sF sX

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

            onLeft :: Lens' (Status global (Pair l r)) (Status global l)
            onLeft k (Status g (Pair l r)) = fmap adapt (k (Status g l))
              where
                adapt (Status g' l') = Status g' (Pair l' r)

            onRight :: Lens' (Status global (Pair l r)) (Status global r)
            onRight k (Status g (Pair l r)) = fmap adapt (k (Status g r))
              where
                adapt (Status g' r') = Status g' (Pair l r')

data Unit = Unit
    deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)

instance ToJSON Unit where
    toJSON Unit = toJSON ([] :: [Value])

data Pair a b = Pair !a !b
    deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)

instance (ToJSON a, ToJSON b) => ToJSON (Pair a b) where
    toJSON (Pair a b) =
        case (toJSON a, toJSON b) of
            (Array as, Array bs) -> Array (as <> bs)
            (a'      , b'      ) -> toJSON [ a', b' ]

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
    model checker behavior depends on whether you enable the termination
    check:

    * If you enable the termination check then the model checker will
      `Exception.throw` a `Nontermination` exception since revisiting the same
      state indicates a simulation path that permits an infinite loop

    * If you disable the termination check then the model checker will still
      end the current simulation branch since it has already visited this
      state before

-}
yield :: label -> Process global local label ()
yield _label = Choice (pure (Yield _label mempty))
{-# INLINABLE yield #-}

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
{-# INLINE skip #-}

{-| Non-deterministically simulate multiple subroutines, like an @either@
    statement in PlusCal

    This is a synonym for @"Data.Foldable".`Foldable.asum`@, but with a
    `Process`-specific type signature.

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
{-# INLINE either #-}

{-| Non-deterministically select from one of multiple possible values, like
    a @with@ statement in PlusCal

    `with` is (essentially) the same thing as using `either` to select from a
    list of `pure` subroutines:

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
with :: Foldable list => list result -> Process global local label result
with = foldr cons empty
  where
    cons result rest = pure result <|> rest
{-# INLINABLE with #-}

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

    This is a synonym for @"Control.Monad".`Monad.guard`@, but with a
    `Process`-specific type signature.

@
`await` `False` = `empty`
`await` `True`  = `mempty`

`await` (a `||` b) = `await` a `<|>` `await` b
`await` (a `&&` b) = `await` a `<>` `await` b
@
-}
await :: Bool -> Process global local label ()
await = Monad.guard
{-# INLINE await #-}

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
    :: (ToJSON local, ToJSON global, Show local, Show global)
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
{-# INLINABLE print #-}

{-| The `ModelException` type represents all of the ways in which the model
    checker can fail
-}
data ModelException =
          forall value . (Show value, ToJSON value)
      =>  Nontermination { _history :: [value] }
          -- ^ The process does not necessarily terminate because at least one
          --   branch of execution permits an infinite cycle
    |     Deadlock
          -- ^ The process deadlocked, meaning that no branch of execution
          --   successfully ran to completion
    |     forall value . (Show value, ToJSON value)
      =>  AssertionFailed { _status :: value }
          -- ^ The process failed to satisfy an `assert` statement
    |     forall value . (Show value, ToJSON value)
      =>  PropertyFailed
               { _inputHistory :: [value]
               , _reason :: PropertyFailedReason
               }
          -- ^ At least one branch of execution failed to satisfy the specified
          -- `Property`
    |     Failure { _message :: Text }
          -- ^ Used by the `fail` method

instance Pretty ModelException where
    pretty = Pretty.unAnnotate . prettyModelException

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
    showsPrec _ PropertyFailed{ _inputHistory, _reason } =
          showString "PropertyFailed {_inputHistory = "
        . Show.showListWith shows _inputHistory
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

data HistoryKey a b = HistoryKey{ _label :: !a, _status :: !b }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable, ToJSON)

prettyKey :: Text -> Doc ann
prettyKey =
      Pretty.pretty
    . spaces
    . Text.concat
    . fmap capitalizeWord
    . Text.splitOn "_"
    . Text.dropWhile (== '_')
  where
    space c
        | Char.isUpper c = Text.pack [ ' ', c ]
        | otherwise      = Text.singleton c

    capitalizeWord text =
        case Text.uncons text of
            Just (t, ext) ->
                Text.cons (Char.toUpper t) ext
            Nothing ->
                text

    spaces text =
        case Text.uncons text of
            Just (t, ext) ->
                Text.cons (Char.toUpper t) (Text.concatMap space ext)
            Nothing ->
                text

prettyValue :: Value -> Doc ann
prettyValue = loop
  where
    loop (Array values) = Pretty.group (Pretty.flatAlt long short)
      where
        long = Pretty.align (lined (fmap item values))

        short
            | null values = "[ ]"
            | otherwise   = "[ " <> commas (fmap loop values) <> " ]"

        item value = "- " <> loop value
    loop (Object keyValues) =
        Pretty.group (Pretty.flatAlt long short)
      where
        long = Pretty.align (lined (fmap processLong list))

        short
            | null keyValues = "{ }"
            | otherwise      = "{ " <> commas (fmap processShort list) <> " }"

        list = Aeson.KeyMap.toList keyValues

        processLong (key, value) =
                prettyKey (Aeson.Key.toText key)
            <>  ":"
            <>  Pretty.hardline
            <>  "  "
            <>  loop value

        processShort (key, value) =
            prettyKey (Aeson.Key.toText key) <> ": " <> loop value
    loop (String text) =
        Pretty.pretty (show text)
    loop (Number scientific) =
      case Scientific.floatingOrInteger scientific of
          Left  double  -> Pretty.pretty @Double double
          Right integer -> Pretty.pretty @Integer integer
    loop (Bool bool) =
        Pretty.pretty bool
    loop Null =
        "null"

prettyValueList :: [Value] -> Doc AnsiStyle
prettyValueList values =
    case values of
        before : afters -> Pretty.group (Pretty.flatAlt long short)
          where
            docs = prettyValue before : diffs before afters

            long = Pretty.align (lined (fmap process docs))

            short
                | null docs = "[ ]"
                | otherwise = "[ " <> commas docs <> " ]"

            process doc = "- " <> doc
        _ -> prettyValue (toJSON values)
  where
    diffs :: Value -> [Value] -> [Doc AnsiStyle]
    diffs _ [] =
        [ ]
    diffs before (after : afters) = do
        snd (diff before after) : diffs after afters

    diff :: Value -> Value -> (Any, Doc AnsiStyle)
    diff (Array old) (Array new) = do
        let oldList = Foldable.toList old
        let newList = Foldable.toList new

        let docs (o : ld) (n : ew) = do
                d  <- diff o n
                ds <- docs ld ew
                return (d : ds)
            docs _ new' = do
                return (fmap (plus . prettyValue) new')

        (ds, Any matching) <- Writer.listen (docs oldList newList)

        ds' <- do
            if matching
                then do
                    return ds
                else do
                    let render (First  a) = minus (prettyValue a)
                        render (Second a) = plus  (prettyValue a)
                        render (Both a _) =        prettyValue a

                    return (fmap render (Diff.getDiff oldList newList))

        let short
                | null ds' =
                    "[ ]"
                | otherwise =
                    "[ " <> commas ds' <> " ]"

        let long = Pretty.align (lined (fmap item ds'))
              where
                item d = "- " <> d

        return (Pretty.group (Pretty.flatAlt long short))
    diff (Object old) (Object new)
        | let both = Aeson.KeyMap.intersection old new
        , not (Aeson.KeyMap.null both) = do
            Writer.tell (Any True)

            let extraOlds = Aeson.KeyMap.difference old new
            let extraNews = Aeson.KeyMap.difference new old

            let combine
                    :: Key
                    -> Value
                    -> Value
                    -> (Any, (Doc AnsiStyle, Doc AnsiStyle))
                combine key o n = do
                    doc <- diff o n
                    let long =
                                prettyKey (Aeson.Key.toText key)
                            <>  ":"
                            <>  Pretty.hardline
                            <>  "  "
                            <>  doc

                    let short =
                                prettyKey (Aeson.Key.toText key)
                            <>  ": "
                            <>  doc

                    return (long, short)

            let boths :: KeyMap (Any, (Doc AnsiStyle, Doc AnsiStyle))
                boths = Aeson.KeyMap.intersectionWithKey combine old new

            (bothLongs, bothShorts) <- do
                fmap unzip (Monad.sequence (Aeson.KeyMap.elems boths))

            let (extraNewLongs, extraNewShorts) =
                    unzip (fmap (extra plus) (Aeson.KeyMap.toList extraNews))

            let (extraOldLongs, extraOldShorts) =
                    unzip (fmap (extra minus) (Aeson.KeyMap.toList extraOlds))

            let longs  = extraOldLongs <> extraNewLongs  <> bothLongs
            let shorts = extraOldShorts <> extraNewShorts <> bothShorts

            let long = Pretty.align (lined longs)

            let short
                    | null shorts =
                        "{ }"
                    | otherwise =
                        "{ " <> commas shorts <> " }"

            return (Pretty.group (Pretty.flatAlt long short))
      where
        extra sign (key, value) =
            ( sign
                (   prettyKey (Aeson.Key.toText key)
                <>  ":"
                <>  Pretty.hardline
                <>  "  "
                <>  prettyValue value
                )
            , sign
                (   prettyKey (Aeson.Key.toText key)
                <>  ": "
                <>  prettyValue value
                )
            )
    diff old new
        | old == new = do
            Writer.tell (Any True)
            return (prettyValue new)
        | otherwise = do
            return (plus (prettyValue new))
    
    minus = Pretty.annotate (Pretty.Terminal.color Red  )
    plus  = Pretty.annotate (Pretty.Terminal.color Green)

lined :: Foldable list => list (Doc ann) -> Doc ann
lined = Pretty.concatWith append
  where
    append x y = x <> Pretty.hardline <> y

commas :: Foldable list => list (Doc ann) -> Doc ann
commas = Pretty.concatWith append
  where
    append x y = x <> ", " <> y

prettyModelException :: ModelException -> Doc AnsiStyle
prettyModelException  Nontermination{ _history } =
    Pretty.align
        (   "Non-termination"
        <>  Pretty.hardline
        <>  Pretty.hardline
        <>  prettyValueList (fmap toJSON (reverse _history))
        )
prettyModelException Deadlock =
    "Deadlock"
prettyModelException AssertionFailed{ _status } =
    Pretty.align
        (   "Assertion failed"
        <>  Pretty.hardline
        <>  Pretty.hardline
        <>  prettyValue (toJSON _status)
        )
prettyModelException PropertyFailed{ _inputHistory, _reason } =
    Pretty.align
        (   "Property failed: " <> reason
        <>  Pretty.hardline
        <>  Pretty.hardline
        <>  prettyValueList (fmap toJSON (reverse _inputHistory))
        )
  where
    reason = case _reason of
        Unsatisfiable          -> "unsatisfiable"
        UnsatisfyingConclusion -> "unsatisfying conclusion"

prettyModelException Failure{ _message } = "Failure: " <> pretty _message

{-| A `Model` represents the  model to check, alongside all model-checking
    options
-}
data Model global label = Model
    { termination :: Bool
      -- ^ When `True`, throw a `Nontermination` exception if any cycles are
      -- detected
    , deadlock :: Bool
      -- ^ When `True`, throw a `Deadlock` exception if if the `Coroutine`
      --   cannot make progress
    , debug :: Bool
      -- ^ Set this to `True` if you want to pretty-print the `ModelException`
      --   and instead throw @`Exit.ExitFailure` 1@ in its place
    , statistics :: Bool
      -- ^ Set this to `True` to display model-checking statistics at the end
      --   of the run
    , coroutine :: Coroutine global label
      -- ^ `Coroutine` to check
    , property :: Property (Input global label) Bool
      -- ^ `Property` to check
    , startingGlobals :: [global]
      -- ^ Possible starting global states
    }

{-| Default model-checking options

    > defaultModel = Model
    >     { termination = `True`
    >     , deadlock = `True`
    >     , debug = `False`
    >     , statistics = `False`
    >     , coroutine = `mempty`
    >     , property = `true`
    >     , startingGlobals = pure ()
    >     }
-}
defaultModel :: Model () ()
defaultModel = Model
    { termination = True
    , deadlock = True
    , debug = False
    , statistics = False
    , coroutine = mempty
    , property = true
    , startingGlobals = pure ()
    }

{-  This type is used internally within the `model` function to keep track of
    state specific to one \"timeline\" of the model checker (i.e. one possible
    branch of execution)

    This `Timeline` state is isolated from other branches of execution
-}
data Timeline global local label status = Timeline
    { _history        :: [ HistoryKey label (Status global local) ]
      -- ^ This is kept for error reporting so that if things go wrong we can
      --   report to the user what sequence of events led up to the problem
    , _historySet     :: !(HashSet (HistoryKey label (Status global local)))
      -- ^ This always the same as @`HashSet.fromList` _history@,
      --   but kept as a separate field for efficiently updating and querying
      --   which states we've seen so far in order to detect cycles
    , _propertyStatus :: !(HashSet status)
      -- ^ This stores the internal state of the temporal `Property`
    , _inputHistory :: [ Input global label ]
    }

{-| Run the model checker on a `Coroutine` by supplying a list of starting
    states

    If you want to check more than one `Coroutine`, then combine those
    `Coroutine`s using `Applicative` operations or @ApplicativeDo@ notation

    >>> model defaultModel  -- The default model has no valid execution branches
    *** Exception: Deadlock

    >>> -- An example coroutine with one execution branch that does nothing
    >>> exampleCoroutine = Coroutine{ startingLabel = (), startingLocals = pure (), process = mempty }
    >>> model defaultModel{ coroutine = exampleCoroutine }  -- Success


    >>> -- Create a coroutine that never terminates
    >>> endlessCoroutine = Coroutine{ startingLabel = False, startingLocals = pure (), process = while true (yield True) }
    >>> model defaultModel{ coroutine = endlessCoroutine, property = true }
    *** Exception: Nontermination {_history = [HistoryKey {_label = True, _status = Status {_global = (), _local = ()}},HistoryKey {_label = True, _status = Status {_global = (), _local = ()}},HistoryKey {_label = False, _status = Status {_global = (), _local = ()}}]}

    >>> -- Enable debugging output for clarity
    >>> model defaultModel{ coroutine = endlessCoroutine, property = true, debug = True }
    Non-termination
    ...
    - { Label: False, Status: { Global: [ ], Local: [ ] } }
    - { Label: True, Status: { Global: [ ], Local: [ ] } }
    - { Label: True, Status: { Global: [ ], Local: [ ] } }
    *** Exception: ExitFailure 1

    >>> -- Disable the termination checker if desired
    >>> model defaultModel{ coroutine = endlessCoroutine, property = true, debug = True, termination = False }

    >>> -- Check a non-trivial property that succeeds
    >>> exampleProperty = eventually . always . viewing label
    >>> model defaultModel{ coroutine = endlessCoroutine, property = exampleProperty, debug = True, termination = False }

    >>> -- Check a non-trivial property that fails
    >>> model defaultModel{ coroutine = endlessCoroutine, property = always . viewing (label . to not), debug = True, termination = False }
    Property failed: unsatisfiable
    ...
    [ { Label: False, State: [ ] }, { Label: True, State: [ ] } ]
    *** Exception: ExitFailure 1
-}
model
    :: ( Eq global
       , Eq label
       , Hashable global
       , Hashable label
       , ToJSON label
       , ToJSON global
       , Show global
       , Show label
       )
    => Model global label
    -- ^ Model checking options
    -> IO ()
model Model
    { debug
    , statistics
    , termination
    , deadlock
    , property
    , startingGlobals
    , coroutine = Coroutine{ startingLabel, startingLocals, process }
    } = do
    terminal <- ANSI.hSupportsANSI IO.stdout

    let putDoc doc =
            if terminal
            then Pretty.Terminal.putDoc (doc <> "\n")
            else Pretty.Text.putDoc (doc <> "\n")

    redundantStatesReference <- IORef.newIORef 0

    case Property.check property of
        Check finalPropertyStatus stepProperty -> do
            hashtable <- HashTable.new @Cuckoo.HashTable

            let action = do
                    startingGlobal <- lift (lift (select startingGlobals))

                    startingLocal <- lift (lift (select startingLocals))

                    let startingProcessStatus = Status
                            { _global = startingGlobal
                            , _local  = startingLocal
                            }

                    let startingInput = Input startingGlobal startingLabel

                    let _inputHistory = [ startingInput ]

                    let _propertyStatus = HashSet.fromList do
                            s <- universe

                            -- The temporal `Property` only needs to return `True`
                            -- for the first output, indicating that the property
                            -- holds for the entire sequence
                            (True, s') <- State.Lazy.runStateT (stepProperty startingInput) s

                            return s'

                    Monad.when (HashSet.null _propertyStatus) do
                        let _reason = Unsatisfiable

                        liftIO (Exception.throw PropertyFailed{ _inputHistory, _reason })

                    let historyKey = HistoryKey startingLabel startingProcessStatus

                    lift (put $! startingProcessStatus)

                    put $! Timeline
                        { _history         = [ historyKey ]
                        , _historySet      = HashSet.singleton historyKey
                        , _propertyStatus
                        , _inputHistory
                        }

                    loop process

                loop (Choice steps) = do
                    let wrap
                            | deadlock  = State.mapStateT progressive
                            | otherwise = id

                    step <- lift (wrap steps)

                    case step of
                        Done () -> do
                            Timeline{ _propertyStatus, _inputHistory } <- get

                            Monad.unless (HashSet.member finalPropertyStatus _propertyStatus) do
                                let _reason = UnsatisfyingConclusion

                                liftIO (Exception.throw PropertyFailed{ _inputHistory, _reason })

                        Yield _label rest -> do
                            _processStatus <- lift get

                            Timeline{ _history, _historySet, _propertyStatus, _inputHistory } <- get

                            let Status{ _global } = _processStatus

                            let input = Input{ _state = _global, _label }

                            let newPropertyStatus = HashSet.fromList do
                                    s <- HashSet.toList _propertyStatus

                                    -- We're uninterested in the output of the
                                    -- temporal `Property` for subsequent outputs
                                    -- because we don't care if the temporal
                                    -- `Property` holds for a suffix of the behavior
                                    State.Lazy.execStateT (stepProperty input) s

                            let seenKey = Seen{ _label, _processStatus, _propertyStatus = newPropertyStatus }

                            let historyKey = HistoryKey _label _processStatus

                            let newHistory = historyKey : _history

                            let newInputHistory = input : _inputHistory

                            Monad.when (HashSet.null newPropertyStatus) do
                                let _reason = Unsatisfiable
                                liftIO (Exception.throw PropertyFailed{ _inputHistory = newInputHistory, _reason })

                            Monad.when (HashSet.member historyKey _historySet && termination) do
                                liftIO (Exception.throw (Nontermination newHistory))

                            maybeSeen <- liftIO (HashTable.lookup hashtable seenKey)

                            let member = case maybeSeen of
                                    Nothing -> False
                                    Just _  -> True

                            Monad.when member do
                                liftIO (IORef.modifyIORef' redundantStatesReference (+ 1))
                                empty

                            liftIO (HashTable.insert hashtable seenKey ())

                            lift (put $! _processStatus)
                            put $! Timeline
                                { _history = newHistory
                                , _historySet = HashSet.insert historyKey _historySet
                                , _propertyStatus = newPropertyStatus
                                , _inputHistory = newInputHistory
                                }

                            loop rest

            _successfulBranches <- handler
                (Logic.runLogicT
                    (State.evalStateT
                        (State.evalStateT action uninitializedProcessStatus)
                        uninitializedTimeline
                    )
                    (\_ m -> do
                        !n <- m
                        return (n + 1)
                    )
                    (return (0 :: Natural))
                )

            _visitedStates <- HashTable.foldM (\n _ -> return (n + 1)) 0 hashtable

            _redundantStates <- IORef.readIORef redundantStatesReference

            let stats = Statistics
                    { _redundantStates
                    , _successfulBranches
                    , _visitedStates
                    }

            Monad.when statistics do
                putDoc (prettyValue (toJSON stats))
          where
            uninitializedProcessStatus =
                error "Internal error - Uninitialized process status"

            uninitializedTimeline =
                error "Internal error - Uninitialized timeline"

            handler :: IO a -> IO a
            handler
                | debug     = Exception.handle display
                | otherwise = id
              where
                display exception = do
                    putDoc (prettyModelException exception)

                    Exception.throwIO (Exit.ExitFailure 1)

-- | The input to a temporal `Property`
data Input global label = Input
    { _state :: !global
      -- ^ The current (global) state
    , _label :: !label
      -- ^ The current label
    } deriving stock (Generic, Show)
      deriving anyclass (ToJSON)

-- | A lens for accessing the global state of an `Input`
state :: Lens' (Input global label) global
state k (Input a b) = fmap (\a' -> Input a' b) (k a)
{-# INLINABLE state #-}

-- | A lens for accessing the label of an `Input`
label :: Lens' (Input global label) label
label k (Input a b) = fmap (\b' -> Input a b') (k b)
{-# INLINABLE label #-}

-- | Used internally to detect cycles
data Seen label processStatus propertyStatus = Seen
    { _label          :: !label
    , _processStatus  :: !processStatus
    , _propertyStatus :: !propertyStatus
    } deriving stock (Eq, Generic)
      deriving anyclass (Hashable)

data Statistics = Statistics
    { _successfulBranches :: Natural
    , _visitedStates      :: Natural
    , _redundantStates    :: Natural
    } deriving stock (Generic)
      deriving anyclass (ToJSON)
