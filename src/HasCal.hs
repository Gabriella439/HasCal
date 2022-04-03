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
    ( Process(..)
    , Step(..)
    , Coroutine(..)
    , Status(..)

    -- * Model checking
    , check

    -- * Lenses
    , global
    , local

    -- * Utilities
    , yield
    , skip
    , end
    , branch
    , with
    , while
    , await
    , assert
    , debug

    -- * Ranges
    , boolean
    , (~>)
    , domain

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
import Prettyprinter (Doc, Pretty(..))

import qualified Control.Applicative as Applicative
import qualified Control.Exception.Safe as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Void as Void
import qualified List.Transformer as List
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

newtype Process label m result = Choice (m (Step label m result))
    deriving stock (Functor)

instance Monad m => Applicative (Process label m) where
    pure result = lift (pure result)

    (<*>) = Monad.ap

instance Monad m => Monad (Process label m) where
    Choice ps >>= f = Choice do
        p <- ps
        case p of
            Yield label rest -> do
                return (Yield label (rest >>= f))
            Done result -> do
                let Choice possibilities = f result
                possibilities

instance (Monad m, Alternative m) => Alternative (Process label m) where
    empty = Choice empty

    Choice psL <|> Choice psR = Choice (psL <|> psR)

instance (Monad m, Semigroup result) => Semigroup (Process label m result) where
    (<>) = liftA2 (<>)

instance (Monad m, Monoid result) => Monoid (Process label m result) where
    mempty = pure mempty

instance MonadTrans (Process label) where
    lift m = Choice (fmap Done m)

instance MonadState s m => MonadState s (Process label m) where
    get = lift get

    put s = lift (put s)

    state k = lift (state k)

instance MonadThrow m => MonadThrow (Process label m) where
    throwM e = lift (throwM e)

instance MonadIO m => MonadIO (Process label m) where
    liftIO io = lift (liftIO io)

data Step label m result
    = Yield label (Process label m result)
    | Done result
    deriving stock (Functor)

data Status global local = Status { _global :: global, _local :: local }
    deriving stock (Eq, Generic, Show)
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

global :: Lens' (Status global local) global
global k (Status g l) = fmap (\g' -> Status g' l) (k g)

local :: Lens' (Status global local) local
local k (Status g l) = fmap (\l' -> Status g l') (k l)

data Coroutine global label =
        forall local
    .   (Eq local, Hashable local, ToDocs local, Show local)
    =>  Begin
            { startingLabel :: label
            , startingLocal :: local
            , process
                :: Process label
                       (StateT (Status global local)
                           (ListT IO)
                       )
                       Void
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

yield :: Monad m => label -> Process label m ()
yield label = Choice (pure (Yield label mempty))

skip :: Monad m => Process label m ()
skip = mempty

end :: (Monad m, Alternative m) => Process label m a
end = empty

branch
    :: (Monad m, Alternative m)
    => [Process label m result] -> Process label m result
branch = Foldable.asum

with
    :: (Monad m, Alternative m, Foldable f, Functor f)
    => f result -> Process label m result
with results = Foldable.asum (fmap pure results)

while
    :: Monad m
    => Process label m Bool
    -> Process label m ()
    -> Process label m ()
while condition body = do
    bool <- condition
    Monad.when bool do
        body
        while condition body

await :: (Monad m, Alternative m) => Bool -> Process label m ()
await = Monad.guard

assert
    :: (MonadState local m, MonadThrow m, Pretty local, Show local)
    => Bool -> Process label m ()
assert True  = skip
assert False = do
    status <- get
    Exception.throw AssertionFailed{ status }

debug :: (MonadIO m, Show a) => a -> Process label m ()
debug a = liftIO (print a)

boolean :: NonEmpty Bool
boolean = False :| [ True ]

(~>)
    :: (Eq key, Hashable key)
    => NonEmpty key -> NonEmpty value -> NonEmpty (HashMap key value)
keys ~> values =
    fmap (HashMap.fromList . Foldable.toList) (traverse process keys)
  where
    process key = fmap ((,) key) values

domain :: HashMap key value -> [key]
domain = HashMap.keys

data ModelException =
          forall label status
      .   ( Show label
          , Show status
          , Pretty label
          , Pretty status
          )
      =>  Deadlock { history :: [(label, status)] }
          -- ^ NOTE: The history is stored in reverse
    |     forall local . (Pretty local, Show local)
      =>  AssertionFailed { status :: local }

instance Show ModelException where
    showsPrec _ Deadlock{ history } =
          showString "Deadlock {history = "
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
    pretty Deadlock{ history } = Pretty.group (Pretty.flatAlt long short)
      where
        short = "Deadlock detected" <> suffix
          where
            suffix = case history of
                [] ->
                    mempty

                seens  ->
                    " - History: " <> bars (map adapt (reverse seens))
                  where
                    adapt (label, status) =
                        commas [ pretty label, pretty status ]

        long = Pretty.align ("Deadlock detected" <> suffix)
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
    => NonEmpty global
    -- ^ Starting global state
    -> Coroutine global label
    -- ^
    -> IO ()
check startingGlobals Begin{ startingLabel, startingLocal, process } = do
    result <- Exception.try (List.runListT (State.evalStateT action startingStatus))

    case result of
        Left  exception ->
            Pretty.Text.putDoc (pretty (exception :: ModelException))
        Right () ->
            mempty
  where
    action = loop [] startingSet do
        startingGlobal <- with (Foldable.toList startingGlobals)

        global .= startingGlobal

        process

    startingStatus =
        Status
            { _global = NonEmpty.head startingGlobals
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
                        Exception.throw (Deadlock newHistory)
                    else do
                        let newSeen = HashSet.insert key seen

                        loop newHistory newSeen rest

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
