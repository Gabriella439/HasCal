{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | This documentation assumes that you are already familiar with PlusCal and
-- TLA+.  If not, then you will probably want to read at least one of the
-- following resources first:
--
-- * <https://learntla.com/introduction/ Learn TLA+>
-- * <https://lamport.azurewebsites.net/tla/p-manual.pdf A PlusCal User's Manual>
--
-- This package implements PlusCal as an embedded domain-specific language
-- (i.e. eDSL) in Haskell.  In other words, this does not compile to any
-- intermediate or external language; the whole thing is implemented in pure
-- Haskell.
--
-- The package is organized into the following modules, which you should study
-- in order if you want to learn more:
--
-- "HasCal.Coroutine" provides the domain-specific language for creating and
-- and model-checking concurrent `Coroutine`s
--
-- "HasCal.Property" provides the the domain-specific language for creating and
-- checking temporal `Property`s
--
-- "HasCal.Expression" provides assorted utilities for non-temporal expressions
--
-- … and you can import this module if you want an \"all-in-one\" import for
-- convenience.
--
-- As a complete example, you can translate this PlusCal program from the
-- \"Learn TLA+\" guide:
--
-- > ---- MODULE Transfer ----
-- > EXTENDS Naturals, TLC
-- >
-- > (* --algorithm transfer
-- > variables alice_account = 10, bob_account = 10,
-- >           account_total = alice_account + bob_account;
-- >               
-- > process Transfer \in 1..2
-- >   variable money \in 1..20;
-- > begin             
-- > Transfer:         
-- >   if alice_account >= money then
-- >     A: alice_account := alice_account - money;
-- >        bob_account := bob_account + money;
-- > end if;       
-- > C: assert alice_account >= 0;
-- > end process               
-- >                   
-- > end algorithm *)
-- > 
-- > MoneyInvariant == alice_account + bob_account = account_total
-- >   
-- > ====  
-- 
-- … into a Haskell program that is also model-checked within Haskell:
-- 
-- @
-- {-# LANGUAGE BlockArguments  #-}
-- {-# LANGUAGE DeriveAnyClass  #-}
-- {-# LANGUAGE DeriveGeneric   #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE TemplateHaskell #-}
--
-- import "Control.Monad" (`Control.Monad.when`)
-- import "Prelude" hiding ((`Prelude..`))
-- import "HasCal"
--
-- data Global = Global
--     { _alice_account :: `Int`
--     , _bob_account   :: `Int`
--     , _account_total :: `Int`
--     } deriving (`Eq`, `Generic`, `Hashable`, `Show`, `ToJSON`)
--
-- data Local = Local { _money :: `Int` }
--     deriving (`Eq`, `Generic`, `Hashable`, `Show`, `ToJSON`)
--
-- data Label = Transfer | A | C deriving (`Eq`, `Generic`, `Hashable`, `Show`, `ToJSON`)
--
-- `Control.TH.makeLenses` ''Global
-- `Control.TH.makeLenses` ''Local
--
-- main :: `IO` ()
-- main = do
--     let transfer _ = `Coroutine`
--         { startingLabel = Transfer
--
--         , startingLocals = do
--             _money <- [ 1 .. 20 ]
--             `return` Local{..}
--
--         , process = do
--             _money <- `use` (local.money)
--
--             alice_old <- `use` (global.alice_account)
--
--             when (alice_old `>=` _money) do
--                 `yield` A
--                 global.alice_account `-=` _money
--                 global.bob_account   `+=` _money
--
--             `yield` C
--             alice_new <- `use` (global.alice_account)
--             `assert` (alice_new `>=` 0)
--         }
--
--     `model` `defaultModel`
--         { startingGlobals = do
--             let _alice_account = 10
--             let _bob_account   = 10
--             let _account_total = _alice_account `+` _bob_account
--             `return` Global{..}
--
--         , coroutine = `traverse` transfer [ 1 .. 2 ]
--
--         , property =
--             let predicate (Global{..}, _) =
--                     _alice_account `+` _bob_account `==` _account_total
--             in  `always` . `arr` predicate
--         }
-- @

module HasCal
    ( -- * Internal re-exports
      module HasCal.Expression
    , module HasCal.Property
    , module HasCal.Coroutine

      -- * External re-exports
    , module Lens.Micro.Platform
    , Generic
    , HashMap
    , Alternative(..)
    , Hashable
    , MonadIO(..)
    , ToJSON(..)
    , ToJSONKey(..)
    , Category(..)
    , Arrow(..)
    , Profunctor(..)
    , for_
    , traverse_
    ) where

import Control.Arrow (Arrow(..))
import Control.Applicative (Alternative(..))
import Control.Category (Category(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (ToJSON(..), ToJSONKey(..))
import Data.Foldable (for_, traverse_)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Profunctor (Profunctor(..))
import GHC.Generics (Generic)
import HasCal.Expression
import HasCal.Property
import HasCal.Coroutine
import Lens.Micro.Platform

-- TODO: Associate local state with process names
