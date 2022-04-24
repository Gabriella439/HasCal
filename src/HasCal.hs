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

{-| This documentation assumes that you are already familiar with PlusCal and
    TLA+.  If not, then you will probably want to read at least one of the
    following resources first:

    * <https://learntla.com/introduction/ Learn TLA+>
    * <https://lamport.azurewebsites.net/tla/p-manual.pdf A PlusCal User's Manual>

    This package implements PlusCal as an embedded domain-specific language
    (i.e. eDSL) in Haskell.  In other words, this does not compile to any
    intermediate or external language; the whole thing is implemented in pure
    Haskell.

    "HasCal.Coroutine" provides the API for creating and model-checking
    concurrent `Coroutine`s

    "HasCal.Property" provides the API for creating and checking temporal
    `Property`s
-}

module HasCal
    ( -- * Internal re-exports
      module HasCal.Coroutine
    , module HasCal.Property

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
    ) where

import Control.Arrow (Arrow(..))
import Control.Applicative (Alternative(..))
import Control.Category (Category(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (ToJSON(..), ToJSONKey(..))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Profunctor (Profunctor(..))
import GHC.Generics (Generic)
import HasCal.Property
import HasCal.Coroutine
import Lens.Micro.Platform

-- TODO: Associate local state with process names
