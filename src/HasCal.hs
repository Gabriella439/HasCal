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
-}

module HasCal
    ( -- * Re-exports
      module HasCal.Coroutine
    , module HasCal.Temporal
    , module Lens.Micro.Platform
    , Generic
    , HashMap
    , NonEmpty(..)
    , Alternative(..)
    , Hashable
    , MonadIO(..)
    , Pretty(..)
    , Pretty.unsafeViaShow
    , Monad.when
    , fromList
    ) where

import Control.Applicative (Alternative(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Exts (fromList)
import GHC.Generics (Generic)
import HasCal.Temporal
import HasCal.Coroutine
import Lens.Micro.Platform
import Prettyprinter (Pretty(..))

import qualified Control.Monad as Monad
import qualified Prettyprinter as Pretty

-- TODO: Add highlighting to pretty-printed output
-- TODO: Associate local state with process names
-- TODO: Explicitly enumerate re-exports


