{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This example is taken from section 2.6 of "A PlusCal User's Manual"
module HasCal.Test.FastMutex where

import Control.Monad (when)
import Data.Traversable (for)
import HasCal
import Numeric.Natural (Natural)
import Test.Tasty (TestTree)

import qualified Control.Monad    as Monad
import qualified Test.Tasty.HUnit as HUnit

data Global = Global{ _x :: Int, _y :: Int, _b :: HashMap Int Bool }
    deriving (Eq, Generic, Hashable, Show)

data Label
    = Default
    | NonCriticalSection
    | Start
    | CriticalSection
    | Line Natural
    deriving (Eq, Generic, Hashable, Show)

instance Pretty Label  where pretty = unsafeViaShow
instance Pretty Global where pretty = unsafeViaShow

makeLenses ''Global

fastMutex :: Int -> IO ()
fastMutex n = model defaultModel
    { debug = True

    , termination = False

    , startingGlobals = do
        let _x = 0
        let _y = 0
        let _b = [ 1.. n ] |-> \_i -> False
        return Global{..}

    , coroutine = for [ 1 .. n ] proc

    , property =
        let predicate (_, labels) =
                length (filter (== CriticalSection) labels) <= 1
        in  arr predicate
    }
  where
    proc :: Int -> Coroutine Global Label
    proc self = Coroutine
        { startingLabel = Default

        , startingLocals = pure ()

        , process = ncs
        }
      where
        ncs = do
            yield CriticalSection
            start

        start :: Process Global () Label a
        start = do
            yield Start
            global.b.ix(self) .= True

            yield (Line 1)
            global.x .= self

            yield (Line 2)
            y0 <- use (global.y)
            when (y0 /= 0) do
                yield (Line 3)
                global.b.ix(self) .= False

                yield (Line 4)
                y1 <- use (global.y)
                await (y1 == 0)
                start

            yield (Line 5)
            global.y .= self

            yield (Line 6)
            x0 <- use (global.x)
            when (x0 /= self) do
                yield (Line 7)
                global.b.ix(self) .= False

                yield (Line 8)
                Monad.forM_ [ 1 .. n ] \j -> do
                    Just bool <- preuse (global.b.ix(j))
                    await (not bool)

                yield (Line 9)
                y1 <- use (global.y)
                when (y1 /= self) do
                    yield (Line 10)
                    y2 <- use (global.y)
                    await (y2 == 0)
                    start

            yield CriticalSection

            yield (Line 11)
            global.y .= 0

            yield (Line 12)
            global.b.ix(self) .= False

            ncs

test_fastMutex :: TestTree
test_fastMutex = HUnit.testCase "Fast mutex algorithm" do
    fastMutex 3
