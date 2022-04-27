{-| HourClock
    [example](https://github.com/tlaplus/Examples/blob/master/specifications/SpecifyingSystems/HourClock/HourClock.tla)
    from figure 2.1 on page 20 in Lamport's *Specifying Systems* book:

>        VARIABLE hr
>
>        HCini == hr \in (1 .. 12)
>        HCnxt == hr' = IF hr # 12 THEN hr + 1 ELSE 1
>        HC    == HCini /\ [][HCnxt]_hr

-}

{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

module HasCal.Test.HourClock where

import HasCal
import Prelude hiding ((.))
import Test.Tasty (TestTree)

import qualified Test.Tasty.HUnit as HUnit
import qualified Control.Monad as Monad

data Global = Global { _hr :: Int }
    deriving (Eq, Generic, Hashable, Show, ToJSON)

makeLenses ''Global

data Label = Ini | Nxt
    deriving (Eq, Generic, Hashable, Show, ToJSON)

hcIni :: Process Global () Label ()
hcIni = return ()
   -- We don't need to do anything here, because `_hr` is already initialised in
   -- `startingGlobals` below,

hcNxt :: Process Global () Label ()
hcNxt = do
    yield Nxt
    h <- use (global.hr)
    global.hr .= if h /= 12 then h + 1 else 1

hc :: Process Global () Label ()
hc = do
    hcIni
    Monad.forever hcNxt

test_hourClock :: TestTree
test_hourClock = HUnit.testCase "Hour clock" do
    model defaultModel
        { debug = True

        , termination = False

        , startingGlobals = do
            _hr <- [1 .. 12]
            return Global{..}

        , coroutine = Coroutine
            { startingLabel  = Ini
            , startingLocals = pure ()
            , process        = hc
            }

        , property = always . (arr predicate /\ liveness)
        }
        where
            predicate :: (Global, Label) -> Bool
            predicate (Global _hr, _label) = _hr `elem` [1 .. 12]

            liveness :: Property (Global, Label) Bool
            liveness = eventually . label Nxt

            label :: Eq label => label -> Property (global, label) Bool
            label x = arr ((== x) . snd)
