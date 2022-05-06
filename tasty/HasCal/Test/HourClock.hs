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

data Label = Ini | Nxt
    deriving (Eq, Generic, Hashable, Show, ToJSON)

makeLenses ''Global

tick :: Int -> Int
tick hour
    | hour == 12 = 1
    | otherwise  = hour + 1

test_hourClock :: TestTree
test_hourClock = HUnit.testCase "Hour clock" do
    model defaultModel
        { termination = False

        , startingGlobals = do
            _hr <- [1 .. 12]
            return Global{..}

        , coroutine = Coroutine
            { startingLabel  = Ini
            , startingLocals = pure ()
            , process = Monad.forever do
                yield Nxt
                global.hr %= tick
            }

        , property = always . viewing (state . hr . to (`elem` [ 1 .. 12 ]))
        }
