{-| This is based on the [AsynchInterface example](https://github.com/tlaplus/Examples/blob/master/specifications/SpecifyingSystems/AsynchronousInterface/AsynchInterface.tla)
    from figure 3.1 on page 27 in Lamport's \"Specifying Systems\" book:

    > ------------------ MODULE AsynchInterface ---------------------
    > EXTENDS Naturals
    > CONSTANT  Data
    > VARIABLES val, rdy, ack
    >
    > TypeInvariant == /\ val \in Data
    >                  /\ rdy \in {0, 1}
    >                  /\ ack \in {0, 1}
    > ---------------------------------------------------------------
    > Init == /\ val \in Data
    >         /\ rdy \in {0, 1}
    >         /\ ack = rdy
    >
    > Send == /\ rdy = ack
    >         /\ val' \in Data
    >         /\ rdy' = 1 - rdy
    >         /\ UNCHANGED ack
    >
    > Rcv  == /\ rdy # ack
    >         /\ ack' = 1 - ack
    >         /\ UNCHANGED <<val, rdy>>
    >
    > Next == Send \/ Rcv
    >
    > Spec == Init /\ [][Next]_<<val, rdy, ack>>
    > ---------------------------------------------------------------
    > THEOREM Spec => []TypeInvariant
    > ===============================================================
-}

{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

module HasCal.Test.AsyncInterface where

import HasCal
import Prelude hiding ((.))
import Test.Tasty (TestTree)

import qualified Test.Tasty.HUnit as HUnit
import qualified Control.Monad as Monad

data Global = Global { _chan :: Chan }
    deriving (Eq, Generic, Hashable, Show, ToJSON)

data Chan = Chan { _val :: Data , _rdy :: Bool , _ack :: Bool }
    deriving (Eq, Generic, Hashable, Show, ToJSON)

data Label = Init | Send | Rcv deriving (Eq, Generic, Hashable, Show, ToJSON)

data Data = D1 | D2 deriving (Eq, Generic, Hashable, Show, ToJSON, Universe)

makeLenses ''Global
makeLenses ''Chan

test_asyncInterface :: TestTree
test_asyncInterface = HUnit.testCase "Async interface" do
    model defaultModel
        { termination = False

        , startingGlobals = do
            _val <- universe
            _rdy <- universe
            let _ack = _rdy
            let _chan = Chan{..}
            return Global{..}

        , coroutine = Coroutine
            { startingLabel  = Init
            , startingLocals = pure ()
            , process        = do
                let send = do
                        Chan{..} <- use (global.chan)
                        await (_rdy == _ack)
                        yield Send
                        global.chan.val <~ with universe
                        global.chan.rdy %= not

                let rcv = do
                        Chan{..} <- use (global.chan)
                        await (_rdy /= _ack)
                        yield Rcv
                        global.chan.ack %= not

                Monad.forever (send <|> rcv)
            }

        , property = true
        }
