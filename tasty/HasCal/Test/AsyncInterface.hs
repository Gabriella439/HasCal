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

data Chan = Chan { _val :: Data, _rdy :: Bool, _ack :: Bool }
    deriving (Eq, Generic, Hashable, Show, ToJSON)

data Label = Init | Send | Rcv deriving (Eq, Generic, Hashable, Show, ToJSON)

data Data = D1 | D2 | D3 deriving (Eq, Generic, Hashable, Show, ToJSON, Universe)

makeLenses ''Chan

-- `send` and `rcv` are factored out into top-level utilities so that they can
-- be reused by the "HasCal.Test.FIFO" example

send :: Data -> Process Chan local label ()
send message = do
    Chan{..} <- use global
    await (_rdy == _ack)
    global.val .= message
    global.rdy %= not

rcv :: Process Chan local label ()
rcv = do
    Chan{..} <- use global
    await (_rdy /= _ack)
    global.ack %= not

channelModel :: Model Chan Label
channelModel = defaultModel
    { termination = False

    , startingGlobals = do
            _val <- universe
            _rdy <- universe
            let _ack = _rdy
            return Chan{..}

    , coroutine = Coroutine
        { startingLabel = Init

        , startingLocals = pure ()

        , process = Monad.forever
            (   (do msg <- with universe
                    send msg
                    yield Send
                )
            <|> (do rcv
                    yield Rcv
                )
            )
        }

    , property = true
    }

test_asyncInterface :: TestTree
test_asyncInterface = HUnit.testCase "Async interface" do
    model channelModel
