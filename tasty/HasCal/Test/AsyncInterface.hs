{-| This is the "Our second specification of an asynchronous interface" example
    from page 30 in "Specifying Systems" by Lamport:

>        CONSTANT Data
>        VARIABLE chan
>        TypeInvariant == chan \in [val : Data, rdy : {0,1}, ack : {0, 1}]
>
>        Init == /\ TypeInvariant
>                /\ chan.ack = chan.rdy
>
>        Send(d) == /\ chan.rdy = chan.ack
>                   /\ chan' = [chan EXCEPT !.val = d, !.rdy = 1 - @]
>
>        Rcv == /\ chan.rdy != chan.ack
>               /\ chan' = [chan EXCPT !.ack = 1 - @]
>
>        Next == (\E d \in Data : Send(d)) \/ Rcv
>
>        Spec == Init /\ [][Next]_chan

    The following module is an attempt to translate it to HasCal.
-}

{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module HasCal.Test.AsyncInterface where

import HasCal
import Prelude hiding (either, init, (.))
import Test.Tasty (TestTree)

import qualified Test.Tasty.HUnit as HUnit
import qualified Control.Monad as Monad

data Global d = Global { _chan :: Chan d }
  deriving (Eq, Generic, Hashable, Show, ToJSON)

data Chan d = Chan
  { _val :: d
  , _rdy :: Bool
  , _ack :: Bool
  } deriving (Eq, Generic, Hashable, Show, ToJSON)

data Label d = Init | Send d | Rcv
    deriving (Eq, Generic, Hashable, Show, ToJSON)

data Data = D1 | D2
  deriving (Bounded, Enum, Eq, Generic, Hashable, Show, ToJSON, Universe)

makeLenses ''Global
makeLenses ''Chan

init :: Universe d => Process (Global d) () (Label d) ()
init = do
  global.chan.ack <~ use (global.chan.rdy)
  Monad.forever next

next :: Universe d => Process (Global d) () (Label d) ()
next =
    either
      [ existsU send
      , rcv
      ]
  where
    existsU p = either (map p universe)

send :: d -> Process (Global d) () (Label d) ()
send d = do
  _rdy <- use (global.chan.rdy)
  _ack <- use (global.chan.ack)
  await (_rdy == _ack)
  yield (Send d)
  global.chan.val .= d
  global.chan.rdy %= not

rcv :: Process (Global d) () (Label d) ()
rcv = do
  _rdy <- use (global.chan.rdy)
  _ack <- use (global.chan.ack)
  await (_rdy /= _ack)
  yield Rcv
  global.chan.ack %= not

test_asyncInterface :: TestTree
test_asyncInterface = HUnit.testCase "Async interface" do
    model defaultModel
        { debug = True

        , termination = False

        , startingGlobals = do
            _val <- universe @Data
            _rdy <- universe @Bool
            _ack <- universe @Bool
            let _chan = Chan{..}
            return Global{..}

        , coroutine = Coroutine
            { startingLabel  = Init
            , startingLocals = pure ()
            , process        = init
            }

        , property = pure True
        }
