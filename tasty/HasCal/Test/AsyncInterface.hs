{-| This is the "Our second specification of an asynchronous interface" example
    from p. 30 in "Specifying Systems" by Lamport:

@
        CONSTANT Data
        VARIABLE chan
        TypeInvariant := chan \in [val : Data, rdy : {0,1}, ack : {0, 1}]

        Init := /\ TypeInvariant
                /\ chan.ack = chan.rdy

        Send(d) := /\ chan.rdy = chan.ack
                   /\ chan' = [chan EXCEPT !.val = d, !.rdy = 1 - @]

        Rcv := /\ chan.rdy != chan.ack
               /\ chan' = [chan EXCPT !.ack = 1 - @]

        Next := (\E d \in Data : Send(d)) \/ Rcv

        Spec := Init /\ [][Next]_chan
@

    The following module is an attempt to translate it to HasCal.
-}

{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module HasCal.Test.AsyncInterface where

import HasCal
import Prelude hiding (either, init)
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import Test.Tasty (TestTree)
import qualified Test.Tasty.QuickCheck as TastyQC

data Global d = Global { _chan :: Chan d }
  deriving (Eq, Generic, Hashable, Show)

instance Show d => Pretty (Global d) where
  pretty = unsafeViaShow

data Chan d = Chan
  { _val :: d
  , _rdy :: Bool
  , _ack :: Bool
  }
  deriving (Eq, Generic, Hashable, Show)

makeLenses ''Chan
makeLenses ''Global

instance Show d => Pretty (Chan d) where
  pretty = unsafeViaShow

data Label = Init | Send | Rcv
  deriving (Eq, Generic, Hashable, Show)

instance Pretty Label where
  pretty = unsafeViaShow

data Data = D1 | D2
  deriving (Bounded, Enum, Eq, Generic, Hashable, Show, Universe)

instance QC.Arbitrary Data where
  arbitrary = QC.elements universe

asyncInterface :: Data -> Bool -> Bool -> IO ()
asyncInterface =
  model defaultOptions{ debug = True, termination = False }
        coroutine property do
    _val <- fromList universe
    _rdy <- fromList universe
    _ack <- fromList universe
    let _chan = Chan{..}
    return Global{..}
  where
    coroutine :: Coroutine (Global Data) Label
    coroutine = Begin{..}
      where
        startingLabel = Init

        startingLocal = ()

        process = init

    property :: Property (Global Data, Label) Bool
    property = arr predicate
      where
        predicate (_global, _label) = True

init :: Universe d => Process (Global d) () Label ()
init = do
  yield Init
  global.chan.ack <~ use (global.chan.rdy)
  next

next :: Universe d => Process (Global d) () Label ()
next =
  forever do
    either
      [ existsU send
      , rcv
      ]
  where
    existsU p = either (map p universe)

send :: d -> Process (Global d) () Label ()
send d = do
  yield Send
  await =<< (==) <$> use (global.chan.rdy) <*> use (global.chan.ack)
  global.chan.val .= d
  global.chan.rdy %= not

rcv :: Process (Global d) () Label ()
rcv = do
  yield Rcv
  await =<< (/=) <$> use (global.chan.rdy) <*> use (global.chan.ack)
  global.chan.ack %= not

test_asyncInterface :: TestTree
test_asyncInterface = TastyQC.testProperty "Async interface" $
  QC.forAll QC.arbitrary $ \(d, rdy0, ack0) ->
    QC.monadicIO (QC.run (asyncInterface d rdy0 ack0))
