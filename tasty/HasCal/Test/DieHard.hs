{-| The following is the "Die Hard"
    [example](https://github.com/tlaplus/Examples/blob/master/specifications/DieHard/DieHard.tla)
    from Lamport's [*TLA+ Video
    Course*](http://lamport.azurewebsites.net/video/videos.html).

>        VARIABLES small, big
>
>        TypeOK = /\ small \in 0..3
>                 /\ big   \in 0..5
>
>        Init == /\ big   = 0
>                /\ small = 0
>
>        FillSmall == /\ small' = 3
>                     /\ big'   = big
>
>        FillBig == /\ big'   = 5
>                   /\ small' = small
>
>        EmptySmall == /\ small' = 0
>                      /\ big'   = big
>
>        EmptyBig == /\ big'   = 0
>                    /\ small' = small
>
>        SmallToBig == /\ big'   = Min(big + small, 5)
>                      /\ small' = small - (big' - big)
>
>        BigToSmall == /\ small' = Min(big + small, 3)
>                      /\ big'   = big - (small' - small)

    This module ports the above TLA+ code to HasCal.
-}

{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module HasCal.Test.DieHard where

import HasCal
import Prelude hiding (either, init, (.))
import Test.Tasty (TestTree)

import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.ExpectedFailure as Failure
import qualified Control.Monad as Monad

data Global = Global { _small :: Int, _big :: Int }
  deriving (Eq, Generic, Hashable, Show, ToJSON)

makeLenses ''Global

data Label = Init | FillSmall | FillBig | EmptySmall | EmptyBig | SmallToBig | BigToSmall
    deriving (Eq, Generic, Hashable, Show, ToJSON)

typeOK :: Global -> Bool
typeOK Global {..} = _small `elem` [0..3] && _big `elem` [0..5]

init, fillSmall, fillBig, emptySmall, emptyBig, smallToBig, bigToSmall :: Process Global () Label ()

init = Monad.forever next

fillSmall = do
  yield FillSmall
  global.small .= 3

fillBig = do
  yield FillBig
  global.big .= 5

emptySmall = do
  yield EmptySmall
  global.small .= 0

emptyBig = do
  yield EmptyBig
  global.big .= 0

smallToBig = do
  yield SmallToBig
  _small <- use (global.small)
  _big   <- use (global.big)
  _big'  <- global.big <.= min (_big + _small) 5
  global.small .= _small - (_big' - _big)

bigToSmall = do
  yield BigToSmall
  _small  <- use (global.small)
  _big    <- use (global.big)
  _small' <- global.small <.= min (_big + _small) 3
  global.big .= _big - (_small' - _small)

next :: Process Global () Label ()
next =
    either
      [ fillSmall
      , fillBig
      , emptySmall
      , emptyBig
      , smallToBig
      , bigToSmall
      ]

test_dieHard :: TestTree
test_dieHard =
    Failure.expectFailBecause "The solution to the puzzle is the counterexample" do
        HUnit.testCase "Die Hard" do
          model defaultModel
              { debug = True

              , termination = False

              , startingGlobals = do
                  let _small = 0
                      _big   = 0
                  return Global{..}

              , coroutine = Coroutine
                  { startingLabel  = Init
                  , startingLocals = pure ()
                  , process        = init
                  }

              , property = always . arr predicate
              }
              where
                predicate :: (Global, Label) -> Bool
                predicate (g, _label) = g^.big /= 4
