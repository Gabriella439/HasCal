{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-| This example is from the "Example: Rate Limiting" section of the "Learn
    TLA+" guide

> ---- MODULE api ----
> EXTENDS Integers, TLC
> (* --algorithm api
> variables made_calls = 0, max_calls \in 5..10, reserved_calls = 0;
>
>
> macro make_calls(n) begin
>   made_calls := made_calls + n;
>   assert made_calls <= max_calls;
> end macro;
>
>
> macro reserve(n) begin
>   await made_calls + reserved_calls + n <= max_calls;
>   reserved_calls := reserved_calls + n;
> end macro
>
> process reset_limit = -1
> begin
>   Reset:
>     while TRUE do
>       made_calls := 0;
>     end while
> end process
>
> process get_collection = 0
> begin
>   GCGetCalls:
>     reserve(1);
>   Request:
>     make_calls(1);
>     reserved_calls := reserved_calls - 1;
>     either goto GCGetCalls
>     or skip
>     end either;
> end process;
>
> process get_put \in 1..3
> begin
>   GPGetCalls:
>     reserve(2); 
>   Call:
>     with c \in {1, 2} do
>       make_calls(c)
>     end with;
>     reserved_calls := reserved_calls - 2;
> end process;
>
> end algorithm; *)

-}

module HasCal.Test.API where

import HasCal
import Prelude hiding ((.))
import Test.Tasty (TestTree)

import qualified Test.Tasty.HUnit as HUnit

data Global = Global
    { _made_calls     :: Int
    , _max_calls      :: Int
    , _reserved_calls :: Int
    } deriving (Eq, Generic, Hashable, Show, ToJSON)

data ResetLimitLabel = ResetBegin | Reset
    deriving (Eq, Generic, Hashable, Show, ToJSON)

data GetCollectionLabel = GetCollectionBegin | GCGetCalls | Request
    deriving (Eq, Generic, Hashable, Show, ToJSON)

data GetPutLabel = GetPutBegin | GPGetCalls | Call
    deriving (Eq, Generic, Hashable, Show, ToJSON)

data Labels = Labels
    { _reset_limit    :: ResetLimitLabel
    , _get_collection :: GetCollectionLabel
    , _get_put        :: GetPutLabel
    } deriving (Eq, Generic, Hashable, Show, ToJSON)

makeLenses ''Global

reserve :: Int -> Process Global local label ()
reserve n = do
    Global{..} <- use global

    await (_made_calls + _reserved_calls + n <= _max_calls)

    global.reserved_calls += n

make_calls :: (Show local, ToJSON local) => Int -> Process Global local label ()
make_calls n = do
    global.made_calls += n

    Global{..} <- use global
    assert (_made_calls <= _max_calls)

test_api :: TestTree
test_api = HUnit.testCase "API" do
    let reset_limit = Coroutine
            { startingLabel = ResetBegin

            , startingLocals = pure ()

            , process = do
                while (pure True) do
                    yield Reset
                    global.made_calls .= 0
            }

    let get_collection = Coroutine
            { startingLabel = GetCollectionBegin

            , startingLocals = pure ()

            , process = do
                let gcGetCalls = do
                        yield GCGetCalls

                        reserve 1

                        yield Request

                        make_calls 1

                        global.reserved_calls -= 1

                        gcGetCalls <|> skip

                gcGetCalls
            }

    let get_put = Coroutine
            { startingLabel = GetPutBegin

            , startingLocals = pure ()

            , process = do
                yield GPGetCalls

                reserve 2

                yield Call

                c <- with [ 1, 2 ]

                make_calls c

                global.reserved_calls -= 2
            }

    model defaultModel
        { startingGlobals = do
            let _made_calls  = 0

            _max_calls <-  [ 5 .. 10 ]

            let _reserved_calls = 0

            return Global{..}

        , coroutine = do
            _reset_limit    <- reset_limit
            _get_collection <- get_collection
            _get_put        <- get_put
            return Labels{..}

        , property = pure True

        , termination = False
        }
