{-# LANGUAGE BlockArguments #-}

{-| This example is from the "Concurrent Invariants" section of the "Learn TLA+"
    guide

> process foo \in 1..2
> variable x \in 1..2
> begin
>   Skip:
>     skip
> end process

    … with the invariant that @x[1] + x[2] /= 4@
-}

module HasCal.Test.ConcurrentInvariant where

default (Int)

import HasCal
import Prelude hiding ((.))
import Test.Tasty (TestTree)

import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.ExpectedFailure as Failure

test_concurrentInvariant :: TestTree
test_concurrentInvariant =
    Failure.expectFailBecause "The original example intentionally fails" do
        HUnit.testCase "Concurrent invariant" do
            let foo _index = Coroutine
                    { startingLabel = ()
                    , startingLocals = pure ()
                    , process = skip
                    }

            let indices = [ 1 .. 2 ]

            model defaultModel
                { startingGlobals =
                    indices --> [ 1 .. 2 ] -- :: [ HashMap Int Int ]

                , coroutine = traverse_ foo indices

                , property =
                    let predicate (xs, _) = sum xs /= 4
                    in  always . arr predicate
                }
