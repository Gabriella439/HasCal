{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{-| This example is from the "Tuples And Structures" section of the "Learn TLA+"
    guide

> ---- MODULE hanoi ----
> EXTENDS TLC, Sequences, Integers
>
> (* --algorithm hanoi
> variables tower = <<<<1, 2, 3>>, <<>>, <<>>>>, 
>
> define 
>   D == DOMAIN tower
> end define;
>
> begin
> while TRUE do
>   assert tower[3] /= <<1, 2, 3>>;
>   with from \in {x \in D : tower[x] /= <<>>},
>        to \in {
>                 y \in D : 
>                   \/ tower[y] = <<>>
>                   \/ Head(tower[from]) < Head(tower[y])
>               } 
>   do
>     tower[from] := Tail(tower[from]) ||
>     tower[to] := <<Head(tower[from])>> \o tower[to];
>   end with;
> end while;
> end algorithm; *)
> ====

-}

module HasCal.Test.Hanoi where

import Control.Monad (forever)
import HasCal hiding (to)
import Prelude hiding ((.))
import Test.Tasty (TestTree)

import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.ExpectedFailure as Failure

data Label = A | B deriving (Eq, Generic, Hashable, Show)

instance Pretty Label where pretty = unsafeViaShow

test_hanoi :: TestTree
test_hanoi =
    Failure.expectFailBecause "The original example intentionally fails" do
        HUnit.testCase "Hanoi" do
            model defaultModel
                { termination = False

                , startingGlobals = do
                    let board :: [[Int]]
                        board = [ [ 1, 2, 3 ], [ ], [ ] ]

                    pure board

                , coroutine = Coroutine
                    { startingLabel = A

                    , startingLocals = pure ()

                    , process = forever do
                        yield B

                        _position2 <- use (global.ix 2)
                        assert (_position2 /= [ 1, 2, 3 ])

                        from <- with [ 0 .. 2 ]
                        to   <- with [ 0 .. 2 ]

                        src <- use (global.ix from)
                        dst <- use (global.ix to  )

                        case (src, dst) of
                            (s : rc, []) -> do
                                global.ix from .= rc
                                global.ix to   .= [ s ]
                            (s : rc, d : st) | s < d -> do
                                global.ix from .= rc
                                global.ix to   .= s : d : st
                            _ -> do
                                empty
                    }

                , property = pure True
                }
