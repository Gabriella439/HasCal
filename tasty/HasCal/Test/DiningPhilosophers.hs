{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-| This example is from the "Processes" section of the "Learn TLA+" guide

> EXTENDS Integers, Sequences, TLC, FiniteSets
> CONSTANTS NumPhilosophers, NULL
> ASSUME NumPhilosophers > 0
> NP == NumPhilosophers
>
> (* --algorithm dining_philosophers
>
> variables forks = [fork \in 1..NP |-> NULL]
>
> define
> LeftFork(p) == p
> RightFork(p) == IF p = NP THEN 1 ELSE p + 1
>
> HeldForks(p) ==
>   { x \in {LeftFork(p), RightFork(p)}: forks[x] = p}
>
> AvailableForks(p) ==
>   { x \in {LeftFork(p), RightFork(p)}: forks[x] = NULL}
>
> end define;
> process philosopher \in 1..NP
> variables hungry = TRUE;
> begin P:
>   while hungry do
>     with fork \in AvailableForks(self) do
>       forks[fork] := self;
>     end with;
>     Eat:
>       if Cardinality(HeldForks(self)) = 2 then
>         hungry := FALSE;
>         forks[LeftFork(self)] := NULL ||
>         forks[RightFork(self)] := NULL;
>       end if;
>   end while;
> end process;
> end algorithm; *)

-}

module HasCal.Test.DiningPhilosophers where

import HasCal
import Prelude hiding ((.))
import Test.Tasty (TestTree)

import qualified Control.Monad as Monad
import qualified Test.Tasty.ExpectedFailure as Failure
import qualified Test.Tasty.HUnit as HUnit

type Fork = Int

type Philosopher = Int

data Global = Global { _forks :: HashMap Fork (Maybe Philosopher) }
    deriving (Eq, Generic, Hashable, ToJSON, Show)

data Local = Local { _hungry :: Bool }
    deriving (Eq, Generic, Hashable, ToJSON, Show)

data Labels = P | Eat
    deriving (Eq, Generic, Hashable, ToJSON, Show)

makeLenses ''Global
makeLenses ''Local

diningPhilosophers :: Int -> IO ()
diningPhilosophers numPhilosophers = do
    let leftFork :: Philosopher -> Fork
        leftFork p = p

    let rightFork :: Philosopher -> Fork
        rightFork p = if p == numPhilosophers then 1 else p + 1

    let coroutine self = Coroutine
            { startingLabel = P

            , startingLocals = do
                let _hungry = True
                return Local{..}

            , process = do
                while (use (local.hungry)) do
                    oldForks <- use (global.forks)

                    let availableForks :: Philosopher -> [Fork]
                        availableForks p = do
                            x <- [ leftFork p, rightFork p ]
                            Monad.guard (oldForks ^? ix x == Just Nothing)
                            return x

                    fork <- with (availableForks self)

                    global.forks.ix fork .= Just self

                    yield Eat

                    newForks <- use (global.forks)

                    let heldForks :: Philosopher -> [Fork]
                        heldForks p = do
                            x <- [ leftFork p, rightFork p ]
                            Monad.guard (newForks ^? ix x == Just (Just p))
                            return x

                    Monad.when (length (heldForks self) == 2) do
                        local.hungry .= False
                        global.forks.ix (leftFork  self) .= Nothing
                        global.forks.ix (rightFork self) .= Nothing
            }

    model defaultModel
        { startingGlobals = do
            let _forks = [ 1 .. numPhilosophers ] |-> \_fork -> Nothing
            return Global{..}

        , coroutine = traverse coroutine [ 1 .. numPhilosophers ]

        , property = pure True
        }

test_diningPhilosophers :: TestTree
test_diningPhilosophers =
    Failure.expectFailBecause "The original example has a deliberate deadlock" do
        HUnit.testCase "Dining philosophers" do
            diningPhilosophers 2
