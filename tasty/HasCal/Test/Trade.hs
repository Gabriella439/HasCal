{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

{-| This example is from the "Introduction" section of the "Learn TLA+" guide

> People == {"alice", "bob"}
> Items == {"ore", "sheep", "brick"}
> (* --algorithm trade
> variable owner_of \in [Items -> People]
>
> process giveitem \in 1..3 \* up to three possible trades made
> variables item \in Items, 
>           owner = owner_of[item], 
>           to \in People,
>           origin_of_trade \in People
> begin Give:
>     if origin_of_trade = owner then 
>         owner_of[item] := to;
>     end if;
> end process;
> end algorithm; *)

-}

module HasCal.Test.Trade where

import Control.Monad (when)
import Data.Foldable (traverse_)
import HasCal hiding (to)
import Test.Tasty (TestTree)

import qualified Test.Tasty.HUnit as HUnit

data People = Alice | Bob
    deriving (Bounded, Enum, Eq, Generic, Hashable, Show, Universe)

data Items = Ore | Sheep | Brick
    deriving (Bounded, Enum, Eq, Generic, Hashable, Show, Universe)

data Global = Global
    { _owner_of :: HashMap Items People
    } deriving (Eq, Generic, Hashable, Show)

instance Pretty Global where pretty = unsafeViaShow

makeLenses ''Global

test_trade :: TestTree
test_trade = HUnit.testCase "Trade" do
    model defaultOptions coroutine property initial
  where
    initial = do
        _owner_of <- universe --> universe
        return Global{..}

    coroutine = traverse_ giveitem [ 1 .. 3 ]

    giveitem :: Int -> Coroutine Global ()
    giveitem _ = Begin{..}
      where
        startingLabel = ()

        startingLocals = pure ()

        process = do
            item            <- with universe
            to              <- with universe
            origin_of_trade <- with universe

            Just owner <- preuse (global.owner_of.ix item)

            when (origin_of_trade == owner) do
                global.owner_of.ix item .= to

    property = pure True
