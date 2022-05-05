{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

{-| This example is from the "Example: Arbitrage" section of the "Learn TLA+"
    guide

> ---- MODULE market ----
> EXTENDS Integers
> CONSTANTS Item, MaxPrice, Vendor, MaxActions
>
> I == Item
> V == Vendor
> P == 1..MaxPrice
>
> ValidMarkets == [V \X I -> [buy : P, sell : P]]
>
> (* --algorithm market
> variables market \in ValidMarkets, 
>           backpack = {}, \* items we have
>           actions = 0,
>           profit = 0; 
>
> begin
>   Act:
>     while actions < MaxActions do
>       either
>         Buy:
>           with v \in V, i \in Item \ backpack do
>           profit := profit - market[<<v, i>>].sell;
>           backpack := backpack \union {i};
>           end with;
>       or
>         Sell:
>           with v \in V, i \in backpack do
>             profit := profit + market[<<v, i>>].buy;
>             backpack := backpack \ {i};
>           end with;
>       end either;
>       Loop:
>         actions := actions + 1;
>     end while;
> end algorithm; *)
>
> \* Translation
>
> NoArbitrage == profit <= 0
> ====

-}
module HasCal.Test.Market where

import Data.Set (Set)
import Prelude hiding ((.))
import HasCal
import Test.Tasty (TestTree)

import qualified Control.Monad as Monad
import qualified Data.Set as Set
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.ExpectedFailure as Failure

data Item = Ore | Sheep | Brick
    deriving
        ( Bounded
        , Enum
        , Eq
        , Generic
        , Hashable
        , Ord
        , Show
        , ToJSON
        , ToJSONKey
        , Universe
        )

data Vendor = Alice
    deriving (Bounded, Enum, Eq, Generic, Hashable, Show, ToJSON, Universe)

data Offer = Offer { _buy :: Int, _sell :: Int }
    deriving (Eq, Generic, Hashable, Show, ToJSON)

data Global = Global
    { _market   :: HashMap (Vendor, Item) Offer
    , _trades   :: HashMap [Item] Item
    , _backpack :: Set Item
    , _profit   :: Int
    } deriving (Eq, Generic, Hashable, Show, ToJSON)

data Label = Act | Buy Int | Sell Int | Trade Int
    deriving (Eq, Generic, Hashable, Show, ToJSON)

makeLenses ''Offer
makeLenses ''Global

arbitrage :: Int -> Int -> IO ()
arbitrage maxPrice maxActions = do
    let _I = universe @Item
    let _V = universe @Vendor
    let _P = [ 1 .. maxPrice ]

    model defaultModel
        { startingGlobals = do
            let _domain = do
                    v <- _V
                    i <- _I
                    return (v, i)

            let _range = do
                    _buy  <- _P
                    _sell <- _P
                    return Offer{..}

            _market <- _domain --> _range

            let maxBuy  = maximum (fmap _buy _market)
            let minSell = minimum (fmap _sell _market)
            Monad.guard (maxBuy <= minSell)

            let _backpack = Set.empty

            let _profit = 0

            _trades <- subset _I --> _I

            return Global{..}

        , coroutine = Coroutine
            { startingLabel = Act

            , startingLocals = pure ()

            , process = Monad.forM_ [ 1 .. maxActions ] \n ->
                (   do  yield (Buy n)
                        _backpack <- use (global.backpack)
                        v <- with _V
                        i <- with (Set.toList (Set.difference (Set.fromList _I) _backpack))
                        Just loss <- preuse (global.market.ix (v, i).sell)
                        global.profit -= loss
                        global.backpack %= Set.insert i
                <|> do  yield (Sell n)
                        _backpack <- use (global.backpack)
                        v <- with _V
                        i <- with (Set.toList _backpack)
                        Just gain <- preuse (global.market.ix (v, i).buy)
                        global.profit += gain
                        global.backpack %= Set.delete i
                <|> do  yield (Trade n)
                        _backpack <- use (global.backpack)
                        _trades <- use (global.trades)
                        itemsLost <- with (Set.toList (Set.intersection (Set.fromList (subset (Set.toList _backpack))) (Set.fromList (domain _trades))))
                        Just itemGained <- preuse (global.trades.ix itemsLost)
                        global.backpack %= Set.insert itemGained . (`Set.difference` (Set.fromList itemsLost))
                )
            }

        , property = always . viewing (state . profit . to (<= 0))
        }

test_market :: TestTree
test_market =
    Failure.expectFailBecause "The original example has a deliberate failure" do
        HUnit.testCase "Market" do
            arbitrage 6 5
