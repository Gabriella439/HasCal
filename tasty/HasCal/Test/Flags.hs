{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-| This example is from the "Behaviors" section of the "Learn TLA+" guide

> ---- MODULE flags ----
> EXTENDS TLC, Integers
> (* --algorithm flags
> variables f1 \in BOOLEAN, f2 \in BOOLEAN, f3 \in BOOLEAN
> begin
>   while TRUE do
>     with f \in {1, 2, 3} do
>       if f = 1 then
>         either
>           f1 := TRUE;
>         or
>           f1 := FALSE;
>         end either;
>       elsif f = 2 then
>         either
>           f2 := TRUE;
>         or
>           f2 := FALSE;
>         end either;
>       else
>         either
>           f3 := TRUE;
>         or
>           f3 := FALSE;
>         end either;
>       end if;
>     end with;
>   end while;
> end algorithm; *)
>
> ====

-}
module HasCal.Test.Flags where

import Control.Monad (forever)
import HasCal
import Test.Tasty (TestTree)

import qualified Test.Tasty.HUnit as HUnit

data Global = Global{ _f1 :: Bool, _f2 :: Bool, _f3 :: Bool }
    deriving (Eq, Generic, Hashable, Show)

makeLenses ''Global

data Label = A | B
    deriving (Eq, Generic, Hashable, Show)

instance Pretty Global where pretty = unsafeViaShow
instance Pretty Label  where pretty = unsafeViaShow

test_flags :: TestTree
test_flags = HUnit.testCase "Flags" do
    let initial = do
            _f1 <- universe
            _f2 <- universe
            _f3 <- universe
            return Global{..}

    let coroutines = Begin A (pure ()) do
            forever do
                yield B
                f    <- with [ f1, f2, f3 ]
                bool <- with universe
                global.f .= bool

    let property = pure True

    model defaultOptions{ debug = True, termination = False } coroutines property initial