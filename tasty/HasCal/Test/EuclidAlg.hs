{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-| This example is taken from sections 2.0 through 2.4 of "A PlusCal User's
    Manual"
-}

module HasCal.Test.EuclidAlg where

import Control.Monad (when)
import HasCal
import Prelude hiding (gcd, print)
import Test.Tasty (TestTree)

import qualified Prelude
import qualified Test.Tasty.HUnit as HUnit

data Global = Global { _u :: Int, _v :: Int }
    deriving (Eq, Generic, Hashable, Show)

makeLenses ''Global

instance Pretty Global where
    pretty = unsafeViaShow

initialU :: Int
initialU = 24

euclidAlg :: Int -> IO ()
euclidAlg n = do
    let coroutine = Coroutine
            { startingLabel = ()

            , startingLocals = pure ()

            , process = do
                initialV <- use (global.v)
                while (do u_ <- use (global.u); return (u_ /= 0)) do
                    tempU <- use (global.u)
                    tempV <- use (global.v)
                    when (tempU < tempV) do
                        global.u .= tempU
                        global.v .= tempV
                    newV <- use (global.v)
                    global.u -= newV
                finalV <- use (global.v)
                assert (Just finalV == gcd initialU initialV)
                assert (finalV == Prelude.gcd initialU initialV)
            }

    let property = pure True

    let initial = do
            _v <- [ 1 .. n ]
            let _u = initialU
            return Global{..}

    model defaultOptions{ debug = True } coroutine property initial

gcd :: Int -> Int -> Maybe Int
gcd x y =
    choose [ 1 .. x ] \i ->
            x `mod` i == 0
        &&  y `mod` i == 0
        &&  forall_ [ 1 .. x ] \j ->
                    x `mod` j == 0
                &&  y `mod` j == 0
                ==> i >= j

test_euclidAlg :: TestTree
test_euclidAlg = HUnit.testCase "Euclid's algorithm" do
    euclidAlg 4
