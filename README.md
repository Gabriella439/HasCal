# HasCal

HasCal embeds PlusCal in Haskell as an ordinary Haskell package.  Everything is
implemented entirely in Haskell, including the model checker.

For example, this PlusCal code from the
[Learn TLA+ book](https://learntla.com/introduction/example/):

```
---- MODULE Transfer ----
EXTENDS Naturals, TLC

(* --algorithm transfer
variables alice_account = 10, bob_account = 10,
          account_total = alice_account + bob_account;

process Transfer \in 1..2
  variable money \in 1..20;
begin
Transfer:
  if alice_account >= money then
    A: alice_account := alice_account - money;
       bob_account := bob_account + money;
end if;
C: assert alice_account >= 0;
end process

end algorithm *)

MoneyInvariant == alice_account + bob_account = account_total
```

… corresponds to this Haskell code:

```haskell
{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (when)
import Prelude hiding ((.))
import HasCal

data Global = Global
    { _alice_account :: Int
    , _bob_account   :: Int
    , _account_total :: Int
    } deriving (Eq, Generic, Hashable, Show, ToJSON)

data Local = Local { _money :: Int }
    deriving (Eq, Generic, Hashable, Show, ToJSON)

data Label = Transfer | A | C deriving (Eq, Generic, Hashable, Show, ToJSON)

makeLenses ''Global
makeLenses ''Local

main :: IO ()
main = do
    let transfer _ = Coroutine
        { startingLabel = Transfer

        , startingLocals = do
            _money <- [ 1 .. 20 ]
            return Local{..}

        , process = do
            _money <- use (local.money)

            alice_old <- use (global.alice_account)

            when (alice_old >= _money) do
                yield A
                global.alice_account -= _money
                global.bob_account   += _money

            yield C
            alice_new <- use (global.alice_account)
            assert (alice_new >= 0)
        }

    model defaultModel
        { startingGlobals = do
            let _alice_account = 10
            let _bob_account   = 10
            let _account_total = _alice_account + _bob_account
            return Global{..}

        , coroutine = traverse transfer [ 1 .. 2 ]

        , property =
            let predicate (Global{..}, _) =
                    _alice_account + _bob_account == _account_total
            in  always . arr predicate
        }
```

You can find more example code in the [test suite](./tasty/HasCal/Test)
