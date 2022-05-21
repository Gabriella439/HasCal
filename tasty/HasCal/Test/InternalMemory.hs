{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-| This is based on the [InternalMemory module](https://github.com/tlaplus/Examples/blob/master/specifications/SpecifyingSystems/CachingMemory/InternalMemory.tla)
    from figure 5.2 on page 52 in Lamport's \"Specifying Systems\" book:

    > ------------------ MODULE InternalMemory ---------------------
    > EXTENDS MemoryInterface
    > VARIABLES mem, ctl, buf
    > --------------------------------------------------------------
    > IInit == /\ mem \in [Adr->Val]
    >          /\ ctl = [p \in Proc |-> "rdy"] 
    >          /\ buf = [p \in Proc |-> NoVal] 
    >          /\ memInt \in InitMemInt
    > 
    > TypeInvariant == 
    >   /\ mem \in [Adr->Val]
    >   /\ ctl \in [Proc -> {"rdy", "busy","done"}] 
    >   /\ buf \in [Proc -> MReq \cup Val \cup {NoVal}]
    > 
    > Req(p) == /\ ctl[p] = "rdy" 
    >           /\ \E req \in  MReq :
    >                 /\ Send(p, req, memInt, memInt') 
    >                 /\ buf' = [buf EXCEPT ![p] = req]
    >                 /\ ctl' = [ctl EXCEPT ![p] = "busy"]
    >           /\ UNCHANGED mem 
    > 
    > Do(p) == 
    >   /\ ctl[p] = "busy" 
    >   /\ mem' = IF buf[p].op = "Wr"
    >               THEN [mem EXCEPT ![buf[p].adr] = buf[p].val] 
    >               ELSE mem 
    >   /\ buf' = [buf EXCEPT ![p] = IF buf[p].op = "Wr"
    >                                   THEN NoVal
    >                                   ELSE mem[buf[p].adr]]
    >   /\ ctl' = [ctl EXCEPT ![p] = "done"] 
    >   /\ UNCHANGED memInt 
    > 
    > Rsp(p) == /\ ctl[p] = "done"
    >           /\ Reply(p, buf[p], memInt, memInt')
    >           /\ ctl' = [ctl EXCEPT ![p]= "rdy"]
    >           /\ UNCHANGED <<mem, buf>> 
    > 
    > INext == \E p \in Proc: Req(p) \/ Do(p) \/ Rsp(p) 
    > 
    > ISpec == IInit  /\  [][INext]_<<memInt, mem, ctl, buf>>
    > --------------------------------------------------------------
    > THEOREM ISpec => []TypeInvariant
    > ==============================================================

    … and the [MemoryInterface module](https://raw.githubusercontent.com/tlaplus/Examples/master/specifications/SpecifyingSystems/CachingMemory/MemoryInterface.tla)
    from figure 5.1 on page 48 in Lamport's \"Specifying Systems\" book:

    > -------------------------- MODULE MemoryInterface ---------------------------
    > VARIABLE memInt
    > CONSTANTS  Send(_, _, _, _),
    >            Reply(_, _, _, _),
    >            InitMemInt, 
    >            Proc,  
    >            Adr,  
    >            Val
    > 
    > (***************************************************************************)
    > (* We comment out the assumption because TLC cannot handle unbounded       *)
    > (* quantifiers.                                                            *)
    > (***************************************************************************)
    > \* ASSUME \A p, d, miOld, miNew : 
    > \*         /\ Send(p,d,miOld,miNew)  \in BOOLEAN
    > \*         /\ Reply(p,d,miOld,miNew) \in BOOLEAN  
    > 
    > -----------------------------------------------------------------------------
    > MReq == [op : {"Rd"}, adr: Adr] 
    >           \cup [op : {"Wr"}, adr: Adr, val : Val]
    > 
    > NoVal == CHOOSE v : v \notin Val
    > =============================================================================
-}
module HasCal.Test.InternalMemory where

import HasCal
import Prelude hiding ((.))
import Test.Tasty (TestTree)

import qualified Control.Monad as Monad
import qualified Test.Tasty.HUnit as HUnit

data Adr = A1 | A2 | A3
    deriving (Eq, Generic, Hashable, Show, ToJSON, ToJSONKey, Universe)

data Proc = P1 | P2
    deriving (Eq, Generic, Hashable, Show, ToJSONKey, ToJSON, Universe)

data Val = V1 | V2
    deriving (Eq, Generic, Hashable, Show, ToJSON, Universe)

data Local = Local
    { _mem :: HashMap Adr Val
    , _ctl :: HashMap Proc Ctl
    , _buf :: HashMap Proc Buf
    , _memInt :: (Proc, Buf)
    } deriving (Eq, Generic, Hashable, Show, ToJSON)

data Ctl = Rdy | Busy | Done
    deriving (Eq, Generic, Hashable, Show, ToJSON)

data Buf
    = Rd { _adr :: Adr }
    | Wr { _adr :: Adr, _val :: Val }
    | Val Val
    | NoVal
    deriving (Eq, Generic, Hashable, Show, ToJSON, Universe)

data Label = Init | Req | Do | Rsp
    deriving (Eq, Generic, Hashable, Show, ToJSON)

makeLenses ''Local

-- https://github.com/tlaplus/Examples/blob/master/specifications/SpecifyingSystems/Liveness/MCInternalMemory.tla#L36-L37
send :: Proc -> Buf -> Process () Local Label ()
send p d = local.memInt .= (p, d)

reply :: Proc -> Buf -> Process () Local Label ()
reply p d = local.memInt .= (p, d)

test_internalMemory :: TestTree
test_internalMemory = HUnit.testCase "Internal memory" do
    model defaultModel
        { termination = False

        , startingGlobals = pure ()

        , coroutine = Coroutine
            { startingLabel = Init

            , startingLocals = do
                _mem <- universe --> universe
                let _ctl = universe |-> \_p -> Rdy
                let _buf = universe |-> \_p -> NoVal
                let _memInt = (P1, NoVal)
                return Local{..}

            , process = Monad.forever do
                p <- with universe

                Just _ctl <- preuse (local.ctl.ix p)

                case _ctl of
                    Rdy -> do
                        yield Req

                        req <- with universe

                        send p req

                        local.buf.ix p .= req

                        local.ctl.ix p .= Busy

                    Busy -> do
                        yield Do

                        Just _buf <- preuse (local.buf.ix p)

                        case _buf of
                            Wr{..} -> do
                                local.mem.ix _adr .= _val

                                local.buf.ix p .= NoVal

                            Rd{..} -> do
                                Just _val <- preuse (local.mem.ix _adr)

                                local.buf.ix p .= Val _val

                            _ -> do
                                skip

                        local.ctl.ix p .= Done

                    Done -> do
                        yield Rsp

                        Just _buf <- preuse (local.buf.ix p)

                        reply p _buf

                        local.ctl.ix p .= Rdy
            }

        , property = true
        }
