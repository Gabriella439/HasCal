{-| This is based on the [Channel module](https://github.com/tlaplus/Examples/blob/master/specifications/SpecifyingSystems/FIFO/Channel.tla)
    from figure 3.2 on page 30 in Lamport's \"Specifying Systems\" book:

    > -------------------------- MODULE Channel -----------------------------
    > EXTENDS Naturals
    > CONSTANT Data
    > VARIABLE chan 
    > 
    > TypeInvariant  ==  chan \in [val : Data,  rdy : {0, 1},  ack : {0, 1}]
    > -----------------------------------------------------------------------
    > Init  ==  /\ TypeInvariant
    >           /\ chan.ack = chan.rdy 
    > 
    > Send(d) ==  /\ chan.rdy = chan.ack
    >             /\ chan' = [chan EXCEPT !.val = d, !.rdy = 1 - @]
    > 
    > Rcv     ==  /\ chan.rdy # chan.ack
    >             /\ chan' = [chan EXCEPT !.ack = 1 - @]
    > 
    > Next  ==  (\E d \in Data : Send(d)) \/ Rcv
    > 
    > Spec  ==  Init /\ [][Next]_chan
    > -----------------------------------------------------------------------
    > THEOREM Spec => []TypeInvariant
    > =======================================================================

    … and the [InnerFIFO module](https://github.com/tlaplus/Examples/blob/master/specifications/SpecifyingSystems/FIFO/InnerFIFO.tla)
    from figure 4.1 on page 38 in Lamport's \"Specifying Systems\" book:

    > ---------------------------- MODULE InnerFIFO -------------------------------
    > EXTENDS Naturals, Sequences
    > CONSTANT Message
    > VARIABLES in, out, q
    > InChan  == INSTANCE Channel WITH Data <- Message, chan <- in
    > OutChan == INSTANCE Channel WITH Data <- Message, chan <- out
    > -----------------------------------------------------------------------------
    > Init == /\ InChan!Init
    >         /\ OutChan!Init
    >         /\ q = << >>
    >
    > TypeInvariant  ==  /\ InChan!TypeInvariant
    >                    /\ OutChan!TypeInvariant
    >                    /\ q \in Seq(Message)
    >
    > SSend(msg)  ==  /\ InChan!Send(msg) \* Send msg on channel `in'.
    >                 /\ UNCHANGED <<out, q>>
    >
    > BufRcv == /\ InChan!Rcv              \* Receive message from channel `in'.
    >           /\ q' = Append(q, in.val)  \*   and append it to tail of q.
    >           /\ UNCHANGED out
    >
    > BufSend == /\ q # << >>               \* Enabled only if q is nonempty.
    >            /\ OutChan!Send(Head(q))   \* Send Head(q) on channel `out'
    >            /\ q' = Tail(q)            \*   and remove it from q.
    >            /\ UNCHANGED in
    >
    > RRcv == /\ OutChan!Rcv          \* Receive message from channel `out'.
    >         /\ UNCHANGED <<in, q>>
    >
    > Next == \/ \E msg \in Message : SSend(msg)
    >         \/ BufRcv
    >         \/ BufSend
    >         \/ RRcv
    >
    > Spec == Init /\ [][Next]_<<in, out, q>>
    > -----------------------------------------------------------------------------
    > THEOREM Spec => []TypeInvariant
    > =============================================================================

    … and the [FIFO example](https://github.com/tlaplus/Examples/blob/master/specifications/SpecifyingSystems/FIFO/FIFO.tla)
    on page 41:

    > ------------------------ MODULE FIFO -------------------------
    > CONSTANT Message
    > VARIABLES in, out
    > Inner(q) == INSTANCE InnerFIFO 
    > Spec == \EE q : Inner(q)!Spec
    > ==============================================================

    For the @Channel@ we reuse the "HasCal.Test.AsyncInterface" module, but
    the other two modules we inline here.
-}

{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

module HasCal.Test.FIFO where

import Data.Sequence (Seq, ViewL(..), (|>))
import HasCal
import HasCal.Test.AsyncInterface (Chan, Data, val)
import Prelude hiding ((.))
import Test.Tasty (TestTree)

import qualified Control.Monad as Monad
import qualified Data.Sequence as Seq
import qualified HasCal.Test.AsyncInterface as Channel
import qualified Test.Tasty.HUnit as HUnit

data Global = Global
    { _inChannel  :: Chan
    , _outChannel :: Chan
    , _q          :: Seq Data
    } deriving (Eq, Generic, Hashable, Show, ToJSON)

data Label = Init | SSend | BufRcv | BufSend | RRcv
    deriving (Eq, Generic, Hashable, Show, ToJSON)

makeLenses ''Global

-- There's technically no need to rename `channelModel` to `inChan` / `outChan`.
-- since they're pure and instantiated identically.  We could have just used
-- `channelModel` directly in their place.  These synonyms are just to show the
-- correspondence to the TLA+ model.

inChan :: Model Chan Channel.Label
inChan = Channel.channelModel

outChan :: Model Chan Channel.Label
outChan = Channel.channelModel

test_asyncInterface :: TestTree
test_asyncInterface = HUnit.testCase "FIFO" do
    model defaultModel
        { termination = termination inChan /\ termination outChan

        , startingGlobals = do
            _inChannel  <- startingGlobals inChan
            _outChannel <- startingGlobals outChan
            let _q = mempty
            return Global{..}

        , coroutine = Coroutine
            { startingLabel = Init

            , startingLocals = pure ()

            , process = do
                let ssend = do
                        _q <- use (global.q)
                        await (Seq.length _q <= 3)
                        msg <- with universe
                        zoomProcess inChannel (Channel.send msg)

                let bufRcv = do
                        zoomProcess inChannel Channel.rcv
                        _val <- use (global.inChannel.val)
                        global.q %= (|> _val)

                let bufSend = do
                        _q <- use (global.q)

                        case Seq.viewl _q of
                            h :< t -> do
                                zoomProcess outChannel (Channel.send h)
                                global.q .= t
                            EmptyL ->
                                empty

                let rRcv = do
                         zoomProcess outChannel Channel.rcv

                Monad.forever
                    (   (do ssend  ; yield SSend  )
                    <|> (do bufRcv ; yield BufRcv )
                    <|> (do bufSend; yield BufSend)
                    <|> (do rRcv   ; yield RRcv   )
                    )
            }

        , property = true
        }
