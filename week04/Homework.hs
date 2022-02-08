{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Week04.Homework where

import Control.Monad.Freer.Extras as Extras
import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text, unpack)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator
import Wallet.Emulator.Wallet

data PayParams = PayParams
    { ppRecipient :: PaymentPubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- awaitPromise $ endpoint @"pay" return
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    Contract.handleError (\err -> Contract.logInfo $ "caught: " ++ unpack err) $ (void $ submitTx tx)
    payContract
    

payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace pp1 pp2 = do
    h <- activateContractWallet (knownWallet 1) payContract
    callEndpoint @"pay" h $ PayParams
        { ppRecipient = mockWalletPaymentPubKeyHash $ knownWallet 2
        , ppLovelace      = pp1
        }
    void $ Emulator.waitNSlots 1
    xs <- observableState h
    Extras.logInfo $ show xs
    callEndpoint @"pay" h $ PayParams
        { ppRecipient = mockWalletPaymentPubKeyHash $ knownWallet 2
        , ppLovelace      = pp2
        } 
    void $ Emulator.waitNSlots 1
    ys <- observableState h
    Extras.logInfo $ show ys

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 10_000_000 20_000_000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000_000_000 20_000_000