{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module OnChain 
    (
      curSymbol
    , policy
    ) where

import           Ledger.Address                     as Address (PaymentPubKeyHash(..))
import           Ledger                             hiding (mint, singleton)
import qualified Ledger.Typed.Scripts               as Scripts
import           Ledger.Value                       as Value
import qualified PlutusTx
import           PlutusTx.Prelude                   hiding (unless)
import           Types
import           Utils


{-# INLINABLE mkPolicy #-}
mkPolicy :: MintPolicyParams -> MintPolicyRedeemer -> ScriptContext -> Bool
mkPolicy params (MintPolicyRedeemer polarity) ctx = 

    case polarity of
        True ->    traceIfFalse "UTxO not consumed"   hasUTxO
                && traceIfFalse "wrong amount minted" checkMintedAmount
                && traceIfFalse "invalid admin signature" signedByAdmin
                && traceIfFalse "invalid NFT meta data" validNFTParams
                
        False ->   traceIfFalse "wrong amount burned" checkBurnedAmount
                && traceIfFalse "invalid NFT meta data" validNFTParams


  where
    oref :: TxOutRef
    oref = mpOref params

    tn :: Value.TokenName
    tn = mpTokenName params

    info :: TxInfo
    info = scriptContextTxInfo ctx  

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    -- for now this is a single signature, with future plans to make this multi-sig
    signedByAdmin :: Bool
    signedByAdmin =  txSignedBy info $ Address.unPaymentPubKeyHash (mpAdminPkh params)

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

    checkBurnedAmount :: Bool
    checkBurnedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == (-1)
        _               -> False

    validNFTParams :: Bool
    validNFTParams = tn == (Value.TokenName $ sha2_256 tn'')
        where
            tn'' =  (mpAddress params) <> 
                    (integerToBS $ mpLat params) <> 
                    (integerToBS $ mpLong params) <> 
                    (mpCategory params) <> 
                    (mpMethod params) <> 
                    (integerToBS $ mpCO2Qty params)
           

policy :: MintPolicyParams -> Scripts.MintingPolicy
policy mpParams = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \mpParams' -> Scripts.wrapMintingPolicy $ mkPolicy mpParams' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode mpParams


curSymbol :: MintPolicyParams -> CurrencySymbol
curSymbol mpParams = scriptCurrencySymbol $ policy mpParams 


