{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module OffChain where

import           Control.Monad                      hiding (fmap)
import           Data.Aeson                         (FromJSON, ToJSON)
import qualified Data.Map                           as Map
import Data.Monoid                                  (Last (..))
import           Data.Text                          (Text)
import           Data.Void                          (Void)
import           GHC.Generics                       (Generic)
import           Plutus.Contract                    as Contract
import qualified PlutusTx               
import           PlutusTx.Prelude                   hiding (Semigroup(..), unless)
import           Ledger                             hiding (mint, singleton)
import           Ledger.Constraints                 as Constraints
import           Ledger.Address                     as Address (PaymentPubKeyHash(..), pubKeyHashAddress)
import           Ledger.Scripts                     as Scripts (Redeemer(..))
import           Ledger.Value                       as Value
import           Playground.Contract                as Playground (ToSchema)
import           Plutus.Contract.Request            as Request (ownPaymentPubKeyHash)
import           Prelude                            (Semigroup (..), Show (..), String)
import           Text.Printf                        (printf)
import           OnChain                            (curSymbol, policy)
import           Utils                              (integerToBS)
import           Types


data NFTParams = NFTParams
    { 
      npAddress     :: !BuiltinByteString
    , npLat         :: !Integer
    , npLong        :: !Integer
    , npCategory    :: !BuiltinByteString
    , npMethod      :: !BuiltinByteString
    , npCO2Qty      :: !Integer
    , npAdminPkh    :: !Address.PaymentPubKeyHash     
    } deriving (Generic, FromJSON, ToJSON, Show, Playground.ToSchema)


mintToken :: NFTParams -> Contract (Last MintPolicyParams) NFTSchema Text ()
mintToken np = do

    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tn = (npAddress np) <> 
                     (integerToBS $ npLat np) <> 
                     (integerToBS $ npLong np) <> 
                     (npCategory np) <> 
                     (npMethod np) <> 
                     (integerToBS $ npCO2Qty np)
                tn' = Value.TokenName $ sha2_256 tn
                red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = True  -- mint token
                     }
                mintParams = MintPolicyParams 
                    {
                      mpOref = oref
                    , mpTokenName = tn'
                    , mpAddress = npAddress np
                    , mpLat = npLat np
                    , mpLong = npLong np
                    , mpCategory = npCategory np
                    , mpMethod = npMethod np
                    , mpCO2Qty = npCO2Qty np
                    , mpAdminPkh = npAdminPkh np
                    }

            let val     = Value.singleton (curSymbol mintParams) tn' 1
                lookups = Constraints.mintingPolicy (policy mintParams) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer red val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)
            Contract.logInfo @String $ printf "NFT params %s" (show mintParams)

            tell $ Last $ Just mintParams


burnToken :: MintPolicyParams -> Contract w NFTSchema Text ()
burnToken mpParams = do
    
    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tn = mpTokenName mpParams
                red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = False -- burn token
                     }
            let val     = Value.singleton (curSymbol mpParams) tn (-1)
                lookups = Constraints.mintingPolicy (policy mpParams) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer red val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "burned %s" (show val)
            Contract.logInfo @String $ printf "Burning params %s" (show mpParams)



type NFTSchema = Endpoint "mint" NFTParams
             .\/ Endpoint "burn" MintPolicyParams

endpoints :: Contract (Last MintPolicyParams) NFTSchema Text ()
endpoints = forever $ handleError logError $ awaitPromise $ mint `select` burn
  where
    mint = endpoint @"mint" $ \(np) -> mintToken np
    burn = endpoint @"burn" $ \(mp) -> burnToken mp 


