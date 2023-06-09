{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeApplications    #-}

module PabContract(
    Contracts(..)
    ) where

import           Data.Aeson                          (FromJSON (..), ToJSON (..))                                                      
import qualified Data.OpenApi                        as OpenApi
import           GHC.Generics                        (Generic)
import           Prettyprinter                       (Pretty (..), viaShow)
import           OffChain
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Prelude                             hiding (init)


-- setup contracts that are used by the PAB
data Contracts = UseContract
                      deriving (Eq, Ord, Show, Generic)
                      deriving anyclass OpenApi.ToSchema
                      deriving anyclass (FromJSON, ToJSON)

instance Pretty Contracts where
    pretty = viaShow
 

instance Builtin.HasDefinitions Contracts where
    getDefinitions = [ UseContract ]
    getSchema =  \case
        UseContract     -> Builtin.endpointsToSchemas @NFTSchema   
   
    getContract = \case
        UseContract     -> Builtin.SomeBuiltin endpoints
     