{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Main where

import           Prelude                          (IO, FilePath, print, putStrLn, String)
import qualified Prelude                          as H

import           PlutusTx
import           PlutusTx.Prelude                hiding (Semigroup(..), unless, ($))
import           Plutus.V2.Ledger.Api
  ( BuiltinData
  , ScriptContext (..)
  , TxInfo (..)
  , TxOut (..)
  , Validator
  , mkValidatorScript
  , PubKeyHash
  , Address (..)
  , Credential (..)
  , adaSymbol
  , adaToken
  , txOutValue
  , txOutAddress
  , txInfoOutputs
  )

import           Plutus.V2.Ledger.Contexts      (txSignedBy)
import qualified Plutus.V1.Ledger.Value         as Value

import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Short           as SBS
import           Codec.Serialise                 (serialise)

import           Cardano.Api                     (writeFileTextEnvelope)
import           Cardano.Api.Shelley             (PlutusScript (..), PlutusScriptV2)

--------------------------------------------------------------------------------
-- Datum & Redeemer types
--------------------------------------------------------------------------------

-- | Supplier with pubkey and share (per-mille; e.g. 100 -> 10%)
data Supplier = Supplier
    { sPkh   :: PubKeyHash
    , sShare :: Integer      -- parts per 1000
    }
PlutusTx.unstableMakeIsData ''Supplier
-- REMOVE makeLift here

data EventEscrow = EventEscrow
    { client            :: PubKeyHash
    , caterer           :: PubKeyHash
    , expectedHeadcount :: Integer
    , pricePerHead      :: Integer   -- in lovelace
    , suppliers         :: [Supplier]
    }
PlutusTx.unstableMakeIsData ''EventEscrow
-- REMOVE makeLift here

data Action = Lock | Finalize Integer | Refund
PlutusTx.unstableMakeIsData ''Action
-- REMOVE makeLift here

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINABLE pubKeyHashAddress #-}
pubKeyHashAddress :: PubKeyHash -> Address
pubKeyHashAddress pkh = Address (PubKeyCredential pkh) Nothing

{-# INLINABLE valuePaidTo #-}
valuePaidTo :: TxInfo -> PubKeyHash -> Integer
valuePaidTo info pkh =
    let outs = txInfoOutputs info
        matches = [ Value.valueOf (txOutValue o) adaSymbol adaToken
                  | o <- outs
                  , txOutAddress o == pubKeyHashAddress pkh
                  ]
    in foldr (+) 0 matches

{-# INLINABLE supplierDue #-}
supplierDue :: Integer -> Supplier -> Integer
supplierDue totalDue (Supplier _ sharePerMille) =
    (totalDue * sharePerMille) `divide` 1000

--------------------------------------------------------------------------------
-- Core validator
--------------------------------------------------------------------------------

{-# INLINABLE mkCateringValidator #-}
mkCateringValidator :: EventEscrow -> Action -> ScriptContext -> Bool
mkCateringValidator datum action ctx =
    case action of
      Lock -> True
      Refund -> traceIfFalse "refund: client signature required" (txSignedBy info (client datum))
      Finalize actualHeadcount ->
        let totalDue = actualHeadcount * pricePerHead datum
            sDues = map (\s -> (sPkh s, supplierDue totalDue s)) (suppliers datum)
            sumSuppliers = foldr (\(_,d) acc -> acc + d) 0 sDues
            catererDue = totalDue - sumSuppliers
            paidToCaterer = valuePaidTo info (caterer datum)
            paidToSuppliersOK = foldr (\(pkh,d) acc -> acc && (valuePaidTo info pkh >= d)) True sDues
            catererOK = paidToCaterer >= catererDue
        in traceIfFalse "finalize: insufficient payment to caterer" catererOK
           && traceIfFalse "finalize: insufficient payment to one or more suppliers" paidToSuppliersOK
  where
    info = scriptContextTxInfo ctx

--------------------------------------------------------------------------------
-- Wrap & compile
--------------------------------------------------------------------------------

{-# INLINABLE wrapped #-}
wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped d r c =
    let esc = unsafeFromBuiltinData d :: EventEscrow
        act = unsafeFromBuiltinData r :: Action
        ctx = unsafeFromBuiltinData c :: ScriptContext
    in if mkCateringValidator esc act ctx
         then ()
         else traceError "CateringEscrow: validation failed"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrapped ||])

--------------------------------------------------------------------------------
-- Write validator to file
--------------------------------------------------------------------------------

saveValidator :: IO ()
saveValidator = do
    let scriptSerialised = serialise validator
        scriptShortBs    = SBS.toShort (LBS.toStrict scriptSerialised)
        plutusScript     = PlutusScriptSerialised scriptShortBs :: PlutusScript PlutusScriptV2
    r <- writeFileTextEnvelope "catering-validator.plutus" Nothing plutusScript
    case r of
      Left err -> print err
      Right () -> putStrLn "✅ Catering validator written to: catering-validator.plutus"

main :: IO ()
main = saveValidator
