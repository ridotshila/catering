

# 📘 Tutorial: Building a Catering Escrow Smart Contract on Cardano


## 🧭 Overview

This tutorial shows how to create and compile a **Catering Escrow smart contract** on Cardano using **Plutus V2**.

The contract handles **payments for catering events**, distributing funds to the **caterer** and **suppliers** based on the final headcount, while allowing **refunds** if necessary.

You will:

1. Define the escrow datum and actions
2. Implement the validator logic
3. Wrap and compile the contract
4. Generate a `.plutus` file using Cabal

---

## ⚙️ 1. Smart Contract Structure

File: **`Catering.hs`**

### Language Extensions & Imports

```haskell
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

import Prelude (IO, print, putStrLn)
import qualified Prelude as H

import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless, ($))
import Plutus.V2.Ledger.Api
    ( BuiltinData, ScriptContext(..), TxInfo(..), TxOut(..),
      Validator, mkValidatorScript,
      PubKeyHash, Address(..), Credential(..),
      adaSymbol, adaToken, txOutValue, txOutAddress, txInfoOutputs )
import Plutus.V2.Ledger.Contexts (txSignedBy)
import qualified Plutus.V1.Ledger.Value as Value

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise (serialise)
import Cardano.Api (writeFileTextEnvelope)
import Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV2)
```

---

## 🧾 2. Datum & Redeemer Types

### Supplier

```haskell
data Supplier = Supplier
    { sPkh   :: PubKeyHash
    , sShare :: Integer  -- parts per 1000
    }
PlutusTx.unstableMakeIsData ''Supplier
```

Each supplier has a **public key hash** and a **share of the payment** in per-mille.

---

### Event Escrow

```haskell
data EventEscrow = EventEscrow
    { client            :: PubKeyHash
    , caterer           :: PubKeyHash
    , expectedHeadcount :: Integer
    , pricePerHead      :: Integer   -- in lovelace
    , suppliers         :: [Supplier]
    }
PlutusTx.unstableMakeIsData ''EventEscrow
```

Stores the **client**, **caterer**, **expected headcount**, **price per head**, and a **list of suppliers**.

---

### Actions

```haskell
data Action = Lock | Finalize Integer | Refund
PlutusTx.unstableMakeIsData ''Action
```

* **Lock** – lock the funds in the contract
* **Finalize n** – distribute funds according to the actual headcount
* **Refund** – refund the client

---

## 🧠 3. Helper Functions

### Address for Public Key

```haskell
{-# INLINABLE pubKeyHashAddress #-}
pubKeyHashAddress :: PubKeyHash -> Address
pubKeyHashAddress pkh = Address (PubKeyCredential pkh) Nothing
```

### Compute value paid to a PubKey

```haskell
{-# INLINABLE valuePaidTo #-}
valuePaidTo :: TxInfo -> PubKeyHash -> Integer
valuePaidTo info pkh =
    let outs = txInfoOutputs info
        matches = [ Value.valueOf (txOutValue o) adaSymbol adaToken
                  | o <- outs, txOutAddress o == pubKeyHashAddress pkh ]
    in foldr (+) 0 matches
```

### Compute supplier due

```haskell
{-# INLINABLE supplierDue #-}
supplierDue :: Integer -> Supplier -> Integer
supplierDue totalDue (Supplier _ sharePerMille) =
    (totalDue * sharePerMille) `divide` 1000
```

---

## 🧩 4. Core Validator Logic

```haskell
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
```

### 🔍 Logic

* **Lock** – always valid
* **Refund** – requires client signature
* **Finalize** – checks:

1. Compute total payment (`headcount * price per head`)
2. Compute each supplier’s share
3. Ensure caterer and suppliers receive sufficient funds

---

## 🔒 5. Wrap & Compile

```haskell
{-# INLINABLE wrapped #-}
wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped d r c =
    let esc = unsafeFromBuiltinData d :: EventEscrow
        act = unsafeFromBuiltinData r :: Action
        ctx = unsafeFromBuiltinData c :: ScriptContext
    in if mkCateringValidator esc act ctx then () else traceError "CateringEscrow: validation failed"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrapped ||])
```

---

## 💾 6. Writing the `.plutus` File

```haskell
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
```

---

## 📦 7. Cabal Executable Section

```cabal
executable catering-exe
  main-is:            Catering.hs
  hs-source-dirs:     tests
  build-depends:
      base >=4.14 && <5,
      plutus-ledger-api,
      plutus-tx,
      plutus-tx-plugin,
      bytestring,
      serialise,
      text,
      containers,
      cardano-api,
      cardano-ledger-core,
      cardano-ledger-shelley
  default-language:   Haskell2010
```

---

## 🧪 8. Build & Run

1. Update Cabal:

```bash
cabal update
```

2. Build the executable:

```bash
cabal build catering-exe
```

3. Run to generate the validator:

```bash
cabal run catering-exe
```

✅ Output:

```
✅ Catering validator written to: catering-validator.plutus
```

---

## 🧩 9. Example Scenario

| Step | Action                        | Result                             |
| ---- | ----------------------------- | ---------------------------------- |
| 1    | Lock funds                    | Contract accepts ADA               |
| 2    | Refund                        | Only client can trigger            |
| 3    | Finalize 50 guests            | Caterer & suppliers paid correctly |
| 4    | Finalize insufficient payment | Transaction fails                  |

---

## ✅ Summary

* Built a **Catering Escrow validator** in Plutus V2
* Handles **locking**, **finalizing payments**, and **refunds**
* Calculates **supplier shares** and ensures proper **fund distribution**
* Compiles and serializes a **`.plutus` file** using Cabal

