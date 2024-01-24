{-# OPTIONS_GHC -Wno-unused-imports #-}

module Spec.StakingSpec (unitTest) where

import Plutarch.Context (
  UTXO,
  address,
  buildMinting',
  input,
  mint,
  output,
  signedWith,
  timeRange,
  txId,
  withInlineDatum,
  withMinting,
  withRefIndex,
  withRefTxId,
  withValue,
 )
import Plutarch.Test.Precompiled (Expectation (Success), testEvalCase, tryFromPTerm)
import PlutusLedgerApi.V1 (POSIXTimeRange, Value)
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V2 (
  Address (..),
  BuiltinByteString,
  Credential (..),
  CurrencySymbol,
  Interval (Interval),
  POSIXTime (..),
  PubKeyHash (..),
  ScriptContext,
  StakingCredential (..),
  TokenName,
  TxOutRef (..),
  singleton,
 )

import PlutusTx qualified
import Test.Tasty (TestTree)

import Mint.Standard (mkStakingNodeMPW)
import Plutarch.Prelude
import Types.Constants (exactAdaCommitment, poriginNodeTN)
import Types.StakingSet (PStakingConfig (..), StakingConfig (..), StakingNodeAction (..), StakingNodeKey (..), StakingSetNode (..))

import Conversions (pconvert)
import Plutarch.Api.V2 (PMintingPolicy)
import Types.StakingSet (PStakingNodeAction (..))

nodeCS :: CurrencySymbol
nodeCS = "746fa3ba2daded6ab9ccc1e39d3835aa1dfcb9b5a54acc2ebe6b79a4"

originNodeTN :: TokenName
originNodeTN = plift poriginNodeTN

mintOriginNode :: Value
mintOriginNode = singleton nodeCS originNodeTN 1

stakeCS :: CurrencySymbol
stakeCS = "746fa3ba2daded6ab9ccc1e39d3835aa1dfcb9b5a54acc2ebe6b7000"

stakeTN :: TokenName
stakeTN = "MIN"

minimumStake :: Integer
minimumStake = 1_000

initUTxO :: TxOutRef
initUTxO = TxOutRef "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d" 1

freezeStake :: POSIXTime
freezeStake = POSIXTime 96_400_000

endStaking :: POSIXTime
endStaking = POSIXTime 196_400_000

penaltyAddress :: Address
penaltyAddress =
  let cred = "b1f2f20a8781a3ba967d8c7b5068d21d799e809dcce22f651679d661"
      stakeCred = PubKeyCredential "52563c5410bff6a0d43ccebb7c37e1f69f5eb260552521adff33b9c2"
   in Address (ScriptCredential cred) (Just (StakingHash stakeCred))

stakingConfig :: Term s PStakingConfig
stakingConfig =
  pconstant
    ( StakingConfig
        { initUTxO
        , freezeStake
        , endStaking
        , penaltyAddress
        , stakeCS
        , stakeTN
        , minimumStake
        }
    )

initAction :: StakingNodeAction
initAction = Init

initUTXO :: UTXO
initUTXO =
  mconcat
    [ withValue (singleton "" "" 4_000_000)
    , withRefTxId "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d"
    , withRefIndex 1
    ]

headAddr :: Address
headAddr =
  let cred = "65c4b5e51c3c58c15af080106e8ce05b6efbb475aa5e5c5ca9372a45"
      stakeCred = PubKeyCredential "b055a795895b15d9af25acb752ac89c78524acfa387acb626c7e1bc8"
   in Address (ScriptCredential cred) (Just (StakingHash stakeCred))

headUTXO :: UTXO
headUTXO =
  mconcat
    [ address headAddr
    , withValue (singleton "" "" 4_000_000 <> mintOriginNode <> mkStakeValue minimumStake)
    , withInlineDatum $
        MkSetNode
          { key = Empty
          , next = Empty
          }
    ]

initScriptContext :: ScriptContext
initScriptContext =
  buildMinting' $
    mconcat
      [ txId "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d"
      , input initUTXO
      , output headUTXO
      , mint mintOriginNode
      , withMinting nodeCS
      ]

deinitAction :: StakingNodeAction
deinitAction = Deinit

burnOriginNode :: Value
burnOriginNode = singleton nodeCS originNodeTN (-1)

deinitScriptContext :: ScriptContext
deinitScriptContext =
  buildMinting' $
    mconcat
      [ input headUTXO
      , mint burnOriginNode
      , withMinting nodeCS
      ]

user1PKH :: BuiltinByteString
user1PKH = "a65ca58a4e9c755fa830173d2a5caed458ac0c73f97db7faae2e7e3b"

coveringTokenName :: TokenName
coveringTokenName = "FSNa65ca58a4e9c755fa830173d2a5caed458ac0c73f97db7faae2e7e3b"

user2PKH :: BuiltinByteString
user2PKH = "e18d73505be6420225ed2a42c8e975e4c6f9148ab38e951ea2572e54"

insertTokenName :: TokenName
insertTokenName = "FSNe18d73505be6420225ed2a42c8e975e4c6f9148ab38e951ea2572e54"

coveringMintedValue :: Value
coveringMintedValue = singleton nodeCS coveringTokenName 1

mkStakeValue :: Integer -> Value
mkStakeValue = singleton stakeCS stakeTN

adaCommitment :: Integer
adaCommitment = plift exactAdaCommitment

coveringNodeValue :: Value
coveringNodeValue = singleton "" "" adaCommitment <> coveringMintedValue <> mkStakeValue 1500

insertMintedValue :: Value
insertMintedValue = singleton nodeCS insertTokenName 1

coveringNode :: StakingSetNode
coveringNode =
  MkSetNode
    { key = Key user1PKH
    , next = Empty
    }

coveringUTXO :: UTXO
coveringUTXO =
  mconcat
    [ address headAddr
    , withValue coveringNodeValue
    , withInlineDatum coveringNode
    ]

outputPrevNode :: StakingSetNode
outputPrevNode =
  MkSetNode
    { key = coveringNode.key
    , next = Key user2PKH
    }

outputPrevNodeUTXO :: UTXO
outputPrevNodeUTXO =
  mconcat
    [ address headAddr
    , withValue coveringNodeValue
    , withInlineDatum outputPrevNode
    ]

outputNode :: StakingSetNode
outputNode =
  MkSetNode
    { key = Key user2PKH
    , next = coveringNode.next
    }

outputNodeUTXO :: UTXO
outputNodeUTXO =
  mconcat
    [ address headAddr
    , withValue (singleton "" "" adaCommitment <> insertMintedValue <> mkStakeValue 2000)
    , withInlineDatum outputNode
    ]

insertAction :: StakingNodeAction
insertAction = Insert (PubKeyHash user2PKH) coveringNode

insertValidTimeRange :: POSIXTimeRange
insertValidTimeRange = Interval (Interval.lowerBound 1_000) (Interval.strictUpperBound 20_000)

insertScriptContext :: ScriptContext
insertScriptContext =
  buildMinting' $
    mconcat
      [ input coveringUTXO
      , output outputPrevNodeUTXO
      , output outputNodeUTXO
      , mint insertMintedValue
      , withMinting nodeCS
      , timeRange insertValidTimeRange
      , signedWith (PubKeyHash user2PKH)
      ]

removeAction :: StakingNodeAction
removeAction = Remove (PubKeyHash user2PKH) coveringNode

rmCoveringNode :: StakingSetNode
rmCoveringNode =
  MkSetNode
    { key = Key user1PKH
    , next = Key user2PKH
    }

inputPrevNodeUTXO :: UTXO
inputPrevNodeUTXO =
  mconcat
    [ address headAddr
    , withValue coveringNodeValue
    , withInlineDatum rmCoveringNode
    ]

removeNode :: StakingSetNode
removeNode =
  MkSetNode
    { key = Key user2PKH
    , next = Empty
    }

removeNodeUTXO :: UTXO
removeNodeUTXO =
  mconcat
    [ address headAddr
    , withValue (singleton "" "" adaCommitment <> insertMintedValue <> mkStakeValue 2000)
    , withInlineDatum removeNode
    ]

rmOutputNode :: StakingSetNode
rmOutputNode =
  MkSetNode
    { key = rmCoveringNode.key
    , next = removeNode.next
    }

rmOutputNodeUTXO :: UTXO
rmOutputNodeUTXO =
  mconcat
    [ address headAddr
    , withValue coveringNodeValue
    , withInlineDatum rmOutputNode
    ]

removeValidTimeRange :: POSIXTimeRange
removeValidTimeRange = Interval (Interval.lowerBound 1_000) (Interval.strictUpperBound 20_000)

removeTokenName :: TokenName
removeTokenName = "FSNe18d73505be6420225ed2a42c8e975e4c6f9148ab38e951ea2572e54"

removeMintedValue :: Value
removeMintedValue = singleton nodeCS removeTokenName (-1)

removeScriptContext :: ScriptContext
removeScriptContext =
  buildMinting' $
    mconcat
      [ input inputPrevNodeUTXO
      , input removeNodeUTXO
      , output rmOutputNodeUTXO
      , mint removeMintedValue
      , withMinting nodeCS
      , timeRange removeValidTimeRange
      , signedWith (PubKeyHash user2PKH)
      ]

removeValidLateTimeRange :: POSIXTimeRange
removeValidLateTimeRange = Interval (Interval.lowerBound 1_000) (Interval.strictUpperBound 100_000_000)

penaltyOutputUTXO :: UTXO
penaltyOutputUTXO =
  mconcat
    [ address penaltyAddress
    , withValue (mkStakeValue 500 <> singleton "" "" 2_000_000)
    ]

lateRemoveScriptContext :: ScriptContext
lateRemoveScriptContext =
  buildMinting' $
    mconcat
      [ input inputPrevNodeUTXO
      , input removeNodeUTXO
      , output rmOutputNodeUTXO
      , output penaltyOutputUTXO
      , mint removeMintedValue
      , withMinting nodeCS
      , timeRange removeValidLateTimeRange
      , signedWith (PubKeyHash user2PKH)
      ]

unitTest :: TestTree
unitTest = tryFromPTerm "Staking Unit Test" (mkStakingNodeMPW # stakingConfig) $ do
  testEvalCase
    "Pass - Init Staking"
    Success
    [ PlutusTx.toData initAction
    , PlutusTx.toData initScriptContext
    ]
  testEvalCase
    "Pass - Deinit Staking"
    Success
    [ PlutusTx.toData deinitAction
    , PlutusTx.toData deinitScriptContext
    ]
  testEvalCase
    "Pass - Insert Staking"
    Success
    [ PlutusTx.toData insertAction
    , PlutusTx.toData insertScriptContext
    ]
  testEvalCase
    "Pass - Remove Staking"
    Success
    [ PlutusTx.toData removeAction
    , PlutusTx.toData removeScriptContext
    ]
  testEvalCase
    "Pass - Late Remove Staking"
    Success
    [ PlutusTx.toData removeAction
    , PlutusTx.toData lateRemoveScriptContext
    ]
