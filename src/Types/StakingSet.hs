{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Types.StakingSet (
  PStakingNodeAction (..),
  StakingNodeAction (..),
  PNodeValidatorAction (..),
  PStakingConfig (..),
  StakingConfig (..),
  StakingNodeKey (..),
  PStakingLaunchConfig (..),
  PSepNodeAction (..),
  PSeparatorConfig (..),
  PStakingSetNode (..),
  StakingSetNode (..),
  PNodeKey (..),
  PNodeKeyState (..),
  isEmptySet,
  asPredecessorOf,
  asSuccessorOf,
  getNextPK,
  getCurrentPK,
  isFirstNode,
  isLastNode,
  mkNode,
  isNothing,
  validNode,
  mkBSNode,
) where

import GHC.Generics (Generic)
import Plutarch.Api.V2 (
  PAddress,
  PCurrencySymbol,
  PPOSIXTime,
  PPubKeyHash (PPubKeyHash),
  PScriptHash (..),
  PStakingCredential (..),
  PTokenName,
  PTxOutRef,
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import PlutusLedgerApi.V2 (Address, BuiltinByteString, CurrencySymbol, POSIXTime, PubKeyHash, TokenName, TxOutRef)
import PlutusTx qualified
import Types.Classes

data NodeValidatorAction
  = LinkedListAct
  | ModifyStake
  | RewardFoldAct
  deriving stock (Generic, Show)

PlutusTx.makeIsDataIndexed ''NodeValidatorAction ([('LinkedListAct, 0), ('ModifyStake, 1), ('RewardFoldAct, 2)])

data PNodeValidatorAction (s :: S)
  = PLinkedListAct (Term s (PDataRecord '[]))
  | PModifyStake (Term s (PDataRecord '[]))
  | PRewardFoldAct (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PNodeValidatorAction where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PNodeValidatorAction where
  type PLifted PNodeValidatorAction = NodeValidatorAction

deriving via
  (DerivePConstantViaData NodeValidatorAction PNodeValidatorAction)
  instance
    (PConstantDecl NodeValidatorAction)

instance PTryFrom PData (PAsData PNodeValidatorAction)
instance PTryFrom PData PNodeValidatorAction

data PStakingLaunchConfig (s :: S)
  = PStakingLaunchConfig
      ( Term
          s
          ( PDataRecord
              '[ "freezeStake" ':= PPOSIXTime
               , "globalCred" ':= PStakingCredential
               , "stakeCS" ':= PCurrencySymbol
               , "stakeTN" ':= PTokenName
               , "minimumStake" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PStakingLaunchConfig where type DPTStrat _ = PlutusTypeData

data StakingConfig = StakingConfig
  { stakingInitUTxO :: TxOutRef
  , rewardInitUTxO :: TxOutRef
  , freezeStake :: POSIXTime
  , endStaking :: POSIXTime
  , penaltyAddress :: Address
  , stakeCS :: CurrencySymbol
  , stakeTN :: TokenName
  , minimumStake :: Integer
  }

PlutusTx.makeIsDataIndexed ''StakingConfig ([('StakingConfig, 0)])

data PStakingConfig (s :: S)
  = PStakingConfig
      ( Term
          s
          ( PDataRecord
              '[ "stakingInitUTxO" ':= PTxOutRef
               , "rewardInitUTxO" ':= PTxOutRef
               , "freezeStake" ':= PPOSIXTime
               , "endStaking" ':= PPOSIXTime
               , "penaltyAddress" ':= PAddress
               , "stakeCS" ':= PCurrencySymbol
               , "stakeTN" ':= PTokenName
               , "minimumStake" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PStakingConfig where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PStakingConfig where type PLifted PStakingConfig = StakingConfig
deriving via (DerivePConstantViaData StakingConfig PStakingConfig) instance PConstantDecl StakingConfig

instance PTryFrom PData (PAsData PStakingConfig)
instance PTryFrom PData PStakingConfig

data StakingNodeKey = Key BuiltinByteString | Empty
  deriving stock (Show, Eq, Ord, Generic)
PlutusTx.makeIsDataIndexed ''StakingNodeKey ([('Key, 0), ('Empty, 1)])

data StakingSetNode = MkSetNode
  { key :: StakingNodeKey
  , next :: StakingNodeKey
  }
  deriving stock (Show, Eq, Generic)
PlutusTx.makeIsDataIndexed ''StakingSetNode ([('MkSetNode, 0)])

data SepNodeAction
  = SepInit
  | SepDeinit
  | SepInsert PubKeyHash StakingSetNode
  | SepRemove PubKeyHash StakingSetNode
  | InsertSeps [BuiltinByteString] StakingSetNode
  | -- | first arg is the key to insert, second arg is the covering node
    RemoveSeps [BuiltinByteString] StakingSetNode
  -- first arg is the key to remove, second arg is the covering node
  deriving stock (Show, Eq, Generic)
PlutusTx.unstableMakeIsData ''SepNodeAction

data StakingNodeAction
  = Init
  | Deinit
  | -- | first arg is the key to insert, second arg is the covering node
    Insert PubKeyHash StakingSetNode
  | -- | first arg is the key to remove, second arg is the covering node
    Remove PubKeyHash StakingSetNode
  | Claim PubKeyHash
  deriving stock (Show, Eq, Generic)
PlutusTx.makeIsDataIndexed ''StakingNodeAction ([('Init, 0), ('Deinit, 1), ('Insert, 2), ('Remove, 3), ('Claim, 4)])

data PNodeKey (s :: S)
  = PKey (Term s (PDataRecord '["_0" ':= PByteString]))
  | PEmpty (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

deriving via
  (DerivePConstantViaData StakingNodeKey PNodeKey)
  instance
    PConstantDecl StakingNodeKey

instance PUnsafeLiftDecl PNodeKey where
  type PLifted PNodeKey = StakingNodeKey

deriving anyclass instance
  PTryFrom PData PNodeKey

instance DerivePlutusType PNodeKey where type DPTStrat _ = PlutusTypeData

data PNodeKeyState (s :: S)
  = PKeyScott (Term s PByteString)
  | PEmptyScott
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PNodeKeyState where type DPTStrat _ = PlutusTypeScott

instance ScottConvertible PNodeKey where
  type ScottOf PNodeKey = PNodeKeyState
  toScott nodeKey = pmatch nodeKey $ \case
    PKey kname -> pcon (PKeyScott (pfield @"_0" # kname))
    PEmpty _ -> pcon PEmptyScott
  fromScott nodeKeyScott = pmatch nodeKeyScott $ \case
    PKeyScott bs -> pcon (PKey (pdcons # pdata bs # pdnil))
    PEmptyScott -> pcon (PEmpty pdnil)

data PStakingSetNodeState (s :: S) = PStakingSetNodeState
  { key :: Term s PNodeKeyState
  , next :: Term s PNodeKeyState
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PStakingSetNodeState where type DPTStrat _ = PlutusTypeScott

data PStakingSetNode (s :: S)
  = PStakingSetNode
      ( Term
          s
          ( PDataRecord
              '[ "key" ':= PNodeKey
               , "next" ':= PNodeKey
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PStakingSetNode where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData PStakingSetNode

deriving anyclass instance
  PTryFrom PData (PAsData PStakingSetNode)

instance PUnsafeLiftDecl PStakingSetNode where
  type PLifted PStakingSetNode = StakingSetNode

deriving via
  (DerivePConstantViaData StakingSetNode PStakingSetNode)
  instance
    PConstantDecl StakingSetNode

instance ScottConvertible PStakingSetNode where
  type ScottOf PStakingSetNode = PStakingSetNodeState
  toScott discSetNode' = pmatch discSetNode' $ \(PStakingSetNode discSetNode) -> pletFields @'["key", "next"] discSetNode $ \discSetNodeF ->
    pcon (PStakingSetNodeState {key = toScott discSetNodeF.key, next = toScott discSetNodeF.next})
  fromScott discSetNode =
    pmatch discSetNode $
      \( PStakingSetNodeState
          { key
          , next
          }
        ) ->
          ( pcon
              ( PStakingSetNode
                  ( pdcons @"key"
                      # pdata (fromScott key)
                      #$ (pdcons @"next" # pdata (fromScott next))
                      #$ pdnil
                  )
              )
          )

data PSeparatorConfig (s :: S)
  = PSeparatorConfig
      ( Term
          s
          ( PDataRecord
              '[ "signer" ':= PPubKeyHash
               , "cutOff" ':= PPOSIXTime
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PSeparatorConfig where type DPTStrat _ = PlutusTypeData

mkNode :: Term s (PNodeKey :--> PNodeKey :--> PStakingSetNode)
mkNode = phoistAcyclic $
  plam $ \key next ->
    pcon $
      PStakingSetNode $
        pdcons @"key"
          # pdata key
          #$ pdcons @"next"
          # pdata next
          #$ pdnil

data PStakingNodeAction (s :: S)
  = PInit (Term s (PDataRecord '[]))
  | PDeinit (Term s (PDataRecord '[]))
  | PInsert (Term s (PDataRecord '["keyToInsert" ':= PPubKeyHash, "coveringNode" ':= PStakingSetNode]))
  | PRemove (Term s (PDataRecord '["keyToRemove" ':= PPubKeyHash, "coveringNode" ':= PStakingSetNode]))
  | PClaim (Term s (PDataRecord '["keyToRemove" ':= PPubKeyHash]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PStakingNodeAction where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData PStakingNodeAction

deriving anyclass instance
  PTryFrom PData (PAsData PStakingNodeAction)

instance PUnsafeLiftDecl PStakingNodeAction where
  type PLifted PStakingNodeAction = StakingNodeAction
deriving via
  (DerivePConstantViaData StakingNodeAction PStakingNodeAction)
  instance
    PConstantDecl StakingNodeAction

data PSepNodeAction (s :: S)
  = PSepInit (Term s (PDataRecord '[]))
  | PSepDeinit (Term s (PDataRecord '[]))
  | PSepInsert (Term s (PDataRecord '["keyToInsert" ':= PPubKeyHash, "coveringNode" ':= PStakingSetNode]))
  | PSepRemove (Term s (PDataRecord '["keyToRemove" ':= PPubKeyHash, "coveringNode" ':= PStakingSetNode]))
  | -- | separators must be sorted or validation will fail
    PInsertSeps (Term s (PDataRecord '["separators" ':= PBuiltinList (PAsData PByteString), "coveringNode" ':= PStakingSetNode]))
  | PRemoveSeps (Term s (PDataRecord '["separators" ':= PBuiltinList (PAsData PByteString), "coveringNode" ':= PStakingSetNode]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PSepNodeAction where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData (PAsData PSepNodeAction)

instance PUnsafeLiftDecl PSepNodeAction where
  type PLifted PSepNodeAction = SepNodeAction
deriving via
  (DerivePConstantViaData SepNodeAction PSepNodeAction)
  instance
    PConstantDecl SepNodeAction

-----------------------------------------------
-- Helpers:

mkBSNode :: ClosedTerm (PByteString :--> PByteString :--> PAsData PStakingSetNode)
mkBSNode = phoistAcyclic $
  plam $ \key' next' ->
    let key = pcon $ PKey $ pdcons @"_0" # pdata key' #$ pdnil
        next = pcon $ PKey $ pdcons @"_0" # pdata next' #$ pdnil
     in pdata $ mkNode # key # next

-- | Checks that the node is the empty head node and the datum is empty
isEmptySet :: ClosedTerm (PAsData PStakingSetNode :--> PBool)
isEmptySet = phoistAcyclic $
  plam $ \head -> P.do
    keys <- pletFields @'["key", "next"] head
    isNothing # pfromData keys.key #&& isNothing # pfromData keys.next

-- | Checks that a PubKeyHash does belong to the first Node in the set.
isFirstNode :: ClosedTerm (PByteString :--> PStakingSetNode :--> PBool)
isFirstNode = phoistAcyclic $
  plam $ \key node -> P.do
    keys <- pletFields @'["key", "next"] node
    pmatch (keys.next) $ \case
      PKey n ->
        key #== pfromData (pfield @"_0" # n) #&& isNothing # pfromData keys.key
      _ -> pcon PFalse

-- | Checks that a PubkeyHash does belong to the last Node in a set.
isLastNode :: ClosedTerm (PByteString :--> PStakingSetNode :--> PBool)
isLastNode = phoistAcyclic $
  plam $ \key node -> P.do
    keys <- pletFields @'["key", "next"] node
    pmatch (keys.key) $ \case
      PKey ((pfield @"_0" #) -> n) ->
        key #== pfromData n #&& isNothing # pfromData keys.next
      _ -> pcon PFalse

-- | Checks that node key is absent.
isNothing :: Term s (PNodeKey :--> PBool)
isNothing = phoistAcyclic $
  plam $ \md -> pmatch md $ \case
    PKey _ -> pcon PFalse
    PEmpty _ -> pcon PTrue

{- | @
  node `asPredecessorOf` next
  @ makes @node@ to be a predecessor of a node with *key* @next@
  Seen as if the node between them was removed.
  @node.key@ remains the same, @node.next@ changes to @next@.
-}
asPredecessorOf :: ClosedTerm (PAsData PStakingSetNode :--> PByteString :--> PStakingSetNode)
asPredecessorOf = phoistAcyclic $
  plam $ \node next ->
    let nodeKey = pfromData $ pfield @"key" # node
        nextPK = pcon $ PKey $ pdcons @"_0" # pdata next #$ pdnil
     in mkNode # nodeKey # nextPK

{- | @
    key `asSuccessorOf` node
  @ makes @node@ to be a successor of a node with *next* @key@
  Seen as if the node between them was removed.
  @node.next@ remains the same, @node.key@ changes to @key@.
-}
asSuccessorOf :: ClosedTerm (PByteString :--> PAsData PStakingSetNode :--> PStakingSetNode)
asSuccessorOf = phoistAcyclic $
  plam $ \key node ->
    let nodeNext = pfromData $ pfield @"next" # node
        keyPK = pcon $ PKey $ pdcons @"_0" # pdata key #$ pdnil
     in mkNode # keyPK # nodeNext

-- | Extracts the next node key
getNextPK :: ClosedTerm (PAsData PStakingSetNode :--> PMaybe PPubKeyHash)
getNextPK = phoistAcyclic $
  plam $ \node ->
    let nextNodeKey = pfromData $ pfield @"next" # node
     in pmatch nextNodeKey $ \case
          PEmpty _ -> pcon PNothing
          PKey ((pfield @"_0" #) -> n) -> pcon $ PJust $ pcon $ PPubKeyHash $ pfromData n

-- | Extracts the node key
getCurrentPK :: ClosedTerm (PAsData PStakingSetNode :--> PMaybe PPubKeyHash)
getCurrentPK = phoistAcyclic $
  plam $ \node ->
    let nodeKey = pfromData $ pfield @"key" # node
     in pmatch nodeKey $ \case
          PEmpty _ -> pcon PNothing
          PKey ((pfield @"_0" #) -> n) -> pcon $ PJust $ pcon $ PPubKeyHash $ pfromData n

{- | Checks whether @SetNode@ key is less than next node key.
  Any valid sequence of nodes MUST follow this property.
-}
validNode :: ClosedTerm (PAsData PStakingSetNode :--> PBool)
validNode = phoistAcyclic $
  plam $ \node -> P.do
    nodeDatum <- pletFields @'["key", "next"] node
    pmatch (nodeDatum.key) $ \case
      PEmpty _ -> pcon PTrue
      PKey ((pfield @"_0" #) -> key) -> pmatch (nodeDatum.next) $ \case
        PEmpty _ -> pcon PTrue
        PKey ((pfield @"_0" #) -> next) ->
          pfromData key #< pfromData next -- nodes ordered incrementally
