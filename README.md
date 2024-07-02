# Single Asset Staking

## Table of Contents

- [Introduction](#introduction)
- [Overview](#overview)
- [Details](#details)
  - [Deployment](#deployment)
    - [Build Scripts](#build-scripts-buildscriptsts)
    - [Deploy Reference Scripts](#deploy-reference-scripts-deployrefscriptsts)
  - [Setup](#setup)
    - [Create Config UTxO](#create-config-utxo-createconfigts)
    - [Initialize Staking](#initialize-staking-initstakingts)
  - [User Participation](#user-participation)
  - [Active Staking](#active-staking)
  - [Rewards Processing](#rewards-processing-processrewardsts)
    - [Initialize Commit Fold](#initialize-commit-fold-initfoldts)
    - [Complete Commit Fold](#complete-commit-fold-multifoldts)
    - [Initialize Reward Fold](#initialize-reward-fold-initrewardfoldts)
    - [Complete Reward Fold](#complete-reward-fold-rewardfoldnodests)
    - [Project Reclaims Reward](#project-reclaims-reward-reclaimrewardts)
    - [Deinitialize Head Node](#deinitialize-head-node-dinitnodets)
  - [Claim](#claim-reclaimnodets)
- [Important Notes](#important-notes)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Building and Developing](#building-and-developing)
  - [Tests](#tests)
- [License](#license)

# Introduction

Single Asset Staking contracts facilitate collective staking of digital assets and distributing rewards among participants in a completely on-chain and trustless manner.

As the name suggests, it allows for a single asset, which can be any Cardano Native Fungible Token, to be staked to earn rewards. The reward itself can be any Cardano Native Fungible Token. The contracts are parameterized with `policyId` and `tokenName` (in addition to a few others) of stake token and reward token. This allows different projects conducting the Staking event to configure the contracts accordingly.

Instead of a fixed percentage based return, the staking reward is determined on pro rata basis. Calculated using the total amount of assets staked till the end of the staking period and the total rewards locked before staking began. Eligible participants are then given rewards propotional to their share of stake (`(userStake * totalRewards) / totalStake`).

This project is funded by the Cardano Treasury in [Catalyst Fund 10](https://projectcatalyst.io/funds/10/f10-developer-ecosystem-the-evolution/anastasia-labs-open-source-production-grade-dapps).

# Overview

An interesting technical detail about this protocol is the use of an [on-chain association list](https://github.com/Plutonomicon/plutonomicon/blob/main/assoc.md). It maintains every unique public key's stake in a separate UTxO which points to the next stake UTxO. Every UTxO in the list will have `StakingSetNode` in its datum.

```hs
data StakingSetNode = MkSetNode
  { key :: StakingNodeKey  -- owner wallet's PaymentPubKeyHash
  , next :: StakingNodeKey -- next PaymentPubKeyHash in a list of lexicographically sorted key hashes
  {- This field tells us which Staking Campaign this node belongs to.
     Each Staking Campaign is uniquely identified by a Config UTxO
     containing an NFT (configCS.configTN) -}
  , configTN :: TokenName
  }

data StakingNodeKey = Key BuiltinByteString | Empty
```

This sections provides you with the timeline of different phases involved in Single Asset Staking. With each phase further listing the order of actions which comprises it.

```mermaid
timeline

    title Single Asset Staking Phases

    Deployment : Build Scripts
               : Deploy Reference Scripts

    Setup : Create Config UTxO
          : Initialize Staking

    User Participation : Register Stake
                       : Modify Stake*
                       : Remove Stake* (w/o penalty)

    Active Staking : Remove Stake* (w/ penalty)

    Rewards Processing : Initialize Commit Fold
                       : Complete Commit Fold
                       : Initialize Reward Fold
                       : Complete Reward Fold
                       : Project Reclaims Reward
                       : Deinitialize Head Node

    Claim : Users Claim Stake & Reward
```

> Note: [*] - Actions which can be performed if user wishes to.

# Details

## Deployment

Everything begins here with _Anastasia Labs_ configuring and providing the Smart Contracts, on-chain as Reference Scripts. Once these are available, Projects can reuse the same contracts for any number of new Staking Campaigns. The deployment phase comprises of below two actions.

### **Build Scripts** `buildScripts.ts`

The contracts available from Single Asset Staking repository, require certain paramters to be applied. These parameters include `configCS` (policyId of ConfigPolicy) and script credentials in case of dependant scripts. This step involves providing contracts with the required parameters.

### **Deploy Reference Scripts** `deployRefScripts.ts`

This step uses the applied validators obtained above to create a [Reference Script UTxO](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033) for every validator. In order to easily identify a particular validator on-chain, a native minting policy is used in conjuction. It mints an NFT with the validator name and is made available inside the RefUTxO. This native minting policy allows minting for a very short duration of _thirty mintues_ within which all the RefUTxOs must be created. All the RefUTxOs are sent to an "Always Fail Script" address ensuring they are immutable and locked forever.

```mermaid
---
title: Deploy Reference Scripts
---
graph LR
    I1(Input UTxO)
    TX[ Transaction ]
    subgraph Always Fails Script
    O1(("UTxO
        $deployId.ConfigPolicy
        ref_script: configPolicy "))
    O2(("UTxO
        $deployId.NodeValidator
        ref_script: nodeValidator "))
    O3(("UTxO
        $deployId.NodePolicy
        ref_script: nodePolicy"))
    ..
    end
    I1 --> TX
    MP{Native Minting Policy} -.-o TX
    TX --> O3
```

## Setup

Every Project which wants to create a new Staking Campaign will start from here. This phases consists of below three actions. Its only after this phase is completed that users can begin staking.

### **Create Config UTxO** `createConfig.ts`

Every individual Staking Campaign begins by first creating a Config UTxO for it. It works like this:

- Every Staking Campaign's configuration, instead of being configured in the contract as parameters, is obtained from a Config UTxO's datum. This datum (of type `StakingConfig`) contains all the event related details. Thereby leaving the same set of contracts to work for multiple campaigns.

```hs
data StakingConfig = StakingConfig
  { stakingInitUTxO :: TxOutRef
  , freezeStake :: POSIXTime
  , endStaking :: POSIXTime
  , penaltyAddress :: Address
  , stakeCS :: CurrencySymbol
  , stakeTN :: TokenName
  , minimumStake :: Integer
  , rewardCS :: CurrencySymbol
  , rewardTN :: TokenName
  }
```

- All the deployed smart contracts require this Config UTxO as a reference input to validate every transaction.
- With the help of a Config Policy, this UTxO is uniquely identified by an NFT (`configCS.configTN`), minted in the same transaction.
- This ouput is then sent to an Always Fails Script address, guaranteeing no changes to the camapaign parameters.
- All the UTxOs belonging to a particular campaign will have the same `configTN` field value in their datum to avoid mixing UTxOs from different campaigns.

```mermaid
---
title: Create Config UTxO
---
graph LR
    I1(Config Init UTxO)
    TX[ Transaction ]
    subgraph Always Fails Script
    O1(("Output
        $ConfigPolicy.abc: 1n
        datum: StakingConfig"))
    end
    I1 --> TX
    MP{"Ref Input
      $deployId.ConfigPolicy"}  -.-o|Mint $ConfigPolicy.configTN| TX
    TX --> O1
```

### **Initialize Staking** `initStaking.ts`

Here project locks the entire staking reward in `tokenHolderValidator`. The total reward amount will be distributed among participants in proportion to their stake. Locking of rewards beforehand gives high assurance to all the participants before they can begin staking. Additional one percent of total reward amount is paid as protocol fees for facilitating staking to Anastasia Labs.

> Note: The wallet containing the `stakingInitUTxO` must have enough reward tokens to cover the total reward amount plus 1% protocol fees along with
> sufficient lovelaces to cover mininum ADA costs and transaction fees.

This transaction also marks the beginning of the association list which will contain all the stake by different participants as separate UTxOs. The first node of the list know as head node is created in this step.

Head node differs from all the nodes in that its key is null. Every valid stake UTxO in the list has a unique "Node Token" which is minted by `nodePolicy` at the time of its insertion. The token name is derived as NODE_PREFIX ("FSN") + PaymentPubKeyHash thereby making every node token unique. Head node just has NODE_PREFIX as the token name.

```mermaid
---
title: Initialize Staking
---
graph LR
    I1(Staking Init UTxO)
    TX[ Transaction ]
    subgraph Token Holder Validator
    O1(("Output 1
        $rewardCS.rewardTN: totalReward
        $TokenHolderPolicy.RTHolder: 1n
        datum: configTN = abc"))
    end
    subgraph Staking Validator
    O2(("Head Node
        $stakeCS.stakeTN: minStake
        $NodePolicy.FSN: 1n
        datum: key = null, next = null,
        configTN = abc"))
    end
    O3(("Output 3"))
    I1 --> TX
    MP1{"Ref Input
      $deployId.TokenHolderPolicy"}  -.-o|Mint $TokenHolderPolicy.RTHolder| TX
    TX --> O1
    TX --> O2
    MP2{"Ref Input
      $deployId.NodePolicy"}  -.-o|Mint $NodePolicy.FSN| TX
    TX -->|1% Protocol Fees| O3
    R1(("Ref Input
        $ConfigPolicy.abc: 1n
        datum: StakingConfig")) -.-o TX
```

## User Participation

Now the Staking event is opened and users can participate by locking their stake in `nodeValidator` by updating the linked list. Before the stake is frozen, participants can choose to increase, decrease or remove their stake altogether.

## Active Staking (`insertNode.ts`, `modifyNode.ts`, `removeNode.ts`)

Once stake is frozen (configured by parameter `freezeStake :: POSIXTime`), the active staking phase begins for which the participants will be earning rewards. This phase lasts till `endStaking :: POSIXTime` as decided by the project. During this period, new participants cannot enter nor can the old ones modify their stake. However, existing stakers can still get their stake back if they choose to, by paying 25% of their stake as penalty fee.

## Rewards Processing `processRewards.ts`

After the active staking phase has ended (after `endStaking :: POSIXTime`) comes the part where project processes and allocates rewards to its participants who staked till now.

Its done by first calculating and saving the total amount staked on-chain. Then every participant's stake UTxO is updated to include rewards in it, in proportion to their stake. Reward calculation is given by the formula `(userStake * totalRewards) / totalStake`.

Following sequence of on-chain actions elaborate further on how rewards processing mechanism works. The `processRewards.ts` endpoint carries out all
the below actions for the project in a sequential manner.

### **Initialize Commit Fold** `initFold.ts`

Commit Fold carries out the computation of total staked amount by going over all the linked list UTxOs one after the other in order. The current state of the computation, i.e. how far along the linked list it has summed and the current sum, is stored in a UTxO at `foldValidator`. This UTxO is uniquely identified with the presence of an NFT ($FoldPolicy.CFold) minted using `foldPolicy`. This initialization of commit UTxO is perfomed in this step.

```mermaid
---
title: Initialize Commit Fold
---
graph LR
    I1(Input UTxO)
    TX[ Transaction ]
    subgraph Staking Validator
    N1(("Ref Input
        $stakeCS.stakeTN: minStake
        $NodePolicy.FSN: 1n
        datum:
        key = null, next = aa1,
        configTN = abc "))
    N2(("UTxO
        $stakeCS.stakeTN: minStake
        $NodePolicy.FSNaa1 : 1n
        datum:
        key = aa1, next = bb2,
        configTN = abc "))
    N3(("UTxO
        $stakeCS.stakeTN: minStake
        $NodePolicy.FSNbb2 : 1n
        datum:
        key = bb2, next = null,
        configTN = abc "))
    end
    subgraph Fold Validator
    O1(("Output
        $FoldPolicy.CFold: 1n
        datum:
        key = null, next = aa1,
        totalStake = 0n,
        configTN = abc
        "))
    end
    N1 -.-o TX
    I1 --> TX
    MP{"Ref Input
      $deployId.FoldPolicy"}  -.-o|Mint $FoldPolicy.CFold| TX
    TX --> O1
    R1(("Ref Input
        $ConfigPolicy.abc: 1n
        datum: StakingConfig")) -.-o TX
```

### **Complete Commit Fold** `multiFold.ts`

Here one stake UTxO after another is used as reference input to calculate and update `totalStake` value in Commit Fold UTxO's datum. This is done till the end of list is not reached, at which point `next = null` in fold datum and `totalStake` is finally determined. This endpoint needs to be called repeatedly till `fetchCampaignState` in `fetchState.ts` does not return `CapaignStatus.StakeCalculationEnded` in its CampaignStatus field of CampaignState.

```mermaid
---
title: Complete Commit Fold
---
graph LR
    I1(Input UTxO)
    TX[ Transaction ]
    subgraph Staking Validator
    N1(("UTxO
        $stakeCS.stakeTN: minStake
        $NodePolicy.FSN: 1n
        datum:
        key = null, next = aa1,
        configTN = abc "))
    N2(("Ref Input
        $stakeCS.stakeTN: minStake
        $NodePolicy.FSNaa1 : 1n
        datum:
        key = aa1, next = bb2,
        configTN = abc "))
    N3(("Ref Input
        $stakeCS.stakeTN: minStake
        $NodePolicy.FSNbb2 : 1n
        datum:
        key = bb2, next = null,
        configTN = abc "))
    end
    subgraph Fold Validator
    F1(("Input
        $FoldPolicy.CFold: 1n
        datum:
        key = null, next = aa1,
        totalStake = 0n,
        configTN = abc
        "))
    F2(("Output
        $FoldPolicy.CFold: 1n
        datum:
        key = null, next = null,
        totalStake = 2 * minStake,
        configTN = abc
        "))
    end
    F1 --> TX
    N2 -.-o TX
    N3 -.-o TX
    I1 --> TX
    TX --> F2
    R1(("Ref Input
        $ConfigPolicy.abc: 1n
        datum: StakingConfig")) -.-o TX
```

> Note: Head Node's stake is never taken into account.

### **Initialize Reward Fold** `initRewardFold.ts`

Now that we have total staked amount available on-chain, we initialize the reward fold wherein a UTxO to `rewardFoldValidator` is sent. This contains total reward amount obtained from UTxO locked at "Token Holder Validator" along with `totalRewardTokens` and `totalStake` in its datum. Additionally, it has `$RewardPolicy.RFold` NFT minted from "Reward Policy" which validates that the initialization is carried out accurately.

```mermaid
---
title: Initialize Reward Fold
---
graph LR
    I1(Input UTxO)
    TX[ Transaction ]
    subgraph Staking Validator
    N1(("Head Node Input
        $ADA : 3
        $stakeCS.stakeTN: minStake
        $NodePolicy.FSN: 1n
        datum:
        key = null, next = aa1,
        configTN = abc "))
    N2(("Head Node Output
        $ADA : 2
        $stakeCS.stakeTN: minStake
        $NodePolicy.FSN: 1n
        datum:
        key = null, next = aa1,
        configTN = abc "))
    end
    subgraph Commit Fold Validator
    F1(("Input
        $FoldPolicy.CFold: 1n
        datum:
        totalStake = 2 * minStake
        key = null, next = null,
        configTN = abc
        "))
    end
    subgraph Reward Fold Validator
    O1(("Output
        $RewardPolicy.RFold: 1n
        $rewardCS.rewardTN: totalReward
        datum:
        totalRewardTokens = totalReward
        totalStake = 2 * minStake
        key = null, next = aa1,
        configTN = abc
        "))
    end
    subgraph Token Holder Validator
    T1(("Input
        $rewardCS.rewardTN: totalReward
        $TokenHolderPolicy.RTHolder: 1n
        datum: configTN = abc "))
    end
    MP1{"Ref Input
      $deployId.TokenHolderPolicy"}  -.-o|Burn $TokenHolderPolicy.RTHolder| TX
    MP2{"Ref Input
      $deployId.FoldPolicy"}  -.-o|Burn $FoldPolicy.CFold| TX
    MP3{"Ref Input
      $deployId.RewardPolicy"}  -.-o|Mint $RewardPolicy.RFold| TX

    T1 --> TX
    F1 --> TX
    N1 --> TX
    I1 --> TX
    TX --> O1
    TX --> N2
    R1(("Ref Input
        $ConfigPolicy.abc: 1n
        datum: StakingConfig")) -.-o TX
```

> Note: Upon undergoing rewards fold a UTxO has to pay 1 ADA folding fee.

### **Complete Reward Fold** `rewardFoldNodes.ts`

With Reward Fold UTxO initialized with rewards and other essential information, rewards can be distributed into individual stake UTxO. This is similar to commit fold, with UTxO after head node being processed first and other UTxOs in the order they appear in list. Rewards fold gets concluded when `next = null` on processing the last UTxO of the list. Upon undergoing rewards fold a UTxO has to pay 1 ADA folding fee. This endpoint needs to be called repeatedly till `fetchCampaignState` in `fetchState.ts` does not return `CapaignStatus.UserClaimsAllowed` in its CampaignStatus field of CampaignState.

```mermaid
---
title: Complete Reward Fold
---
graph LR
    I1(Input UTxO)
    TX[ Transaction ]
    subgraph Staking Validator
    N1(("UTxO
        $stakeCS.stakeTN: minStake
        $NodePolicy.FSN: 1n
        datum:
        key = null, next = aa1,
        configTN = abc "))
    N2(("Input
        $stakeCS.stakeTN: minStake
        $NodePolicy.FSNaa1 : 1n
        datum:
        key = aa1, next = bb2,
        configTN = abc "))
    N3(("Input
        $stakeCS.stakeTN: minStake
        $NodePolicy.FSNbb2 : 1n
        datum:
        key = bb2, next = null,
        configTN = abc "))
    N4(("Output
        $stakeCS.stakeTN: minStake
        $NodePolicy.FSNaa1 : 1n
        $rewardCS.rewardTN: totalReward/2
        datum:
        key = aa1, next = bb2,
        configTN = abc "))
    N5(("Output
        $stakeCS.stakeTN: minStake
        $NodePolicy.FSNbb2 : 1n
        $rewardCS.rewardTN: totalReward/2
        datum:
        key = bb2, next = null,
        configTN = abc "))
    end
    subgraph Reward Fold Validator
    O1(("Output
        $RewardPolicy.RFold: 1n
        $rewardCS.rewardTN: totalReward
        datum:
        totalRewardTokens = totalReward
        totalStake = 2 * minStake
        key = null, next = aa1,
        configTN = abc
        "))
    O2(("Output
        $RewardPolicy.RFold: 1n
        $rewardCS.rewardTN: rewardsLeft*
        datum:
        totalRewardTokens = totalReward
        totalStake = 2 * minStake
        key = null, next = null,
        configTN = abc
        "))
    end
    O1 --> TX
    N2 --> TX
    N3 --> TX
    I1 --> TX
    TX --> O2
    TX --> N4
    TX --> N5
    R1(("Ref Input
        $ConfigPolicy.abc: 1n
        datum: StakingConfig")) -.-o TX
```

> Note: [*] - If any reward tokens are left due to remainder from integer division in `(userStake * totalRewards) / totalStake`

### **Project Reclaims Reward** `reclaimReward.ts`

Once the rewards are processed, project is free to claim any remaining project tokens left in "Reward Fold UTxO" along with any lovelaces present. They'll have to additionally burn "$RewardPolicy.RFold" token for this, which is only allowed when `next = null` i.e. all rewards are processed.

### **Deinitialize Head Node** `dinitNode.ts`

The project is also free to reclaim the Head Node with the "minStake" and lovelaces present in it. It can only be done after the reward fold is initiated (Reward Fold Token datum has `next == *head node's next*`), therefore ensuring no information is lost.

## Claim `reclaimNode.ts`

Only after rewards are processed can the participants claim their stake and reward. They can do so by spending their stake UTxO from "Staking Validator" after signing transaction with private key belonging to the PaymentPubKeyHash as `key` in UTxO's datum.

# Important Notes

1. Its advisable to use two different wallets, each containing one Init UTxO (`configInitUTxO` & `stakingInitUTxO`). So that they aren’t spent before their respective initialization transactions. The wallet containing the `stakingInitUTxO` must have enough reward tokens to cover the total reward amount plus 1% protocol fees along with sufficient lovelaces to cover mininum ADA costs and transaction fees.
2. Only `stakeTN` and `rewardTN` fields are expected to be UTF-8 encoded strings. All the other Currency Symbols/ Policy Ids and Token name strings are expected to Hex encoded strings.
3. The offchain expects Cardano Native Token amounts in their lowest denomination/unit. For example, if the Stake Token is [MIN](https://cardanoscan.io/token/29d222ce763455e3d7a09a665ce554f00ac89d2e99a1a83d267170c64d494e) which has 6 decimal places and you want 10 MIN to be the minimum stake. You will have to configure `minimumStake`(field in `StakingConfig` and other Config objects) to be `10 * 10^6` (`Amount * 10 ^ Decimals`) i.e `10_000_000`. Similarly, the reward amount the project wants to lock as total staking reward, specified by `rewardsAmount` must be in its lowest unit for e.g. if total reward is 1000 MIN then `rewardsAmount == 1_000_000_000`. Likewise, the reponses obtained from the endpoint will provide CNT amount values in their lowest unit. Fields like `totalStake` & `totalReward` (in `CampaignState`), `rewardAmount` in `InitStakingConfig` and `toStake` used while staking or modifying stake, adhere to this representation.
4. All the transactions provided will have a validity range of 6 minutes. Attempting to perform any action using the SDK endpoints with less than 3 minutes of difference from either the `freezeStake` or `endStaking` will result in an error i.e. `|Date.now() - freezeStakeOrEndStaking| > 3 minutes`.
5. Using an on-chain association list for managing stake provides increased throughput with increased number of stakers. However, it
   is susceptible to contention due to multiple users updating the list concurrently. Hence, it is recommended that the project user of Maestro APIs (adding, modifying and withdrawing stake) implement a retry mechanism (with some delay) to handle failures encountered after submitting the signed transactions.


# Getting Started

## Prerequisites

Before you begin, ensure you have [Nix](https://nixos.org) installed on your system. Nix is used for package management and to provide a consistent development environment. If you don't have Nix installed, you can do so by running the following command:

### Official option

[Nix](https://nixos.org/download.html)

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

### Preferred option

[Determinate Systems](https://zero-to-nix.com/concepts/nix-installer)

```sh
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

Make sure to enable [Nix Flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes) by editing either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` on
your machine and add the following configuration entries:

```yaml
experimental-features = nix-command flakes ca-derivations
allow-import-from-derivation = true
```

Optionally, to improve build speed, it is possible to set up binary caches by adding additional configuration entries:

```yaml
substituters = https://cache.nixos.org https://cache.iog.io https://cache.zw3rk.com
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk=
```

To facilitate seamlessly moving between directories and associated Nix development shells we use [direnv](https://direnv.net) and [nix-direnv](https://github.com/nix-community/nix-direnv):

Your shell and editors should pick up on the `.envrc` files in different directories and prepare the environment accordingly. Use `direnv allow` to enable the direnv environment and `direnv reload` to reload it when necessary. Otherwise, the `.envrc` file contains a proper Nix target which will be used with the `nix develop --accept-flake-config` command.

To install both using `nixpkgs`:

```sh
nix profile install nixpkgs#direnv
nix profile install nixpkgs#nix-direnv
```

## Building and developing

Once Nix is installed, you should be able to seamlessly use the repository to
develop, build and run packages.

Download the Git repository:

```sh
git clone https://github.com/Anastasia-Labs/single-asset-staking
```

Navigate to the repository directory:

```sh
cd single-asset-staking
```

Activate the development environment with Nix:

```sh
nix develop --accept-flake-config
```
Or
```sh
make shell
```

Please be patient when building nix development environment for the first time, as it may take a very long time. Subsequent builds should be faster. Additionally, when you run `nix run .#help` you'll get a list of scripts you can run, the Github CI (nix flake check) is setup in a way where it checks the project builds successfully, haskell format is done correctly, and commit message follows conventional commits. Before pushing you should run `cabal run` , `nix run .#haskellFormat` (automatically formats all haskell files, including cabal), if you want to commit a correct format message you can run `cz commit`

Build:

```sh
make build
```

Execute the test suite:

```sh
make test
```

Compile and export Plutarch scripts:

```sh
make export
```

![single-asset-staking](/assets/gifs/single-asset-staking.gif)

## Tests

For comprehensive information on the test suite for Single Asset Staking implementation, including unit tests and property-based tests, please refer to our [test documentation](/test/README.md).

# License

© 2023 Anastasia Labs.

All code is licensed under MIT License. See [LICENSE](./LICENSE) file
for details.
