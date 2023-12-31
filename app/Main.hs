{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Cardano.Binary qualified as CBOR
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (
  first,
 )
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Default (def)
import Data.Text (
  Text,
  pack,
 )
import Data.Text.Encoding qualified as Text
import Mint.Standard (mkStakingNodeMPW)
import MultiFold (pfoldValidatorW, pmintFoldPolicyW, pmintRewardFoldPolicyW, prewardFoldValidatorW)
import Plutarch (
  Config (Config),
  TracingMode (DoTracing, NoTracing),
  compile,
 )
import Plutarch.Evaluate (
  evalScript,
 )
import Plutarch.Prelude
import Plutarch.Script (Script, serialiseScript)
import PlutusLedgerApi.V2 (
  Data,
  ExBudget,
 )
import Ply.Plutarch (
  writeTypedScript,
 )
import System.IO
import Validator (pDiscoverGlobalLogicW, pStakingSetValidator)
import "liqwid-plutarch-extra" Plutarch.Extra.Script (
  applyArguments,
 )

encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = Text.decodeUtf8 . Base16.encode . CBOR.serialize' . serialiseScript

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- compile (Config NoTracing) x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

writePlutusScript :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScript title filepath term = do
  case evalT term of
    Left e -> putStrLn (show e)
    Right (script, _, _) -> do
      let
        scriptType = "PlutusScriptV2" :: String
        plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
        content = encodePretty plutusJson
      LBS.writeFile filepath content

main :: IO ()
main = do
  putStrLn "Exporting Single Asset Staking Scripts"
  writePlutusScript "Single Asset Staking - Staking Validator" "./compiled/stakingStakeValidator.json" pDiscoverGlobalLogicW
  writePlutusScript "Single Asset Staking - Spending Validator" "./compiled/stakingValidator.json" $ pStakingSetValidator def "FSN"
  writePlutusScript "Single Asset Staking Mint" "./compiled/stakingMinting.json" $ mkStakingNodeMPW
  writePlutusScript "Commit Fold Validator" "./compiled/foldValidator.json" pfoldValidatorW
  writePlutusScript "Commit Fold Mint" "./compiled/foldMint.json" pmintFoldPolicyW
  writePlutusScript "Reward Fold Validator" "./compiled/rewardFoldValidator.json" prewardFoldValidatorW
  writePlutusScript "Reward Fold Mint" "./compiled/rewardFoldMint.json" pmintRewardFoldPolicyW
