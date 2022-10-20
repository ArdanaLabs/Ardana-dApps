{-# LANGUAGE UndecidableInstances #-}

module DanaSwap (
  trivialCbor,
  trivialFailCbor,
  configScriptCbor,
  nftCbor,
  liqudityTokenCBor,
) where

import Plutarch.Prelude

import Utils (closedTermToHexString, globalConfig, validatorToHexString)

import Plutarch.Api.V2 (PMintingPolicy, PScriptPurpose (PMinting), PValidator, mkValidator)
import Plutarch.Extensions.Api (passert, passert_)

import Plutarch.Api.V1 (PTokenName, PValue (PValue))
import Plutarch.Api.V1.AssocMap qualified as PMap
import Plutarch.Api.V2.Tx (PTxOutRef)
import Plutarch.Extensions.Data (parseData, ptryFromData)
import Plutarch.Extensions.Monad (pletFieldC)
import Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC)

data LiquidityAction (s :: S)
  = Open (Term s (PDataRecord '[]))
  | Spend (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType LiquidityAction where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData LiquidityAction
instance PShow LiquidityAction

newtype LiquidityRedeemer (s :: S)
  = LiquidityRedeemer
      ( Term
          s
          ( PDataRecord
              '[ "poolId" ':= PTokenName
               , "action" ':= LiquidityAction
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType LiquidityRedeemer where type DPTStrat _ = PlutusTypeNewtype
instance PTryFrom PData (PAsData LiquidityRedeemer)
instance PShow LiquidityRedeemer

trivialCbor :: Maybe String
trivialCbor = closedTermToHexString trivial

trivial :: ClosedTerm (PData :--> PValidator)
trivial = plam $ \_ _ _ _ -> popaque $ pcon PUnit

trivialFailCbor :: Maybe String
trivialFailCbor = closedTermToHexString trivialFail

trivialFail :: ClosedTerm PValidator
trivialFail = perror

configScriptCbor :: String
configScriptCbor = validatorToHexString $ mkValidator globalConfig configScript

configScript :: ClosedTerm PValidator
configScript = perror

liqudityTokenCBor :: Maybe String
liqudityTokenCBor = closedTermToHexString liqudityTokenMP

liqudityTokenMP :: ClosedTerm (PData :--> PMintingPolicy)
liqudityTokenMP = phoistAcyclic $
  plam $
    \poolIdCsData redeemerData sc -> unTermCont $ do
      LiquidityRedeemer redemerRec' <- pmatchC $ pfromData $ ptryFromData redeemerData
      redemerRec <- pletFieldsC @'["poolId", "action"] redemerRec'
      scRec <- pletFieldsC @'["txInfo", "purpose"] sc
      poolIdCs <- pletC $ pfromData (ptryFromData poolIdCsData)
      poolIdTn <- pletC $ getField @"poolId" redemerRec
      PMinting liquidityCsRec <- pmatchC $ getField @"purpose" scRec
      infoRec <- pletFieldsC @'[ "mint" , "inputs" ] (getField @"txInfo" scRec)
      let minting = getField @"mint" infoRec
      PValue mintingMap <- pmatchC minting
      PJust liquidity <- pmatchC $ PMap.plookup # (pfield @"_0" # liquidityCsRec) # mintingMap
      PMap.PMap liquidityAsList <- pmatchC liquidity
      passert_ "minted exactly one token name of liquidity tokens" $
        plength # liquidityAsList #== 1
      passert_ "token name matched redeemer" $
        pfromData (pfstBuiltin #$ phead # liquidityAsList) #== poolIdTn
      pmatchC (pfromData $ getField @"action" redemerRec) >>= \case
        Open _ -> do
          PJust idTokens <- pmatchC $ PMap.plookup # poolIdCs # mintingMap
          PJust _shouldBe1 <- pmatchC $ PMap.plookup # getField @"poolId" redemerRec # idTokens
          -- TODO should we check that it is just 1? It should be redundant so for now I'm not checking
          pure $ popaque $ pcon PUnit
        Spend _ -> do
          let inputs = pfromData $ getField @"inputs" infoRec
          passert "token name matched redeemer" $
            pany # isRightPool # inputs
              where
                isRightPool = plam $ \input -> unTermCont $ do
                    PValue val <- pmatchC $ pfield @"value" # (pfield @"resolved" # input)
                    pmatchC (PMap.plookup # poolIdCs # val) >>= \case
                      PNothing -> pure $ pcon PFalse
                      PJust poolIdToken ->
                        pmatchC (PMap.plookup # poolIdTn # poolIdToken) >>= \case
                          PNothing -> pure $ pcon PFalse
                          PJust _ -> pure $ pcon PTrue

nftCbor :: Maybe String
nftCbor = closedTermToHexString standardNft

standardNft :: ClosedTerm (PData :--> PMintingPolicy)
standardNft = phoistAcyclic $
  plam $ \outRefData _ sc -> unTermCont $ do
    outRef :: Term _ PTxOutRef <- parseData outRefData
    let (inputs :: Term _ (PBuiltinList PTxOutRef)) =
          pmap # pfield @"outRef"
            #$ pfield @"inputs"
            #$ pfield @"txInfo" # sc
    passert "didn't spend out ref" $ pelem # outRef # inputs
