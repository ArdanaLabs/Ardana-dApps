{-# LANGUAGE UndecidableInstances #-}

module DanaSwap (
  trivialCbor,
  trivialFailCbor,
  configScriptCbor,
  nftCbor,
  liqudityTokenCBor,
) where

import Plutarch.Prelude

import Utils (closedTermToHexString)

import Plutarch.Api.V2 (PMintingPolicy, PScriptPurpose (PMinting), PValidator)

import Plutarch.Api.V1 (PTokenName, PValue (PValue))
import Plutarch.Api.V1.AssocMap qualified as PMap
import Plutarch.Api.V2.Tx (PTxOutRef)
import Plutarch.Extensions.Data (parseData, ptryFromData)
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC)
import Plutarch (Config)

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

trivialCbor :: Config -> Maybe String
trivialCbor = closedTermToHexString trivial

trivial :: ClosedTerm (PData :--> PValidator)
trivial = plam $ \_ _ _ _ -> popaque $ pcon PUnit

trivialFailCbor :: Config -> Maybe String
trivialFailCbor = closedTermToHexString trivialFail

trivialFail :: ClosedTerm PValidator
trivialFail = perror

configScriptCbor :: Config -> Maybe String
configScriptCbor = closedTermToHexString configScript

configScript :: ClosedTerm PValidator
configScript = perror

liqudityTokenCBor :: Config -> Maybe String
liqudityTokenCBor = closedTermToHexString liqudityTokenMP

liqudityTokenMP :: ClosedTerm (PData :--> PMintingPolicy)
liqudityTokenMP = phoistAcyclic $
  plam $
    \poolIdCSData redeemerData scriptContextData -> unTermCont $ do
      -- Parse various information
      LiquidityRedeemer redeemerRec' <- pmatchC $ pfromData $ ptryFromData redeemerData
      redeemerRec <- pletFieldsC @'["poolId", "action"] redeemerRec'
      scriptContextRec <- pletFieldsC @'["txInfo", "purpose"] scriptContextData
      poolIdCS <- pletC $ pfromData (ptryFromData poolIdCSData)
      poolIdTokenName <- pletC $ getField @"poolId" redeemerRec
      -- Get own CS from script purpose
      PMinting liquidityCSRec <- pmatchC $ getField @"purpose" scriptContextRec
      infoRec <- pletFieldsC @'["mint", "inputs"] (getField @"txInfo" scriptContextRec)
      let minting = getField @"mint" infoRec
      PValue mintingMap <- pmatchC minting
      PJust liquidity <- pmatchC $ PMap.plookup # (pfield @"_0" # liquidityCSRec) # mintingMap
      PMap.PMap liquidityAsList <- pmatchC liquidity
      -- Common assertions for both actions
      pguardC "minted exactly one token name of liquidity tokens" $
        plength # liquidityAsList #== 1
      pguardC "token name matched redeemer" $
        pfromData (pfstBuiltin #$ phead # liquidityAsList) #== poolIdTokenName
      -- Case over the action field of the redeemer
      pmatchC (pfromData $ getField @"action" redeemerRec) >>= \case
        Open _ -> do
          -- look up the pool id from the mint field of the script context
          -- and check that it matches the redeemer
          PJust idTokens <- pmatchC $ PMap.plookup # poolIdCS # mintingMap
          PJust _shouldBe1 <- pmatchC $ PMap.plookup # getField @"poolId" redeemerRec # idTokens
          -- TODO should we check that it is just 1? It should be redundant so for now I'm not checking
          pure $ popaque $ pcon PUnit
        Spend _ -> do
          let inputs = pfromData $ getField @"inputs" infoRec
          pguardC "token name matched redeemer" $
            pany # isRightPool # inputs
          pure $ popaque $ pcon PUnit
          where
            isRightPool = plam $ \input -> unTermCont $ do
              -- Check that the value of the field contains the right poolID
              -- This can't error when it doesn't (not all inputs are the pool) hence the cases
              PValue val <- pmatchC $ pfield @"value" # (pfield @"resolved" # input)
              pmatchC (PMap.plookup # poolIdCS # val) >>= \case
                PNothing -> pure $ pcon PFalse
                PJust poolIdToken ->
                  pmatchC (PMap.plookup # poolIdTokenName # poolIdToken) >>= \case
                    PNothing -> pure $ pcon PFalse
                    PJust _ -> pure $ pcon PTrue

nftCbor :: Config -> Maybe String
nftCbor = closedTermToHexString standardNft

standardNft :: ClosedTerm (PData :--> PMintingPolicy)
standardNft = phoistAcyclic $
  plam $ \outRefData _ sc -> unTermCont $ do
    outRef :: Term _ PTxOutRef <- parseData outRefData
    let (inputs :: Term _ (PBuiltinList PTxOutRef)) =
          pmap # pfield @"outRef"
            #$ pfield @"inputs"
            #$ pfield @"txInfo" # sc
    pguardC "didn't spend out ref" $ pelem # outRef # inputs
    pure $ popaque $ pcon PUnit
