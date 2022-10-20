module Main (main) where

import Hello
import HelloDiscovery

import Control.Monad (unless)
import DanaSwap (liqudityTokenCBor)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (die)
import Utils (Cbor (..), toPureScript)

{- | Main takes a directory as a comand line argument
  and creates a file CBOR.purs in that directory
  which will provide variables as configured in
  the cbors constant
-}

-- TODO should this be a utility? or should we just remove the helloworld one eventually
main :: IO ()
main = do
  getArgs >>= \case
    [out] -> do
      exists <- doesDirectoryExist out
      unless exists $ die $ "directory: " <> out <> " does not exist"
      writeFile (out ++ "/CBOR.purs")
        . ( "--this file was automatically generated by the onchain code\n"
              <>
          )
        =<< toPureScript cbors
    _ -> die "usage: cabal run hello-world <file_path>"

cbors :: [Cbor]
cbors =
  [ Cbor "trivial" trivialCbor
  , Cbor "trivialFail" trivialFailCbor
  , Cbor "nft" nftCbor
  , Cbor "configScript" $ pure configScriptCbor
  , Cbor "liqudityTokenMP" liqudityTokenCBor
  ]
