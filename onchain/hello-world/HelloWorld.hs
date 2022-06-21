module Main (main) where

import Hello

import Control.Monad (unless)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (die)
import Data.List (intercalate)

{- | Main takes a directory as a comand line argument
  and creates a file CBOR.purs in that directory
  which will provide variables as configured in
  the cbors constant
-}
main :: IO ()
main = do
  getArgs >>= \case
    [out] -> do
      exists <- doesDirectoryExist out
      unless exists $ die $ "directory: " <> out <> " does not exist"
      writeFile (out ++ "/CBOR.purs") $ toPs cbors
    _ -> do
      die "usage: cabal run hello-world <file_path>"

cbors :: [CBOR]
cbors =
  [ CBOR "paramHello" paramHelloCBOR
  , CBOR "hello" helloWorldHexString
  ]

data CBOR = CBOR{name :: String,cbor :: String}

toPs :: [CBOR] -> String
toPs cs =
  "module CBOR (\n  " <> intercalate ",\n  " (name <$> cs) <> "\n) where\n\n"
  <> intercalate "\n\n" (toDec <$> cs)

toDec :: CBOR -> String
toDec c = name c <> " :: String\n" <> name c <> " = \""
  <> cbor c <> "\""
