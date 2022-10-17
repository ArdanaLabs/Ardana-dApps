module Main(main) where

import Contract.Prelude
import CBOR(trivial)


main :: Effect Unit
main = log trivial
