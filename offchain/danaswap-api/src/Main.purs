module Main (main) where

import Contract.Prelude

main :: Effect Unit
main = log "The api isn't really runable. Maybe you want to run the tests with purs-nix test?"
-- This seems better than throwing a module not found error on purs-nix run
