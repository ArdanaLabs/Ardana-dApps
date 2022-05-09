module Main where

import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS as Warp (defaultTlsSettings, runTLS)
import Options.Applicative

import CLI (Serve (..), mockServerOptions)
import Network.Kraken.Server.Mock (krakenMockApp)

main :: IO ()
main = do
  execParser (mockServerOptions "kraken") >>= \(Serve priceDataPath port) ->
    runTLS defaultTlsSettings (setPort port defaultSettings) (krakenMockApp priceDataPath)
