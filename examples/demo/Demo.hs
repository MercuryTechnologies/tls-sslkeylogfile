-- SPDX-FileCopyrightText: 2025 Mercury Technologies, Inc
--
-- SPDX-License-Identifier: MIT

import Network.HTTP.Client (Response (..), httpLbs, parseRequest)
import Network.TLS.SSLKeyLogFile

main :: IO ()
main = do
  man <- makeManager

  req <- parseRequest "https://example.com/index.html"
  resp <- httpLbs req man
  putStrLn $ "The status code was: " ++ show (responseStatus resp)
