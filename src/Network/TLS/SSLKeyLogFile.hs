-- SPDX-FileCopyrightText: 2025 Mercury Technologies, Inc
--
-- SPDX-License-Identifier: MIT

-- | Implements @SSLKEYLOGFILE@ support: logs keys to the file specified in the
-- @SSLKEYLOGFILE@ environment variable. Completely inactive when the
-- environment variable is not set.
--
-- This is used for inspecting TLS traffic in-flight in combination with a
-- packet dump, with a primary use case of debugging.
--
-- For easier usage, you can embed the key log in the pcap and Wireshark will
-- decrypt it without having to fiddle with any settings:
--
-- > $ editcap --inject-secrets tls,mykeylog.tls_keys input.pcap output.pcapng
--
-- - See: <https://www.ietf.org/archive/id/draft-thomson-tls-keylogfile-00.html>
-- - See: <https://jade.fyi/blog/announcing-clipper/>
module Network.TLS.SSLKeyLogFile (
  -- * Demo and workflow

  -- | There is a demonstration of the workflow of using tls-sslkeylogfile in
  -- the [README on GitHub](https://github.com/MercuryTechnologies/tls-sslkeylogfile#readme)
  -- including all the steps necessary to capture packets and analyze their
  -- decrypted versions.

  -- * Security considerations

  -- | See the RFC: <https://www.ietf.org/archive/id/draft-thomson-tls-keylogfile-00.html#name-security-considerations-8>
  --
  -- This package should be thought of the same as @gdb@ in terms of security. If
  -- the person invoking your software is passing @SSLKEYLOGFILE@, they could
  -- have just as well run it under @gdb@ and gotten the keys out of it without
  -- this package.
  --
  -- The combination of keys extracted from this and traffic decrypted using
  -- those keys, in the absence of session resumption (off by default in hs-tls),
  -- only impacts encryption sessions that were executed while keys were being
  -- logged and no past or future sessions. This assumes that obscure
  -- features like exporters (not supported by hs-tls anyhow as of
  -- 2025-03-06) are not used in a manner that impacts confidentiality of
  -- past or future sessions.
  --
  -- Logging the session keys for debugging has no impact on the security
  -- properties or behaviour of TLS as visible to other hosts; it just allows you
  -- to decrypt traffic as someone who already has full access to the process.

  -- * Functions
  makeManager,
  MakeTLSSettingsParams (..),
  makeTLSSettingsWithKeyLogging,
  addKeyLoggingToClientParams,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Default (Default (..))
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.TLS (ClientParams (..), DebugParams (..), Shared (..), Supported (..), defaultParamsClient)
import Network.TLS qualified as TLS
import Network.TLS.Extra.Cipher qualified as TLS
import System.Environment (lookupEnv)
import System.IO (IOMode (..), hFlush, hPutStrLn, openFile)
import System.X509 (getSystemCertificateStore)

-- | This is the equivalent of 'TLSSettingsSimple' with only the actually-used
-- fields implemented.
data MakeTLSSettingsParams = MakeTLSSettingsParams
  { disableCertificateValidation :: Bool
  -- ^ Whether to ignore server certificates and not validate them. This is
  -- obviously insecure to set to 'True'.
  --
  -- __Default__: 'False'.
  , clientSupported :: Supported
  -- ^ Value of 'Supported' to use when constructing the 'ClientParams'.
  -- Used for e.g. allowing legacy TLS 1.2 services without Extended Main
  -- Secret support in versions of the Haskell @tls@ library >= 2.0.
  --
  -- __Default__: @def \@Supported {supportedCiphers = TLS.ciphersuite_default}@
  }
  deriving stock (Show)

instance Default MakeTLSSettingsParams where
  def =
    MakeTLSSettingsParams
      { disableCertificateValidation = False
      , clientSupported = def {supportedCiphers = TLS.ciphersuite_default}
      }

-- | Creates a 'TLSSettings' with the system CA trust store and the default
-- cipher suites. This function is for those who want a 'TLSSettings' value that
-- works in normal use cases.
--
-- See @Network.Connection.makeTLSParams@ in @crypton-connection@ for the code
-- this function replicates.
makeTLSSettingsWithKeyLogging :: MakeTLSSettingsParams -> IO TLSSettings
makeTLSSettingsWithKeyLogging MakeTLSSettingsParams {disableCertificateValidation, clientSupported} = do
  caStore <- getSystemCertificateStore

  TLSSettings
    <$> addKeyLoggingToClientParams
      ( (defaultParamsClient "" "")
          { clientShared =
              (def @Shared)
                { sharedCAStore = caStore
                , sharedValidationCache = validationCache
                }
          , TLS.clientSupported
          }
      )
  where
    validationCache
      | disableCertificateValidation =
          TLS.ValidationCache
            (\_ _ _ -> return TLS.ValidationCachePass)
            (\_ _ _ -> return ())
      | otherwise = def

-- | Creates a 'Manager' with TLS support, with the default configuration, with
-- key logging available.
makeManager :: IO Manager
makeManager = do
  tlsSettings <- makeTLSSettingsWithKeyLogging def
  newManager $ mkManagerSettings tlsSettings Nothing

-- | Adds key logging support to the given ClientParams using the
-- @SSLKEYLOGFILE@ environment variable. This is an advanced function. Most use
-- cases should use 'makeTLSSettings' or 'makeManager' to create a
-- 'TLSSettings' or a 'Manager' which works.
--
-- __IMPORTANT NOTE__: If you haven't put a 'sharedCAStore' or 'supportedCiphers'
-- into the given ClientParams, it will not make a connection successfully!
--
-- By default, @crypton-connection@ uses a 'TLSSettingsSimple' into which the CA
-- store and cipher suites are injected during connection setup to make a
-- 'TLSSettings' containing an appropriate 'ClientParams' that can actually
-- establish a TLS connection. By using this function, you are necessarily
-- bypassing this logic and constructing your /own/ 'ClientParams', which,
-- unless you specifically add some, will not have any trusted certification
-- authorities or allowed cipher suites and will thus fail to establish any
-- connection.
addKeyLoggingToClientParams :: ClientParams -> IO ClientParams
addKeyLoggingToClientParams params = do
  keyLogFileEnv <- lookupEnv "SSLKEYLOGFILE"

  -- XXX(jade): due to API design limitations in hs-tls, there's no way to have
  -- a properly managed lifetime for the file handle. we do just leak it. it's
  -- fine, it either gets closed or not (in which case it gets closed on process
  -- exit), but it won't get eaten by the GC or anything either way.

  keyLog <- case keyLogFileEnv of
    Just f -> do
      hand <- openFile f AppendMode
      pure $ \line -> liftIO (hPutStrLn hand line >> hFlush hand)
    Nothing -> do
      pure $ \_ -> pure ()

  pure
    params
      { clientDebug =
          (def @DebugParams)
            { debugKeyLogger = keyLog
            }
      }
