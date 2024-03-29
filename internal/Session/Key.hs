module Session.Key
  ( SessionKey (..)
  , SessionKeyManager (..)
  , makeSessionKeyManager
  , sessionKeyToCookieValue
  , sessionKeyFromCookieValue
  )
where

import Internal.Prelude

import Base64 (decodeBase64, encodeBase64)
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Randomization

data SessionKeyManager m = SessionKeyManager
  { new :: m SessionKey
  -- ^ Generate a new session key
  --
  -- In a production setting, it is critical that this action be
  -- thread-safe and produce a securely random result.
  , check :: SessionKey -> Bool
  -- ^ Validate that a text is something that plausibly could have
  --   been generated by 'new'.
  }

-- | Secret value that is sent to and subsequently furnished by
--   the client to identify the session
newtype SessionKey = SessionKey {text :: Text}
  deriving newtype (Eq, Ord, Show)

makeSessionKeyManager :: Monad m => Randomization m -> SessionKeyManager m
makeSessionKeyManager (Randomization generateRandomBytes) =
  let
    new =
      SessionKey
        . encodeBase64
        <$> generateRandomBytes keyLengthInBytes

    check (SessionKey text) =
      T.length text
        == keyLengthAsText
        && either
          (const False)
          ((== keyLengthInBytes) . BS8.length)
          (decodeBase64 text)
  in
    SessionKeyManager {new, check}

-- We generate 18-byte session keys. This number is rather arbitrary.
keyLengthInBytes :: Integral a => a
keyLengthInBytes = 18

-- 18 bytes in base64 encoding ends up being a text 24 characters
keyLengthAsText :: Integral a => a
keyLengthAsText = 24

sessionKeyToCookieValue :: SessionKey -> ByteString
sessionKeyToCookieValue = (.text) >>> encodeUtf8

sessionKeyFromCookieValue :: ByteString -> Maybe SessionKey
sessionKeyFromCookieValue v =
  decodeUtf8' v & either (const Nothing) Just <&> SessionKey
