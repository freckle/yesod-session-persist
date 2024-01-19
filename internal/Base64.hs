{-# LANGUAGE CPP #-}

module Base64
  ( encodeBase64
  , decodeBase64
  ) where

import Data.ByteString (ByteString)
import Data.Either (Either)
import Data.Function ((.))
import Data.Text (Text)
import Data.Text.Encoding qualified as T

#if MIN_VERSION_base64(1,0,0)
import Data.Base64.Types qualified as I (extractBase64)
import Data.ByteString.Base64.URL qualified as I (decodeBase64Untyped, encodeBase64)
#else
import Data.ByteString.Base64.URL qualified as I (decodeBase64, encodeBase64)
#endif

encodeBase64 :: ByteString -> Text
decodeBase64 :: Text -> Either Text ByteString

#if MIN_VERSION_base64(1,0,0)
encodeBase64 = I.extractBase64 . I.encodeBase64
decodeBase64 = I.decodeBase64Untyped . T.encodeUtf8
#else
encodeBase64 = I.encodeBase64
decodeBase64 = I.decodeBase64 . T.encodeUtf8
#endif
