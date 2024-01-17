module Yesod.Session.Cookie.SetCookie
  ( makeSetCookieHeaders
  ) where

import Internal.Prelude

import Data.Text.Encoding (encodeUtf8)
import Session.Timing.Math
import Session.Timing.Options
import Session.Timing.Time
import Session.Timing.Timeout
import Session.TransportSecurity qualified as TransportSecurity
import Time
import Web.Cookie qualified as C
import Yesod.Core.Types (Header (AddCookie))
import Yesod.Session.Key
import Yesod.Session.Options

makeSetCookieHeaders
  :: Options tx m -> Maybe (SessionKey, Time UTCTime) -> [Header]
makeSetCookieHeaders options =
  (: []) <$> maybe (deleteCookie options) (createCookie options)

cookieNameBS :: Options tx m -> ByteString
cookieNameBS options = encodeUtf8 options.cookieName

-- | Create a cookie for the given session
createCookie :: Options tx m -> (SessionKey, Time UTCTime) -> Header
createCookie options (key, time) =
  AddCookie
    C.def
      { C.setCookieName = cookieNameBS options
      , C.setCookieValue = sessionKeyToCookieValue key
      , C.setCookiePath = Just "/"
      , C.setCookieExpires = Just $ cookieExpires options.timing.timeout time
      , C.setCookieDomain = Nothing
      , C.setCookieHttpOnly = True
      , C.setCookieSecure = TransportSecurity.cookieSecure options.transportSecurity
      }

-- | Remove the session cookie from the client
deleteCookie :: Options tx m -> Header
deleteCookie options =
  AddCookie
    C.def
      { C.setCookieName = cookieNameBS options
      , C.setCookieValue = ""
      , C.setCookiePath = Just "/"
      , C.setCookieExpires = Just $ UTCTime systemEpochDay 1
      , C.setCookieMaxAge = Just 0
      , C.setCookieDomain = Nothing
      , C.setCookieHttpOnly = True
      , C.setCookieSecure = TransportSecurity.cookieSecure options.transportSecurity
      }

-- | Calculate the date that should be used for the cookie's "expires" field
cookieExpires :: Timeout NominalDiffTime -> Time UTCTime -> UTCTime
cookieExpires timeout time =
  fromMaybe (addUTCTime (years 10) time.accessed)
    $ nextExpires timeout time
