module Yesod.Session.Persist.Cookie.SetCookie
  ( makeSetCookieHeaders
  ) where

import Yesod.Session.Persist.Prelude

import Yesod.Session.Persist.Options
import Yesod.Session.Persist.SessionKey
import Yesod.Session.Persist.Timing.Math
import Yesod.Session.Persist.Timing.Options
import Yesod.Session.Persist.Timing.Time
import Yesod.Session.Persist.Timing.Timeout

import Yesod.Core.Types (Header (AddCookie))

import Web.Cookie qualified as C
import Yesod.Session.Persist.TransportSecurity qualified as TransportSecurity

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
 where
  years = (* 365.2) . days
  days = (* 24) . hours
  hours = (* 60) . minutes
  minutes = (* 60)
