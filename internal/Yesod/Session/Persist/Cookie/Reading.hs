module Yesod.Session.Persist.Cookie.Reading
  ( findSessionKey
  )
where

import Yesod.Session.Persist.Prelude

import Network.HTTP.Types.Header
import Network.Wai
import Web.Cookie

-- | Find a session key in the request
findSessionKey :: ByteString -> Request -> Maybe ByteString
findSessionKey cookieNameBS =
  one
    . concatMap (lookupAll cookieNameBS . parseCookies)
    . lookupAll hCookie
    . requestHeaders

one :: [a] -> Maybe a
one = \case [x] -> Just x; _ -> Nothing

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll a = fmap snd . filter ((== a) . fst)
