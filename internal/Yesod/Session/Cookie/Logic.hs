module Yesod.Session.Cookie.Logic
  ( setCookie
  , CookieContext (..)
  ) where

import Internal.Prelude

import Yesod.Core.Types (Header)
import Yesod.Session.Cookie.SetCookie
import Yesod.Session.Options
import Yesod.Session.SaveResult
import Yesod.Session.SessionType

data CookieContext = CookieContext
  { cookie :: Maybe ByteString
  , load :: Maybe Session
  , save :: SaveResult Session
  }

setCookie :: Options tx m -> CookieContext -> [Header]
setCookie options = \case
  CookieContext {save = Frozen} ->
    -- Never send anything when a freeze was requested.
    []
  CookieContext {save = Deleted} ->
    -- There was a session but it's now gone; send deletion
    -- cookies so the client can forget all about it.
    cookiesForSession Nothing
  CookieContext {save = Saved s} ->
    -- Any time a session was saved, send cookies.
    -- At the very least this will probably be wanted to give
    -- the client a new expiration time.
    cookiesForSession (Just s)
  CookieContext {save = NoChange, load = Just s} ->
    -- There was a session loaded but change saved; send cookies
    -- for the session that was loaded. This is probably superfluous.
    -- It will only be useful if the server's timeout settings
    -- have changed. But it's low cost, so might as well do it.
    cookiesForSession (Just s)
  CookieContext {save = NoChange, load = Nothing, cookie = Nothing} ->
    -- No cookie was sent, no session was loaded, no change was saved.
    -- There is nothing to send.
    []
  CookieContext {save = NoChange, load = Nothing, cookie = Just _} ->
    -- The client sent a cookie but it did not result in a session load,
    -- and there is no new session key to send. Send a deletion to put
    -- this worthless session cookie out of its misery.
    cookiesForSession Nothing
 where
  cookiesForSession :: Maybe Session -> [Header]
  cookiesForSession = makeSetCookieHeaders options . fmap (\s -> (s.key, s.time))
