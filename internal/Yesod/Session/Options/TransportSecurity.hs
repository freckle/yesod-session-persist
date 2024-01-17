module Yesod.Session.Options.TransportSecurity
  ( TransportSecurity (..)
  , cookieSecure
  ) where

import Internal.Prelude

data TransportSecurity
  = -- | Only allow cookies on HTTPS connections
    --
    -- Set this in production.
    RequireSecureTransport
  | -- | Allow cookies over either HTTP or HTTPS
    --
    -- This is okay for development.
    AllowPlaintextTranport

cookieSecure :: TransportSecurity -> Bool
cookieSecure = \case
  RequireSecureTransport -> True
  AllowPlaintextTranport -> False
