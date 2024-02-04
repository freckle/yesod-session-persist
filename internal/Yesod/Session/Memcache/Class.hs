module Yesod.Session.Memcache.Class
  (HasMemcacheClient (..)
  )
where

import Database.Memcache.Client as Memcache

class HasMemcacheClient env where
  getMemcacheClient :: env -> Memcache.Client
