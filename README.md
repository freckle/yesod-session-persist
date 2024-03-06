# yesod-session-persist

[![Hackage](https://img.shields.io/hackage/v/yesod-session-persist.svg?style=flat)](https://hackage.haskell.org/package/yesod-session-persist)
[![CI](https://github.com/freckle/yesod-session-persist/actions/workflows/ci.yml/badge.svg)](https://github.com/freckle/yesod-session-persist/actions/workflows/ci.yml)

Use this package to construct a Yesod session backend for which sessions are
stored in a backend data store.

## Features

### Key rotation

The key reason to switch from client-side sessions (Yesod's default) to server storage
is to be able to rotate keys and invalidate old credentials.

With client session storage, when a user logs out, you send them a new cookie.
But this does nothing to satisfy a user who is logging out because their session secret may
have been compromised; the old cookie value will still be a working authentication credential.
Being able to _revoke_ authentication credentials requires storing state on the server.

Whenever user's authentication changes (but especially on logging out), users of this library
should use the `rotateSessionKey` action to provoke a key rotation.
This copies any existing session data into a new session with a different secret key,
deleting the session with the old key and thus disabling any outdated credentials that
an attacker may possess.

### Disabling session changes

There may be some unusual circumstances in which you want to disable the effects of session
management -- writes to the session backend and sending of session cookies -- for the
handling of a particular request.
At such times, you can use the `assignSessionFreeze` action to indicate whether the
session should be persisted at the end of the handling of the request.

### Expiration by idle timeout

The most recent access time of each session is stored. After a configurable duration has
elapsed without access, a session is considered to be expired. An expired session is treated
as if it did not exist.

### Expiration by absolute timeout

The creation time of each session is stored. After a configurable duration has elapsed since
the creation time, a session is considered to be expired, regardless of whether it is still
in active use.

### Approximate storage of access time

To avoid excessive database writes, updates which would only increment a session's access
time by a short duration are not performed.
The definition of "a short duration" is configurable; we call it the _timeout resolution_.

## Absent features

### Garbage collection

Garbage collection is supported when using `memcache` as the data store. Please see 'Yesod.Session.Memcache.Storage.SessionPersistence'.

The `Yesod.Session.Persist` module _does not_ does not proactively seek out expired sessions for deletion. Thus, in the absence of some other intervention, your session table will grow without bound.

## Prior art

### `serversession`

This package is based on
[serversession](https://hackage.haskell.org/package/serversession) +
[serversession-frontend-yesod](https://hackage.haskell.org/package/serversession-frontend-yesod) +
[serversession-backend-persistent](https://hackage.haskell.org/package/serversession-backend-persistent).

Compared to `serversession`, here we simplify somewhat by concretizing to Yesod and
Persistent rather than supporting multiple frontends and backends.

Their sessions have a concept of "auth ID" specifying who is logged in.
`serversession` uses this to automatically rotate keys when the auth ID changes, and
to provide a means for mass invalidation of all the sessions belonging to a particular user.
We do not borrow this concept, because it does not generalize well to more complex
authentication situations where a session may have been authenticated as multiple principals.

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
