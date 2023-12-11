module Web.Session.SessionKeySpec
  ( spec
  ) where

import TestPrelude

import Web.Session.SessionManager

import Data.Set qualified as Set
import Data.Text qualified as T

spec :: Spec
spec = context "SessionKeyManager" $ do
  specify "generates 24-character text" $ hedgehog $ do
    mock <- newMock defaultMockOptions
    sessionKey <- newSessionKey mock.sessionManager
    T.length sessionKey.text === 24

  specify "uses only letters, numbers, dash, underscore" $ hedgehog $ do
    mock <- newMock defaultMockOptions
    sessionKey <- newSessionKey mock.sessionManager
    let charactersPresent = Set.fromList (T.unpack sessionKey.text)
    assert $ charactersPresent `Set.isSubsetOf` charactersWanted

  modifyArgs (\x -> x {maxSuccess = 1})
    $ specify "never generates the same key twice"
    $ hedgehog
    $ do
      mock <- newMock defaultMockOptions
      let n = 1000
      sessionKeys <-
        fmap Set.fromList
          $ replicateM n
          $ newSessionKey mock.sessionManager
      Set.size sessionKeys === n

  specify "accepts its own keys" $ hedgehog $ do
    mock <- newMock defaultMockOptions
    sessionKey <- newSessionKey mock.sessionManager
    assert $ sessionKeyAppearsReasonable mock.sessionManager sessionKey

  specify "does not accept invalid keys" $ hedgehog $ do
    mock <- newMock defaultMockOptions
    for_ someInvalidCookies $ \c ->
      checkedSessionKeyFromCookieValue mock.sessionManager c
        === Nothing

charactersWanted :: Set Char
charactersWanted =
  foldMap Set.fromList [['a' .. 'z'], ['A' .. 'Z'], ['0' .. '9'], "-_"]

someInvalidSessionKeyTexts :: [Text]
someInvalidSessionKeyTexts =
  [ ""
  , "123456789-123456789-123"
  , "123456789-123456789-12345"
  , "aaaaaaaaaaaaaaaaaa*aaaaa"
  ]

someInvalidCookies :: [ByteString]
someInvalidCookies =
  (encodeUtf8 <$> someInvalidSessionKeyTexts)
    <> someInvalidUtf8ByteStrings

someInvalidUtf8ByteStrings :: [ByteString]
someInvalidUtf8ByteStrings =
  [ "\xc3\x28"
  , "\xa0\xa1"
  , "\xe2\x28\xa1"
  , "\xe2\x82\x28"
  , "\xf0\x28\x8c\xbc"
  , "\xf0\x90\x28\xbc"
  , "\xf0\x28\x8c\x28"
  ]
