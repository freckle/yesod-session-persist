module Yesod.Session.Persist.SessionKeySpec
  ( spec
  ) where

import Yesod.Session.Persist.Test.Prelude

import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)

spec :: Spec
spec = context "SessionKeyManager" $ do
  specify "generates 24-character text"
    $ forAll (genMockInit id)
    $ \mockInit -> ioProperty $ do
      Mock {sessionManager} <- newMock id mockInit
      sessionKey <- newSessionKey sessionManager
      pure $ T.length sessionKey.text == 24

  specify "uses only letters, numbers, dash, underscore"
    $ forAll (genMockInit id)
    $ \mockInit -> ioProperty $ do
      Mock {sessionManager} <- newMock id mockInit
      sessionKey <- newSessionKey sessionManager
      let charactersPresent = Set.fromList (T.unpack sessionKey.text)
      pure $ charactersPresent `Set.isSubsetOf` charactersWanted

  specify "never generates the same key twice"
    $ forAll (genMockInit id)
    $ \mockInit -> ioProperty $ do
      Mock {sessionManager} <- newMock id mockInit
      let n = 1000
      sessionKeys <-
        fmap Set.fromList
          $ replicateM n
          $ newSessionKey sessionManager
      pure $ Set.size sessionKeys == n

  specify "accepts its own keys"
    $ forAll (genMockInit id)
    $ \mockInit -> ioProperty $ do
      Mock {sessionManager} <- newMock id mockInit
      sessionKey <- newSessionKey sessionManager
      pure $ sessionKeyAppearsReasonable sessionManager sessionKey

  specify "does not accept invalid keys"
    $ forAll (genMockInit id)
    $ \mockInit -> ioProperty $ do
      Mock {sessionManager} <- newMock id mockInit
      pure
        $ all
          (isNothing . checkedSessionKeyFromCookieValue sessionManager)
          someInvalidCookies

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
