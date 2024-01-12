module Yesod.Session.Persist.YesodSpec
  ( spec
  ) where

import Yesod.Session.Persist.Test.Prelude

import Control.Concurrent.STM.TVar (readTVarIO)
import Data.Aeson (encode, object)
import Network.Wai
import Network.Wai.Test (simpleHeaders)
import Web.Cookie
import Yesod.Session.Persist.YesodApp
import Yesod.Test

import Data.List qualified as List
import Data.Sequence qualified as Seq

spec :: Spec
spec =
  withApp defaultTimingOptions $ do
    context "Yesod App" $ do
      specify @(YesodExample App ()) "Sets a session cookie" $ do
        app <- getTestYesod
        now <- liftIO $ readTVarIO app.mock.currentTime

        -- Make a request to a normal route
        request $ do setUrl HomeR; setMethod "GET"

        -- Since there was no session, one should be inserted
        sessionKey :: SessionKey <- do
          transcript <- liftIO $ takeTranscript app.mock.mockStorage
          case toList transcript of
            [StorageOperation' (InsertSession s)] -> pure s.key
            _ -> liftIO $ fail $ show transcript

        -- We should receive a set-cookie header
        assertSetCookie
          $ def
            { setCookieName = "session-key"
            , setCookieValue = encodeUtf8 sessionKey.text
            , setCookiePath = Just "/"
            , setCookieExpires =
                Just
                  $ truncateToSeconds
                  $ addUTCTime (60 * 60 * 8) now
            , setCookieHttpOnly = True
            }

      specify @(YesodExample App ())
        "Doesn't set a session cookie if sessions are disabled"
        $ do
          app <- getTestYesod

          -- Make a request to a route without session management
          request $ do setUrl PingR; setMethod "GET"

          -- No storage operations should have been performed
          transcript <- liftIO $ takeTranscript app.mock.mockStorage
          liftIO $ transcript `shouldBe` Seq.empty

          -- No cookie should be set
          assertNoSetCookie

      specify @(YesodExample App ()) "Saves the session"
        $ do
          app <- getTestYesod

          -- Log in
          request $ do
            setUrl LogInR
            setMethod "POST"
            setRequestBody $ encode $ object [("uid", "xyz")]

          sessionKey :: SessionKey <- do
            transcript <- liftIO $ takeTranscript app.mock.mockStorage
            case toList transcript of
              [StorageOperation' (InsertSession s)] -> pure s.key
              _ -> liftIO $ fail $ show transcript

          replicateM_ 3 $ do
            -- A short pause should not affect anything
            advanceTime 90 app.mock

            -- Verify that we're now logged in
            request $ do setUrl UserR; setMethod "GET"
            bodyEquals (show @ByteString "xyz")

            liftIO $ do
              transcript <- takeTranscript app.mock.mockStorage
              toList transcript
                `shouldBe` [StorageOperation' (GetSession sessionKey)]

      specify @(YesodExample App ()) "Does not load an expired session"
        $ do
          app <- getTestYesod

          request $ do
            setUrl LogInR
            setMethod "POST"
            setRequestBody $ encode $ object [("uid", "xyz")]

          -- A pause longer than the idle timeout should kill the session
          advanceTime (60 * 60 * 10) app.mock

          request $ do setUrl UserR; setMethod "GET"
          bodyEquals "-"

      specify @(YesodExample App ()) "rotates the key when 'rotateSessionKey' is used" $ do
        app <- getTestYesod

        -- Log in
        request $ do
          setUrl LogInR
          setMethod "POST"
          setRequestBody $ encode $ object [("uid", "xyz")]

        -- Get the session
        transcript <- liftIO $ takeTranscript app.mock.mockStorage
        sessionKey :: SessionKey <-
          case toList transcript of
            [StorageOperation' (InsertSession s)] -> pure s.key
            _ -> liftIO $ fail $ show transcript

        -- Make a request to the route that does a key rotation
        request $ do setUrl RotateR; setMethod "GET"

        -- The old session should be deleted
        transcript' <- liftIO $ takeTranscript app.mock.mockStorage
        liftIO
          $ List.take 2 (toList transcript')
          `shouldBe` [ StorageOperation' (GetSession sessionKey)
                     , StorageOperation' (DeleteSession sessionKey)
                     ]

        -- But we're still logged in
        request $ do setUrl UserR; setMethod "GET"
        bodyEquals (show @ByteString "xyz")

      specify @(YesodExample App ()) "rotates the key on auth changes"
        $ do
          app <- getTestYesod

          -- Log in
          request $ do
            setUrl LogInR
            setMethod "POST"
            setRequestBody $ encode $ object [("uid", "xyz")]

          -- Get the session
          transcript <- liftIO $ takeTranscript app.mock.mockStorage
          sessionKey :: SessionKey <-
            case toList transcript of
              [StorageOperation' (InsertSession s)] -> pure s.key
              _ -> liftIO $ fail $ show transcript

          -- Log in differently
          request $ do
            setUrl LogInR
            setMethod "POST"
            setRequestBody $ encode $ object [("uid", "hello")]

          -- The old session should be deleted
          transcript' <- liftIO $ takeTranscript app.mock.mockStorage
          liftIO
            $ List.take 2 (toList transcript')
            `shouldBe` [ StorageOperation' (GetSession sessionKey)
                       , StorageOperation' (DeleteSession sessionKey)
                       ]

          -- But we're still logged in as the new user
          request $ do setUrl UserR; setMethod "GET"
          bodyEquals (show @ByteString "hello")

withApp :: TimingOptions NominalDiffTime -> SpecWith (App, Middleware) -> Spec
withApp timing = around ((=<< newApp timing) . (. (,id :: Middleware)))

-- | Assert that the response contains a set-cookie header that parses to a particular value
assertSetCookie :: SetCookie -> YesodExample site ()
assertSetCookie expected =
  withResponse $ \response ->
    case List.lookup "set-cookie" $ simpleHeaders response of
      Just value -> liftIO $ parseSetCookie value `shouldBe` expected
      Nothing -> liftIO $ expectationFailure "no set-cookie header present"

-- | Fail if set-cookie is present in the response
assertNoSetCookie :: YesodExample site ()
assertNoSetCookie =
  withResponse $ \response ->
    case List.lookup "set-cookie" $ simpleHeaders response of
      Just value ->
        liftIO $ expectationFailure $ "expected no set-cookie, but got " <> show value
      Nothing -> pure ()

-- | Round a time to seconds, because that is the precision in cookie headers
truncateToSeconds :: UTCTime -> UTCTime
truncateToSeconds x =
  x {utctDayTime = secondsToDiffTime (truncate (utctDayTime x))}
