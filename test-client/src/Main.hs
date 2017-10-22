{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Prelude                                    ()
import           Prelude.Compat

import           Control.Arrow                              (left)
import           Data.Aeson
import           Data.Char                                  (chr, isPrint)
import           Data.Foldable                              (forM_)
import           Data.Monoid                                hiding (getLast)
import           Data.Proxy
import           GHC.Generics                               (Generic)
import qualified Network.HTTP.Types                         as HTTP
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck
import           Web.FormUrlEncoded                         (FromForm, ToForm)

import           Servant.API                                ((:<|>) ((:<|>)),
                                                             (:>), AuthProtect,
                                                             BasicAuth,
                                                             BasicAuthData (..),
                                                             Capture,
                                                             CaptureAll,
                                                             DeleteNoContent,
                                                             EmptyAPI,
                                                             FormUrlEncoded,
                                                             Get, Header,
                                                             Headers, JSON,
                                                             NoContent (NoContent),
                                                             Post, Raw,
                                                             QueryFlag,
                                                             QueryParam,
                                                             QueryParams,
                                                             ReqBody,
                                                             getHeaders)
import           Servant.Client.Ghcjs
import qualified Servant.Client.Core.Internal.Request as Req
import qualified Servant.Client.Core.Internal.Auth as Auth

main :: IO ()
main = hspec spec


spec :: Spec
spec = describe "Servant.Client.Ghcjs" $ do
    sucessSpec
    failSpec
    basicAuthSpec
    genAuthSpec

-- * test data types

data Person = Person
  { _name :: String
  , _age  :: Integer
  } deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

instance ToForm Person
instance FromForm Person

alice :: Person
alice = Person "Alice" 42

type TestHeaders = '[Header "X-Example1" Int, Header "X-Example2" String]

type SuccessApi =
       "get" :> Get '[JSON] Person
  :<|> "deleteEmpty" :> DeleteNoContent '[JSON] NoContent
  :<|> "capture" :> Capture "name" String :> Get '[JSON,FormUrlEncoded] Person
  :<|> "captureAll" :> CaptureAll "names" String :> Get '[JSON] [Person]
  :<|> "body" :> ReqBody '[FormUrlEncoded,JSON] Person :> Post '[JSON] Person
  :<|> "param" :> QueryParam "name" String :> Get '[FormUrlEncoded,JSON] Person
  :<|> "params" :> QueryParams "names" String :> Get '[JSON] [Person]
  :<|> "flag" :> QueryFlag "flag" :> Get '[JSON] Bool
  :<|> "rawSuccess" :> Raw
  :<|> "rawFailure" :> Raw
  :<|> "multiple" :>
            Capture "first" String :>
            QueryParam "second" Int :>
            QueryFlag "third" :>
            ReqBody '[JSON] [(String, [Rational])] :>
            Post '[JSON] (String, Maybe Int, Bool, [(String, [Rational])])
  :<|> "headers" :> Get '[JSON] (Headers TestHeaders Bool)
  :<|> "deleteContentType" :> DeleteNoContent '[JSON] NoContent
  :<|> "empty" :> EmptyAPI

successApi :: Proxy ("success" :> SuccessApi)
successApi = Proxy

getGet          :: ClientM Person
getDeleteEmpty  :: ClientM NoContent
getCapture      :: String -> ClientM Person
getCaptureAll   :: [String] -> ClientM [Person]
getBody         :: Person -> ClientM Person
getQueryParam   :: Maybe String -> ClientM Person
getQueryParams  :: [String] -> ClientM [Person]
getQueryFlag    :: Bool -> ClientM Bool
getRawSuccess   :: HTTP.Method -> ClientM Response
getRawFailure   :: HTTP.Method -> ClientM Response
getMultiple     :: String -> Maybe Int -> Bool -> [(String, [Rational])]
  -> ClientM (String, Maybe Int, Bool, [(String, [Rational])])
getRespHeaders  :: ClientM (Headers TestHeaders Bool)
getDeleteContentType :: ClientM NoContent

getGet
  :<|> getDeleteEmpty
  :<|> getCapture
  :<|> getCaptureAll
  :<|> getBody
  :<|> getQueryParam
  :<|> getQueryParams
  :<|> getQueryFlag
  :<|> getRawSuccess
  :<|> getRawFailure
  :<|> getMultiple
  :<|> getRespHeaders
  :<|> getDeleteContentType
  :<|> EmptyClient = client successApi

type FailApi =
       "get" :> Get '[JSON] Person
  :<|> "deleteEmpty" :> DeleteNoContent '[JSON] NoContent
  :<|> "capture" :> Capture "name" String :> Get '[JSON,FormUrlEncoded] Person
  :<|> "body" :> ReqBody '[FormUrlEncoded,JSON] Person :> Post '[JSON] Person

failGetGet          :: ClientM Person
failGetDeleteEmpty  :: ClientM NoContent
failGetCapture      :: String -> ClientM Person
failGetBody         :: Person -> ClientM Person

failApi :: Proxy ("fail" :> FailApi)
failApi = Proxy

failGetGet
  :<|> failGetDeleteEmpty
  :<|> failGetCapture
  :<|> failGetBody = client failApi

-- * basic auth stuff

type BasicAuthApi =
       BasicAuth "foo-realm" () :> "private" :> "basic" :> Get '[JSON] Person

basicAuthApi :: Proxy ("basicauth" :> BasicAuthApi)
basicAuthApi = Proxy

getBasic :: Client ClientM BasicAuthApi
getBasic = client basicAuthApi

-- * general auth stuff

type GenAuthApi =
  AuthProtect "auth-tag" :> "private" :> "auth" :> Get '[JSON] Person

genAuthApi :: Proxy ("genauth" :> GenAuthApi)
genAuthApi = Proxy

getProtected :: Client ClientM GenAuthApi
getProtected = client genAuthApi

type instance Auth.AuthClientData (AuthProtect "auth-tag") = ()

sucessSpec :: Spec
sucessSpec = do

    it "Servant.API.Get" $ \() -> do
      left show <$> runClientM getGet `shouldReturn` Right alice

    describe "Servant.API.Delete" $ do
      it "allows empty content type" $ \() -> do
        left show <$> runClientM getDeleteEmpty `shouldReturn` Right NoContent

      it "allows content type" $ \() -> do
        left show <$> runClientM getDeleteContentType `shouldReturn` Right NoContent

    it "Servant.API.Capture" $ \() -> do
      left show <$> runClientM (getCapture "Paula") `shouldReturn` Right (Person "Paula" 0)

    it "Servant.API.CaptureAll" $ \() -> do
      let expected = [Person "Paula" 0, Person "Peta" 1]
      left show <$> runClientM (getCaptureAll ["Paula", "Peta"]) `shouldReturn` Right expected

    it "Servant.API.ReqBody" $ \() -> do
      let p = Person "Clara" 42
      left show <$> runClientM (getBody p) `shouldReturn` Right p

    it "Servant.API.QueryParam" $ \() -> do
      left show <$> runClientM (getQueryParam (Just "alice"))  `shouldReturn` Right alice
      Left (FailureResponse r) <- runClientM (getQueryParam (Just "bob"))
      responseStatusCode r `shouldBe` HTTP.Status 400 "bob not found"

    it "Servant.API.QueryParam.QueryParams" $ \() -> do
      left show <$> runClientM (getQueryParams []) `shouldReturn` Right []
      left show <$> runClientM (getQueryParams ["alice", "bob"])
        `shouldReturn` Right [Person "alice" 0, Person "bob" 1]

    context "Servant.API.QueryParam.QueryFlag" $
      forM_ [False, True] $ \ flag -> it (show flag) $ \() -> do
        left show <$> runClientM (getQueryFlag flag) `shouldReturn` Right flag

    it "Servant.API.Raw on success" $ \() -> do
      res <- runClientM (getRawSuccess HTTP.methodGet)
      case res of
        Left e -> assertFailure $ show e
        Right r -> do
          responseStatusCode r `shouldBe` HTTP.status200
          responseBody r `shouldBe` "rawSuccess"

    it "Servant.API.Raw should return a Left in case of failure" $ \() -> do
      res <- runClientM (getRawFailure HTTP.methodGet)
      case res of
        Right _ -> assertFailure "expected Left, but got Right"
        Left (FailureResponse r) -> do
          responseStatusCode r `shouldBe` HTTP.status400
          responseBody r `shouldBe` "rawFailure"
        Left e -> assertFailure $ "expected FailureResponse, but got " ++ show e

    it "Returns headers appropriately" $ \() -> do
      res <- runClientM getRespHeaders
      case res of
        Left e -> assertFailure $ show e
        Right val -> getHeaders val `shouldBe` [("X-Example1", "1729"), ("X-Example2", "eg2")]

    modifyMaxSuccess (const 20) $ do
      it "works for a combination of Capture, QueryParam, QueryFlag and ReqBody" $ \() ->
        property $ forAllShrink pathGen shrink $ \(NonEmpty cap) num flag body ->
          ioProperty $ do
            result <- left show <$> runClientM (getMultiple cap num flag body)
            return $
              result === Right (cap, num, flag, body)

failSpec :: Spec
failSpec = do
    context "client returns errors appropriately" $ do
      it "reports FailureResponse" $ \() -> do
        Left res <- runClientM failGetDeleteEmpty
        case res of
          -- Expect 405 because the server also runs a raw endpoint to serve
          -- static files
          FailureResponse r | responseStatusCode r == HTTP.status405 -> return ()
          _ -> fail $ "expected 404 response, but got " <> show res

      it "reports DecodeFailure" $ \() -> do
        Left res <- runClientM (failGetCapture "foo")
        case res of
          DecodeFailure _ _ -> return ()
          _ -> fail $ "expected DecodeFailure, but got " <> show res

      it "reports UnsupportedContentType" $ \() -> do
        Left res <- runClientM failGetGet
        case res of
          UnsupportedContentType ("application/octet-stream") _ -> return ()
          _ -> fail $ "expected UnsupportedContentType, but got " <> show res

      it "reports InvalidContentTypeHeader" $ \() -> do
        Left res <- runClientM (failGetBody alice)
        case res of
          InvalidContentTypeHeader _ -> return ()
          _ -> fail $ "expected InvalidContentTypeHeader, but got " <> show res

basicAuthSpec :: Spec
basicAuthSpec = do
  context "Authentication works when requests are properly authenticated" $ do

    it "Authenticates a BasicAuth protected server appropriately" $ \() -> do
      let basicAuthData = BasicAuthData "servant" "server"
      left show <$> runClientM (getBasic basicAuthData) `shouldReturn` Right alice

  context "Authentication is rejected when requests are not authenticated properly" $ do

    it "Authenticates a BasicAuth protected server appropriately" $ \() -> do
      let basicAuthData = BasicAuthData "not" "password"
      Left (FailureResponse r) <- runClientM (getBasic basicAuthData)
      responseStatusCode r `shouldBe` HTTP.Status 403 "Forbidden"

genAuthSpec :: Spec
genAuthSpec = do
  context "Authentication works when requests are properly authenticated" $ do

    it "Authenticates a AuthProtect protected server appropriately" $ \() -> do
      let authRequest = Auth.mkAuthenticatedRequest () (\_ req -> Req.addHeader "AuthHeader" ("cool" :: String) req)
      left show <$> runClientM (getProtected authRequest) `shouldReturn` Right alice

  context "Authentication is rejected when requests are not authenticated properly" $ do

    it "Authenticates a AuthProtect protected server appropriately" $ \() -> do
      let authRequest = Auth.mkAuthenticatedRequest () (\_ req -> Req.addHeader "Wrong" ("header" :: String) req)
      Left (FailureResponse r) <- runClientM (getProtected authRequest)
      responseStatusCode r `shouldBe` HTTP.Status 401 "Unauthorized"

pathGen :: Gen (NonEmptyList Char)
pathGen = fmap NonEmpty path
 where
  path = listOf1 $ elements $
    filter (not . (`elem` ("?%[]/#;" :: String))) $
    filter isPrint $
    map chr [0..127]
