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

module Main where

import           Prelude                                    ()
import           Prelude.Compat
import           Control.Monad.Error.Class                  (throwError)
import           Data.Aeson
import           Data.Proxy
import           GHC.Generics                               (Generic)
import qualified Network.HTTP.Types                         as HTTP
import qualified Network.Wai                                as Wai
import           Network.Wai.Handler.Warp
import           Web.FormUrlEncoded                         (FromForm, ToForm)
import           Servant.API                                ((:<|>) ((:<|>)),
                                                             (:>), AuthProtect,
                                                             BasicAuth,
                                                             BasicAuthData (..),
                                                             Capture,
                                                             CaptureAll,
                                                             DeleteNoContent,
                                                             EmptyAPI, addHeader,
                                                             FormUrlEncoded,
                                                             Get, Header,
                                                             Headers, JSON,
                                                             NoContent (NoContent),
                                                             Post, Raw,
                                                             QueryFlag,
                                                             QueryParam,
                                                             QueryParams,
                                                             ReqBody)
import           Servant.Server
import           Servant (serveDirectoryFileServer)
import           Servant.Server.Experimental.Auth
import           System.Environment (getArgs)

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
       -- Note: Changed multiple to Post, since browsers don't allow request
       -- bodies in Get requests
  :<|> "multiple" :>
            Capture "first" String :>
            QueryParam "second" Int :>
            QueryFlag "third" :>
            ReqBody '[JSON] [(String, [Rational])] :>
            Post '[JSON] (String, Maybe Int, Bool, [(String, [Rational])])
  :<|> "headers" :> Get '[JSON] (Headers TestHeaders Bool)
  :<|> "deleteContentType" :> DeleteNoContent '[JSON] NoContent
  :<|> "empty" :> EmptyAPI

successApi :: Proxy SuccessApi
successApi = Proxy

successServer :: Server SuccessApi
successServer =
       return alice
  :<|> return NoContent
  :<|> (\ name -> return $ Person name 0)
  :<|> (\ names -> return (zipWith Person names [0..]))
  :<|> return
  :<|> (\ name -> case name of
                   Just "alice" -> return alice
                   Just n -> throwError $ ServantErr 400 (n ++ " not found") "" []
                   Nothing -> throwError $ ServantErr 400 "missing parameter" "" [])
  :<|> (\ names -> return (zipWith Person names [0..]))
  :<|> return
  :<|> (Tagged $ \ _request respond -> respond $ Wai.responseLBS HTTP.ok200 [] "rawSuccess")
  :<|> (Tagged $ \ _request respond -> respond $ Wai.responseLBS HTTP.badRequest400 [] "rawFailure")
  :<|> (\ a b c d -> return (a, b, c, d))
  :<|> (return $ addHeader 1729 $ addHeader "eg2" True)
  :<|> return NoContent
  :<|> emptyServer

type FailApi =
       "get" :> Raw
  :<|> "capture" :> Capture "name" String :> Raw
  :<|> "body" :> Raw
failApi :: Proxy FailApi
failApi = Proxy

failServer :: Server FailApi
failServer =
       (Tagged $ \ _request respond -> respond $ Wai.responseLBS HTTP.ok200 [] "")
  :<|> (\ _capture -> Tagged $ \_request respond -> respond $ Wai.responseLBS HTTP.ok200 [("content-type", "application/json")] "")
  :<|> (Tagged $ \_request respond -> respond $ Wai.responseLBS HTTP.ok200 [("content-type", "fooooo")] "")


-- * basic auth stuff

type BasicAuthApi =
       BasicAuth "foo-realm" () :> "private" :> "basic" :> Get '[JSON] Person

basicAuthApi :: Proxy BasicAuthApi
basicAuthApi = Proxy

basicAuthHandler :: BasicAuthCheck ()
basicAuthHandler =
  let check (BasicAuthData username password) =
        if username == "servant" && password == "server"
        then return (Authorized ())
        else return Unauthorized
  in BasicAuthCheck check

basicServerContext :: Context '[ BasicAuthCheck () ]
basicServerContext = basicAuthHandler :. EmptyContext

basicAuthServer :: Server BasicAuthApi
basicAuthServer = const (return alice)

-- * general auth stuff

type GenAuthApi =
  AuthProtect "auth-tag" :> "private" :> "auth" :> Get '[JSON] Person

genAuthApi :: Proxy GenAuthApi
genAuthApi = Proxy

type instance AuthServerData (AuthProtect "auth-tag") = ()

genAuthHandler :: AuthHandler Wai.Request ()
genAuthHandler =
  let handler req = case lookup "AuthHeader" (Wai.requestHeaders req) of
        Nothing -> throwError (err401 { errBody = "Missing auth header" })
        Just _ -> return ()
  in mkAuthHandler handler

genAuthServerContext :: Context '[ AuthHandler Wai.Request () ]
genAuthServerContext = genAuthHandler :. EmptyContext

genAuthServer :: Server GenAuthApi
genAuthServer = const (return alice)


type RealApi
   =    "success"   :> SuccessApi
   :<|> "fail"      :> FailApi
   :<|> "basicauth" :> BasicAuthApi
   :<|> "genauth"   :> GenAuthApi
   :<|> Raw

realApi :: Proxy RealApi
realApi = Proxy

realContext :: Context '[ BasicAuthCheck (), AuthHandler Wai.Request () ]
realContext = basicAuthHandler :. genAuthHandler :. EmptyContext

realServer :: FilePath -> Application
realServer clientPath = serveWithContext realApi realContext (
    successServer   :<|>
    failServer      :<|>
    basicAuthServer :<|>
    genAuthServer   :<|>
    serveDirectoryFileServer clientPath)


main :: IO ()
main = do
  [clientpath] <- getArgs
  run 8000 $ realServer clientpath
