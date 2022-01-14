-- File auto generated by servant-purescript! --
module Plutus.PAB.Webserver where

import Prelude

import Affjax (defaultRequest, request)
import Affjax.RequestBody (json) as Request
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (json) as Response
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Reader.Class (class MonadAsk, asks)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Argonaut.Encode.Aeson as E
import Data.Array (fromFoldable, null)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.RawJson (RawJson)
import Data.String (joinWith)
import Effect.Aff.Class (class MonadAff, liftAff)
import MarloweContract (MarloweContract)
import Plutus.PAB.Webserver.Types
  ( ContractActivationArgs
  , ContractInstanceClientState
  , ContractSignatureResponse
  , FullReport
  )
import Servant.PureScript
  ( class ToURLPiece
  , AjaxError
  , ErrorDescription(..)
  , toURLPiece
  )
import Wallet.Types (ContractInstanceId)

foreign import encodeURIComponent :: String -> String

type SPSettings_
  =
  { baseURL :: String
  }

class HasSPSettings a where
  spSettings :: a -> SPSettings_

getApiHealthcheck
  :: forall env m
   . HasSPSettings env
  => MonadAsk env m
  => MonadError AjaxError m
  => MonadAff m
  => m Unit
getApiHealthcheck = do
  spSettings <- asks spSettings
  let baseURL = spSettings.baseURL
  let httpMethod = Left GET
  let
    encodeQueryItem :: forall a. ToURLPiece a => String -> a -> String
    encodeQueryItem name val = name <> "=" <> toURLPiece val
  let
    queryArgs :: Array String
    queryArgs =
      []
  let
    queryString = if null queryArgs then "" else "?" <> (joinWith "&" queryArgs)
  let
    reqURL =
      baseURL
        <> "api"
        <> "/"
        <> "healthcheck"
        <> queryString
  let
    reqHeaders =
      [
      ]
  let
    affReq =
      defaultRequest
        { method = httpMethod
        , url = reqURL
        , headers = defaultRequest.headers <> reqHeaders
        , responseFormat = Response.json
        }
  let
    decoder =
      D.unit
  result <- liftAff $ request affReq
  response <- case result of
    Left err -> throwError $
      { request: affReq, description: ConnectingError err }
    Right r -> pure r
  when (unwrap response.status < 200 || unwrap response.status >= 299)
    $ throwError
    $ { request: affReq, description: UnexpectedHTTPStatus response }
  case D.decode decoder response.body of
    Left err -> throwError $ { request: affReq, description: DecodingError err }
    Right body -> pure body

getApiFullreport
  :: forall env m
   . HasSPSettings env
  => MonadAsk env m
  => MonadError AjaxError m
  => MonadAff m
  => m (FullReport MarloweContract)
getApiFullreport = do
  spSettings <- asks spSettings
  let baseURL = spSettings.baseURL
  let httpMethod = Left GET
  let
    encodeQueryItem :: forall a. ToURLPiece a => String -> a -> String
    encodeQueryItem name val = name <> "=" <> toURLPiece val
  let
    queryArgs :: Array String
    queryArgs =
      []
  let
    queryString = if null queryArgs then "" else "?" <> (joinWith "&" queryArgs)
  let
    reqURL =
      baseURL
        <> "api"
        <> "/"
        <> "fullreport"
        <> queryString
  let
    reqHeaders =
      [
      ]
  let
    affReq =
      defaultRequest
        { method = httpMethod
        , url = reqURL
        , headers = defaultRequest.headers <> reqHeaders
        , responseFormat = Response.json
        }
  let
    decoder =
      D.value
  result <- liftAff $ request affReq
  response <- case result of
    Left err -> throwError $
      { request: affReq, description: ConnectingError err }
    Right r -> pure r
  when (unwrap response.status < 200 || unwrap response.status >= 299)
    $ throwError
    $ { request: affReq, description: UnexpectedHTTPStatus response }
  case D.decode decoder response.body of
    Left err -> throwError $ { request: affReq, description: DecodingError err }
    Right body -> pure body

postApiContractActivate
  :: forall env m
   . HasSPSettings env
  => MonadAsk env m
  => MonadError AjaxError m
  => MonadAff m
  => ContractActivationArgs MarloweContract
  -> m ContractInstanceId
postApiContractActivate reqBody = do
  spSettings <- asks spSettings
  let baseURL = spSettings.baseURL
  let httpMethod = Left POST
  let
    encodeQueryItem :: forall a. ToURLPiece a => String -> a -> String
    encodeQueryItem name val = name <> "=" <> toURLPiece val
  let
    queryArgs :: Array String
    queryArgs =
      []
  let
    queryString = if null queryArgs then "" else "?" <> (joinWith "&" queryArgs)
  let
    reqURL =
      baseURL
        <> "api"
        <> "/"
        <> "contract"
        <> "/"
        <> "activate"
        <> queryString
  let
    reqHeaders =
      [
      ]
  let
    affReq =
      defaultRequest
        { method = httpMethod
        , url = reqURL
        , headers = defaultRequest.headers <> reqHeaders
        , responseFormat = Response.json
        , content = Just
            $ Request.json
            $ flip E.encode reqBody
            $ E.value
        }
  let
    decoder =
      D.value
  result <- liftAff $ request affReq
  response <- case result of
    Left err -> throwError $
      { request: affReq, description: ConnectingError err }
    Right r -> pure r
  when (unwrap response.status < 200 || unwrap response.status >= 299)
    $ throwError
    $ { request: affReq, description: UnexpectedHTTPStatus response }
  case D.decode decoder response.body of
    Left err -> throwError $ { request: affReq, description: DecodingError err }
    Right body -> pure body

getApiContractInstanceByContractinstanceidStatus
  :: forall env m
   . HasSPSettings env
  => MonadAsk env m
  => MonadError AjaxError m
  => MonadAff m
  => ContractInstanceId
  -> m (ContractInstanceClientState MarloweContract)
getApiContractInstanceByContractinstanceidStatus contract_instance_id = do
  spSettings <- asks spSettings
  let baseURL = spSettings.baseURL
  let httpMethod = Left GET
  let
    encodeQueryItem :: forall a. ToURLPiece a => String -> a -> String
    encodeQueryItem name val = name <> "=" <> toURLPiece val
  let
    queryArgs :: Array String
    queryArgs =
      []
  let
    queryString = if null queryArgs then "" else "?" <> (joinWith "&" queryArgs)
  let
    reqURL =
      baseURL
        <> "api"
        <> "/"
        <> "contract"
        <> "/"
        <> "instance"
        <> "/"
        <> encodeURIComponent (toURLPiece contract_instance_id)
        <> "/"
        <> "status"
        <> queryString
  let
    reqHeaders =
      [
      ]
  let
    affReq =
      defaultRequest
        { method = httpMethod
        , url = reqURL
        , headers = defaultRequest.headers <> reqHeaders
        , responseFormat = Response.json
        }
  let
    decoder =
      D.value
  result <- liftAff $ request affReq
  response <- case result of
    Left err -> throwError $
      { request: affReq, description: ConnectingError err }
    Right r -> pure r
  when (unwrap response.status < 200 || unwrap response.status >= 299)
    $ throwError
    $ { request: affReq, description: UnexpectedHTTPStatus response }
  case D.decode decoder response.body of
    Left err -> throwError $ { request: affReq, description: DecodingError err }
    Right body -> pure body

getApiContractInstanceByContractinstanceidSchema
  :: forall env m
   . HasSPSettings env
  => MonadAsk env m
  => MonadError AjaxError m
  => MonadAff m
  => ContractInstanceId
  -> m (ContractSignatureResponse MarloweContract)
getApiContractInstanceByContractinstanceidSchema contract_instance_id = do
  spSettings <- asks spSettings
  let baseURL = spSettings.baseURL
  let httpMethod = Left GET
  let
    encodeQueryItem :: forall a. ToURLPiece a => String -> a -> String
    encodeQueryItem name val = name <> "=" <> toURLPiece val
  let
    queryArgs :: Array String
    queryArgs =
      []
  let
    queryString = if null queryArgs then "" else "?" <> (joinWith "&" queryArgs)
  let
    reqURL =
      baseURL
        <> "api"
        <> "/"
        <> "contract"
        <> "/"
        <> "instance"
        <> "/"
        <> encodeURIComponent (toURLPiece contract_instance_id)
        <> "/"
        <> "schema"
        <> queryString
  let
    reqHeaders =
      [
      ]
  let
    affReq =
      defaultRequest
        { method = httpMethod
        , url = reqURL
        , headers = defaultRequest.headers <> reqHeaders
        , responseFormat = Response.json
        }
  let
    decoder =
      D.value
  result <- liftAff $ request affReq
  response <- case result of
    Left err -> throwError $
      { request: affReq, description: ConnectingError err }
    Right r -> pure r
  when (unwrap response.status < 200 || unwrap response.status >= 299)
    $ throwError
    $ { request: affReq, description: UnexpectedHTTPStatus response }
  case D.decode decoder response.body of
    Left err -> throwError $ { request: affReq, description: DecodingError err }
    Right body -> pure body

postApiContractInstanceByContractinstanceidEndpointByEndpointname
  :: forall env m
   . HasSPSettings env
  => MonadAsk env m
  => MonadError AjaxError m
  => MonadAff m
  => RawJson
  -> ContractInstanceId
  -> String
  -> m Unit
postApiContractInstanceByContractinstanceidEndpointByEndpointname
  reqBody
  contract_instance_id
  endpoint_name = do
  spSettings <- asks spSettings
  let baseURL = spSettings.baseURL
  let httpMethod = Left POST
  let
    encodeQueryItem :: forall a. ToURLPiece a => String -> a -> String
    encodeQueryItem name val = name <> "=" <> toURLPiece val
  let
    queryArgs :: Array String
    queryArgs =
      []
  let
    queryString = if null queryArgs then "" else "?" <> (joinWith "&" queryArgs)
  let
    reqURL =
      baseURL
        <> "api"
        <> "/"
        <> "contract"
        <> "/"
        <> "instance"
        <> "/"
        <> encodeURIComponent (toURLPiece contract_instance_id)
        <> "/"
        <> "endpoint"
        <> "/"
        <> encodeURIComponent (toURLPiece endpoint_name)
        <> queryString
  let
    reqHeaders =
      [
      ]
  let
    affReq =
      defaultRequest
        { method = httpMethod
        , url = reqURL
        , headers = defaultRequest.headers <> reqHeaders
        , responseFormat = Response.json
        , content = Just
            $ Request.json
            $ flip E.encode reqBody
            $ E.value
        }
  let
    decoder =
      D.unit
  result <- liftAff $ request affReq
  response <- case result of
    Left err -> throwError $
      { request: affReq, description: ConnectingError err }
    Right r -> pure r
  when (unwrap response.status < 200 || unwrap response.status >= 299)
    $ throwError
    $ { request: affReq, description: UnexpectedHTTPStatus response }
  case D.decode decoder response.body of
    Left err -> throwError $ { request: affReq, description: DecodingError err }
    Right body -> pure body

putApiContractInstanceByContractinstanceidStop
  :: forall env m
   . HasSPSettings env
  => MonadAsk env m
  => MonadError AjaxError m
  => MonadAff m
  => ContractInstanceId
  -> m Unit
putApiContractInstanceByContractinstanceidStop contract_instance_id = do
  spSettings <- asks spSettings
  let baseURL = spSettings.baseURL
  let httpMethod = Left PUT
  let
    encodeQueryItem :: forall a. ToURLPiece a => String -> a -> String
    encodeQueryItem name val = name <> "=" <> toURLPiece val
  let
    queryArgs :: Array String
    queryArgs =
      []
  let
    queryString = if null queryArgs then "" else "?" <> (joinWith "&" queryArgs)
  let
    reqURL =
      baseURL
        <> "api"
        <> "/"
        <> "contract"
        <> "/"
        <> "instance"
        <> "/"
        <> encodeURIComponent (toURLPiece contract_instance_id)
        <> "/"
        <> "stop"
        <> queryString
  let
    reqHeaders =
      [
      ]
  let
    affReq =
      defaultRequest
        { method = httpMethod
        , url = reqURL
        , headers = defaultRequest.headers <> reqHeaders
        , responseFormat = Response.json
        }
  let
    decoder =
      D.unit
  result <- liftAff $ request affReq
  response <- case result of
    Left err -> throwError $
      { request: affReq, description: ConnectingError err }
    Right r -> pure r
  when (unwrap response.status < 200 || unwrap response.status >= 299)
    $ throwError
    $ { request: affReq, description: UnexpectedHTTPStatus response }
  case D.decode decoder response.body of
    Left err -> throwError $ { request: affReq, description: DecodingError err }
    Right body -> pure body

getApiContractInstancesWalletByWalletid
  :: forall env m
   . HasSPSettings env
  => MonadAsk env m
  => MonadError AjaxError m
  => MonadAff m
  => String
  -> Maybe String
  -> m (Array (ContractInstanceClientState MarloweContract))
getApiContractInstancesWalletByWalletid wallet_id status = do
  spSettings <- asks spSettings
  let baseURL = spSettings.baseURL
  let httpMethod = Left GET
  let
    encodeQueryItem :: forall a. ToURLPiece a => String -> a -> String
    encodeQueryItem name val = name <> "=" <> toURLPiece val
  let
    queryArgs :: Array String
    queryArgs =
      []
        <> fromFoldable (encodeQueryItem "status" <$> status)
  let
    queryString = if null queryArgs then "" else "?" <> (joinWith "&" queryArgs)
  let
    reqURL =
      baseURL
        <> "api"
        <> "/"
        <> "contract"
        <> "/"
        <> "instances"
        <> "/"
        <> "wallet"
        <> "/"
        <> encodeURIComponent (toURLPiece wallet_id)
        <> queryString
  let
    reqHeaders =
      [
      ]
  let
    affReq =
      defaultRequest
        { method = httpMethod
        , url = reqURL
        , headers = defaultRequest.headers <> reqHeaders
        , responseFormat = Response.json
        }
  let
    decoder =
      D.value
  result <- liftAff $ request affReq
  response <- case result of
    Left err -> throwError $
      { request: affReq, description: ConnectingError err }
    Right r -> pure r
  when (unwrap response.status < 200 || unwrap response.status >= 299)
    $ throwError
    $ { request: affReq, description: UnexpectedHTTPStatus response }
  case D.decode decoder response.body of
    Left err -> throwError $ { request: affReq, description: DecodingError err }
    Right body -> pure body

getApiContractInstances
  :: forall env m
   . HasSPSettings env
  => MonadAsk env m
  => MonadError AjaxError m
  => MonadAff m
  => Maybe String
  -> m (Array (ContractInstanceClientState MarloweContract))
getApiContractInstances status = do
  spSettings <- asks spSettings
  let baseURL = spSettings.baseURL
  let httpMethod = Left GET
  let
    encodeQueryItem :: forall a. ToURLPiece a => String -> a -> String
    encodeQueryItem name val = name <> "=" <> toURLPiece val
  let
    queryArgs :: Array String
    queryArgs =
      []
        <> fromFoldable (encodeQueryItem "status" <$> status)
  let
    queryString = if null queryArgs then "" else "?" <> (joinWith "&" queryArgs)
  let
    reqURL =
      baseURL
        <> "api"
        <> "/"
        <> "contract"
        <> "/"
        <> "instances"
        <> queryString
  let
    reqHeaders =
      [
      ]
  let
    affReq =
      defaultRequest
        { method = httpMethod
        , url = reqURL
        , headers = defaultRequest.headers <> reqHeaders
        , responseFormat = Response.json
        }
  let
    decoder =
      D.value
  result <- liftAff $ request affReq
  response <- case result of
    Left err -> throwError $
      { request: affReq, description: ConnectingError err }
    Right r -> pure r
  when (unwrap response.status < 200 || unwrap response.status >= 299)
    $ throwError
    $ { request: affReq, description: UnexpectedHTTPStatus response }
  case D.decode decoder response.body of
    Left err -> throwError $ { request: affReq, description: DecodingError err }
    Right body -> pure body

getApiContractDefinitions
  :: forall env m
   . HasSPSettings env
  => MonadAsk env m
  => MonadError AjaxError m
  => MonadAff m
  => m (Array (ContractSignatureResponse MarloweContract))
getApiContractDefinitions = do
  spSettings <- asks spSettings
  let baseURL = spSettings.baseURL
  let httpMethod = Left GET
  let
    encodeQueryItem :: forall a. ToURLPiece a => String -> a -> String
    encodeQueryItem name val = name <> "=" <> toURLPiece val
  let
    queryArgs :: Array String
    queryArgs =
      []
  let
    queryString = if null queryArgs then "" else "?" <> (joinWith "&" queryArgs)
  let
    reqURL =
      baseURL
        <> "api"
        <> "/"
        <> "contract"
        <> "/"
        <> "definitions"
        <> queryString
  let
    reqHeaders =
      [
      ]
  let
    affReq =
      defaultRequest
        { method = httpMethod
        , url = reqURL
        , headers = defaultRequest.headers <> reqHeaders
        , responseFormat = Response.json
        }
  let
    decoder =
      D.value
  result <- liftAff $ request affReq
  response <- case result of
    Left err -> throwError $
      { request: affReq, description: ConnectingError err }
    Right r -> pure r
  when (unwrap response.status < 200 || unwrap response.status >= 299)
    $ throwError
    $ { request: affReq, description: UnexpectedHTTPStatus response }
  case D.decode decoder response.body of
    Left err -> throwError $ { request: affReq, description: DecodingError err }
    Right body -> pure body