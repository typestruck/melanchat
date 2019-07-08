module Client.Common(
	setItem,
	post,
	post',
	addEventListener,
	querySelector,
	value,
	setLocation,
	alert,
	tokenKey,
	search
) where

import Prelude

import Affjax as A
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.ResponseFormat as RF
import Control.Monad.Error.Class as CMEC
import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Either (Either(..))
import Data.Either as DE
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.MediaType (MediaType(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception as EE
import Type.Data.Boolean (kind Boolean)
import Web.DOM.Document as WDD
import Web.DOM.Element (Element)
import Web.DOM.Element as WDE
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode as WDP
import Web.Event.Event (EventType)
import Web.Event.EventTarget as WET
import Web.Event.Internal.Types (Event)
import Web.HTML as WH
import Web.HTML.HTMLDocument as WHHD
import Web.HTML.HTMLInputElement as WHHI
import Web.HTML.Location as WHL
import Web.HTML.Window as WHW
import Web.Storage.Storage as WSS

tokenKey :: String
tokenKey = "token"

-- | Adds an event to the given element.
addEventListener :: forall a . Element -> EventType -> (Event -> Effect a) -> Effect Unit
addEventListener element eventType handler = do
	listener <- WET.eventListener handler
	WET.addEventListener eventType listener false $ WDE.toEventTarget element

-- | Selects a single element.
querySelector :: String -> Effect Element
querySelector selector = do
	window <- WH.window
	document <- WHHD.toDocument <$> WHW.document window
	maybeElement <- WDP.querySelector (QuerySelector selector) $ WDD.toParentNode document
	DM.maybe (EE.throwException $ EE.error $ "Selector returned no nodes:" <> selector) pure maybeElement

value :: Element -> Effect String
value element = DM.maybe inputException WHHI.value $ WHHI.fromElement element
	where inputException = do
		id <- WDE.id element
		EE.throwException <<< EE.error $ "Element is not an input type" <> id

alert :: String -> Effect Unit
alert message = do
	window <- WH.window
	WHW.alert message window

-- | A simplified version of post without the option to handle errors
post' :: forall contents c response r. Generic contents c => EncodeRep c => Generic response r => DecodeRep r => String -> contents -> Aff response
post' url data' = do
	response <- post url data'
	case response of
		Right right -> pure right
		Left error -> parseJSONError $ A.printResponseFormatError error

-- | Performs a POST request
post :: forall contents c response r. Generic contents c => EncodeRep c => Generic response r => DecodeRep r => String -> contents -> Aff (Either ResponseFormatError response)
post url data' = do
	--see Token in shared/Types.purs
	token <- liftEffect $ getItem tokenKey
	request url POST [RequestHeader "x-access-token" token] data'

-- | Performs a HTTP request with a JSON payload
request :: forall contents c response r. Generic contents c => EncodeRep c => Generic response r => DecodeRep r => String -> Method -> Array RequestHeader -> contents -> Aff (Either ResponseFormatError response)
request url method extraHeaders data' = do
	response <- A.request $ A.defaultRequest {
			url = url,
			method = Left method,
			responseFormat = RF.json,
			headers = [
				Accept $ MediaType "application/json",
				ContentType $ MediaType "application/json"
			] <> extraHeaders,
			content = Just <<< RB.json $ DAEGR.genericEncodeJson data'
		}
	case response.body of
		Right payload -> DE.either parseJSONError (pure <<< Right) $ DADGR.genericDecodeJson payload
		Left left -> pure $ Left left

parseJSONError message = do
	liftEffect $ alert message
	CMEC.throwError <<< EE.error $ "Could not parse json: " <> message

setLocation :: String -> Effect Unit
setLocation url = do
	window <- WH.window
	location <- WHW.location window
	WHL.setHref url location

setItem :: String -> String -> Effect Unit
setItem key itemValue = do
	window <- WH.window
	localStorage <- WHW.localStorage window
	WSS.setItem key itemValue localStorage

getItem :: String  -> Effect String
getItem key = do
	window <- WH.window
	localStorage <- WHW.localStorage window
	DM.fromMaybe "" <$> WSS.getItem key localStorage

search :: Effect String
search = do
	window <- WH.window
	location <- WHW.location window
	WHL.search location
