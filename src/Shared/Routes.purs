module Shared.Routes (routes) where

import Prelude

import Data.String as DS
import Data.Symbol (class IsSymbol)
import Payload.Client (defaultOpts)
import Payload.Client.Internal.Url (class EncodeUrl)
import Payload.Client.Queryable (class EncodeOptionalQuery, class EncodeUrlWithParams, encodeOptionalQuery, encodeUrlWithParams)
import Payload.Internal.Route (DefaultParentRoute, DefaultRouteSpec)
import Payload.Spec (Spec, Route(Route), Routes)
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.Symbol (class Append)
import Prim.Symbol as Symbol
import Record as R
import Shared.Spec (spec)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

--refactor: actually give this any thought

--this is a hack adapted from payload source code, as I didn't see an builtin/easier way to do it
routes ∷ _
routes = makeRoutes spec

makeRoutes ∷ ∀ r routesSpec routesSpecList client. RowToList routesSpec routesSpecList ⇒ ToRouteList routesSpecList "" () (Record client) ⇒ Spec { routes ∷ Record routesSpec | r } → Record client
makeRoutes _ = makeRouteList (Proxy ∷ _ routesSpecList) (Proxy ∷ _ "") (Proxy ∷ _ (Record ()))

type ToString payload = payload → String

class ToRouteList (routesSpecList ∷ RowList Type) (basePath ∷ Symbol) (baseParams ∷ Row Type) client | routesSpecList → client where
      makeRouteList ∷ Proxy routesSpecList → Proxy basePath → Proxy (Record baseParams) → client

instance ToRouteList Nil basePath baseParams (Record ()) where
      makeRouteList _ _ _ = {}

instance
      ( IsSymbol parentName
      , IsSymbol basePath
      , IsSymbol path
      , EncodeUrl path childParams
      , Union parentSpec DefaultParentRoute mergedSpec
      , Nub mergedSpec parentSpecWithDefaults
      , TypeEquals (Record parentSpecWithDefaults) { params ∷ Record parentParams, guards ∷ parentGuards | childRoutes }
      , Union baseParams parentParams childParams
      , Cons parentName (Record childClient) remClient client
      , RowToList childRoutes childRoutesList
      , Append basePath path childBasePath
      , ToRouteList childRoutesList childBasePath childParams (Record childClient)
      , Lacks parentName remClient
      , ToRouteList remRoutes basePath baseParams (Record remClient)
      ) ⇒
      ToRouteList (Cons parentName (Routes path (Record parentSpec)) remRoutes) basePath baseParams (Record client) where
      makeRouteList _ basePath baseParams =
            R.insert (Proxy ∷ _ parentName) childRoutes $ makeRouteList (Proxy ∷ _ remRoutes) basePath baseParams
            where
            childRoutes = makeRouteList (Proxy ∷ _ childRoutesList) (Proxy ∷ _ childBasePath) (Proxy ∷ _ (Record childParams))

instance
      ( IsSymbol routeName
      , IsSymbol path
      , Cons routeName (ToString payload) remClient client
      , Lacks routeName remClient
      , ToUrlString (Route "GET" path routeSpec) basePath baseParams payload
      , ToRouteList remRoutes basePath baseParams (Record remClient)
      ) ⇒
      ToRouteList (Cons routeName (Route "GET" path routeSpec) remRoutes) basePath baseParams (Record client) where
      makeRouteList _ _ _ = R.insert (Proxy ∷ _ routeName) asString rest
            where
            rest = makeRouteList (Proxy ∷ _ remRoutes) (Proxy ∷ _ basePath) (Proxy ∷ _ (Record baseParams))
            asString ∷ ToString payload
            asString payload =
                  makeUrlString (Route ∷ Route "GET" path routeSpec) (Proxy ∷ _ basePath) (Proxy ∷ _ (Record baseParams)) payload

else instance
      ( Lacks routeName client
      , ToRouteList remRoutes basePath baseParams (Record client)
      ) ⇒
      ToRouteList (Cons routeName (Route method path routeSpec) remRoutes) basePath baseParams (Record client) where
      makeRouteList _ _ _ = makeRouteList (Proxy ∷ _ remRoutes) (Proxy ∷ _ basePath) (Proxy ∷ _ (Record baseParams))

class ToUrlString route (basePath ∷ Symbol) (baseParams ∷ Row Type) payload | route baseParams basePath → payload where
      makeUrlString ∷ route → Proxy basePath → Proxy (Record baseParams) → ToString payload

instance
      ( Lacks "body" route
      , Union route DefaultRouteSpec mergedRoute
      , Nub mergedRoute routeWithDefaults
      , TypeEquals (Record routeWithDefaults)
              { params ∷ Record params
              , query ∷ query
              | r
              }
      , Union baseParams params fullUrlParams
      , Symbol.Append basePath path fullPath
      , RowToList fullUrlParams fullParamsList
      , EncodeUrlWithParams fullPath fullParamsList payload
      , EncodeOptionalQuery fullPath query payload
      ) ⇒
      ToUrlString (Route "GET" path (Record route)) basePath baseParams (Record payload) where
      makeUrlString _ _ _ payload =
            let
                  urlPath = encodeUrlWithParams defaultOpts (Proxy ∷ _ fullPath) (Proxy ∷ _ fullParamsList) payload
                  urlQuery = encodeOptionalQuery (Proxy ∷ _ fullPath) (Proxy ∷ _ query) payload
            in
                  (if DS.null urlPath then "/" else urlPath) <> urlQuery
