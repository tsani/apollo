{-|
 - This module adds servant combinators.
 -}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGuAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Apollo.Types.Servant
( -- * Core API
  QueryParam'
, OnParseFail(..)
, Requirement(..)
, ParamValue(..)
  -- * Helpers
, StrictQueryParam
, maybeParamValue
) where

import Apollo.Reflection

import Control.Monad ( join )
import Data.String.Conversions ( cs )
import GHC.TypeLits
import Servant.Server.Internal
import Web.HttpApiData ( FromHttpApiData, parseQueryParamMaybe, ToHttpApiData )
import Network.HTTP.Types.URI ( parseQueryText )
import Network.Wai ( rawQueryString )
import Servant.API ( (:>), QueryParam )
import Servant.Utils.Links ( IsElem, IsElem', HasLink(..) )

------------------------------------------------------------------------
--- Core API                                                         ---
------------------------------------------------------------------------

data QueryParam' (req :: Requirement) (act :: OnParseFail) (sym :: Symbol) a

-- | Whether the parameter is required or optional.
--
-- If a parameter is optional, then its /underlying value/ (as determined by
-- what happens when parse fails) is wrapped with 'Maybe'.
data Requirement = Required | Optional

-- | What do to if the parameter couldn't be parsed, but was present.
--
-- If we pass on parse failures, then the /underlying value/ (as determined by
-- the 'FromHttpApiData' instance) is wrapped with 'ParamValue'.
data OnParseFail = Pass | Error400

-- | A wrapper for parameters whose parsing may fail.
data ParamValue a
  = ParamOk a
  | ParamFailed
  deriving (Eq, Ord, Read, Show, Functor)

------------------------------------------------------------------------
--- Helpers                                                          ---
------------------------------------------------------------------------

-- | A strict query parameter is one whose presence is required and that must
-- correctly parse. This is essentially equivalent to a capture, but as a query
-- string.
type StrictQueryParam = QueryParam' 'Required 'Error400

-- | Converts to @Maybe@.
maybeParamValue :: ParamValue a -> Maybe a
maybeParamValue = \case
  ParamOk x -> Just x
  ParamFailed -> Nothing

------------------------------------------------------------------------
--- Links integration                                                ---
------------------------------------------------------------------------

-- | A required query param in an API will only match endpoitns also containing
-- a required query param. Furthermore, the names and decoded types must match.
type instance IsElem'
  (QueryParam' 'Required _ x a :> e)
  (QueryParam' 'Required _ x a :> api)
  = IsElem e api

-- | An optional query param can be skipped.
type instance IsElem' e (QueryParam' 'Optional _ x a :> api) = IsElem e api

type family Require (req :: Requirement) (t :: *) :: * where
  Require 'Required a = a
  Require 'Optional a = Maybe a

instance
  ( KnownSymbol sym
  , ToHttpApiData a
  , HasLink sub
  )
  => HasLink (QueryParam' req act sym a :> sub) where

  type MkLink (QueryParam' req act sym a :> sub) = a -> MkLink sub

  toLink _ l x = toLink qp l (Just x) where
    qp = Proxy :: Proxy (QueryParam sym a :> sub)

------------------------------------------------------------------------
--- Implementation details                                           ---
------------------------------------------------------------------------

data OnParseFailS :: OnParseFail -> * where
  PassS :: OnParseFailS 'Pass
  Error400S :: OnParseFailS 'Error400

type instance Demote' ('KProxy :: KProxy OnParseFail) = OnParseFailS

instance ReflectS 'Pass where
  reflectS _ = PassS

instance ReflectS 'Error400 where
  reflectS _ = Error400S

instance Proxiable OnParseFailS where
  proxy = \case
    PassS -> Proxy
    Error400S -> Proxy

data RequirementS :: Requirement -> * where
  RequiredS :: RequirementS 'Required
  OptionalS :: RequirementS 'Optional

type instance Demote' ('KProxy :: KProxy Requirement) = RequirementS

instance ReflectS 'Required where
  reflectS _ = RequiredS

instance ReflectS 'Optional where
  reflectS _ = OptionalS

instance Proxiable RequirementS where
  proxy = \case
    RequiredS -> Proxy
    OptionalS -> Proxy

addQueryCheck
  :: Delayed env (a -> b)
  -> DelayedIO a
  -> Delayed env b
addQueryCheck Delayed{..} new = Delayed
  { capturesD = \env -> (,) <$> capturesD env <*> new
  , serverD = \(caps, a) auth body req -> ($ a) <$> serverD caps auth body req
  , ..
  }

type family PassedParam (req :: Requirement) (act :: OnParseFail) (t :: *) :: * where
  PassedParam 'Required 'Error400 a = a
  PassedParam 'Required 'Pass a = ParamValue a
  PassedParam 'Optional 'Error400 a = Maybe a
  PassedParam 'Optional 'Pass a = Maybe (ParamValue a)

instance
  ( KnownSymbol sym
  , FromHttpApiData a
  , HasServer api context
  , ReflectS req
  , ReflectS act
  )
  => HasServer (QueryParam' req act sym a :> api) context where

  type ServerT (QueryParam' req act sym a :> api) m
    = PassedParam req act a -> ServerT api m

  route Proxy context subserver = route api context server where
    api = Proxy :: Proxy api
    server = addQueryCheck subserver $ withRequest $ \req -> do
      let paramname = cs (symbolVal (Proxy :: Proxy sym))
      let m = fmap parseQueryParamMaybe <$> lookup paramname (parseQueryText (rawQueryString req))
            :: Maybe (Maybe (Maybe a))
      --       |      |      +------->  whether parse succeeded
      --       |      +-------------->  whether there was an equals in the QS
      --       +--------------------->  whether lookup succeeded
      --
      -- We can collapse the inner two maybes, by considering the value being
      -- not present as equivalent to a parse failure.
      let m' = join <$> m

      case (reflectS (Proxy :: Proxy req), reflectS (Proxy :: Proxy act), m') of
        (_, Error400S, Just Nothing)
          -- we got the query string param but it didn't parse, and we require
          -- successful parse
          -> delayedFail err400
        (RequiredS, _, Nothing)
          -- query string param is required and we didn't get it
          -> delayedFail err400
        (OptionalS, Error400S, Nothing)
          -- query string param is optional and we didn't get it, so return
          -- nothing
          -> pure Nothing
        (OptionalS, PassS, Nothing)
          -- query string param is optional and we didn't get it, so return
          -- nothing
          -> pure Nothing
        (OptionalS, PassS, Just Nothing)
          -> pure (Just ParamFailed)
        (RequiredS, PassS, Just Nothing)
          -- query string param is required, we got it but it didn't parse, but
          -- successful parse isn't needed
          -> pure ParamFailed
        (OptionalS, Error400S, Just (Just x))
          -> pure (Just x)
        (RequiredS, Error400S, Just (Just x))
          -> pure x
        (RequiredS, PassS, Just (Just x))
          -> pure (ParamOk x)

        (OptionalS, PassS, Just (Just x))
          -- query string param is optional and we got it but
          -> pure (Just (ParamOk x))
