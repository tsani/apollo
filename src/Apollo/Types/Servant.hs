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
  StrictQueryParam
) where

import Apollo.Reflection

import Control.Monad ( join )
import Data.Monoid ( (<>) )
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

-- | A strict query parameter is one whose presence is required and that must
-- correctly parse. This is essentially equivalent to a capture, but as a query
-- string.
data StrictQueryParam (sym :: Symbol) (ty :: *)

------------------------------------------------------------------------
--- Links integration                                                ---
------------------------------------------------------------------------

-- | A required query param in an API will only match endpoitns also containing
-- a required query param. Furthermore, the names and decoded types must match.
type instance IsElem'
  (StrictQueryParam sym ty :> e)
  (StrictQueryParam sym ty :> api)
  = IsElem e api

instance
  ( KnownSymbol sym
  , ToHttpApiData a
  , HasLink sub
  )
  => HasLink (StrictQueryParam sym a :> sub) where

  type MkLink (StrictQueryParam sym a :> sub) x = a -> MkLink sub x

  toLink f _ l x = toLink f qp l (Just x) where
    qp = Proxy :: Proxy (QueryParam sym a :> sub)

------------------------------------------------------------------------
--- Implementation details                                           ---
------------------------------------------------------------------------

instance
  ( KnownSymbol sym
  , FromHttpApiData a
  , HasServer api context
  )
  => HasServer (StrictQueryParam sym a :> api) context where

  type ServerT (StrictQueryParam sym a :> api) m = a -> ServerT api m

  route Proxy context subserver =
    let queryText r = parseQueryText $ rawQueryString r
        param r = let p = lookup paramname (queryText r) in
          case join $ parseQueryParamMaybe <$> join p of
            Nothing -> delayedFailFatal err400
              { errBody = cs $
                "parameter " <> paramname <> " is absent or has no value"
              }
            Just e -> case e of
              Left err -> delayedFailFatal err400
                { errBody = cs $
                  "failed to parse parameter " <> paramname <> ": " <> err
                }
              Right x -> pure x
        delayed = addParameterCheck subserver . withRequest $ param
    in route (Proxy :: Proxy api) context delayed
    where
      paramname = cs $ symbolVal (Proxy :: Proxy sym)
