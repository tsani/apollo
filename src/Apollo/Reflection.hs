{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Apollo.Reflection
( Demote'
, Demote
, ReflectS(..)
, Proxiable(..)
, Proxy(..)
, KProxy(..)
) where

import Data.Proxy

-- | Type level function that selects a canonical type constructor for a given
-- kind. Generally, the selected type constructor consist of singletons, taking
-- each type in the kind @k@ to a distinct type.
type family Demote' (p :: KProxy k) :: k -> *

-- | The proxy argument can be inferred if a concrete type of kind @k@ is
-- available.
type Demote (a :: k) = Demote' ('KProxy :: KProxy k)

-- | Types in some kind @k@ that can be reflected into values.
class ReflectS (a :: k) where
  reflectS :: Proxy a -> Demote a a

-- | Class of type constructors that can be demoted to proxies.
--
-- Useful for converting various things into proxies without needing to write
-- explicit type synonyms.
class Proxiable (s :: k -> *) where
  proxy :: s a -> Proxy a

instance Proxiable [] where
  proxy _ = Proxy

instance Proxiable Maybe where
  proxy _ = Proxy
