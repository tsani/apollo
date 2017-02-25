module Apollo.Misc where

(<#>) :: Functor f => f a -> (a -> b) -> f b
(<#>) = flip fmap

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$) -- really !
