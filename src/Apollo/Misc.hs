module Apollo.Misc where

import Data.Bool ( bool )

(<#>) :: Functor f => f a -> (a -> b) -> f b
(<#>) = flip fmap

infixl 4 <#>

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$) -- really !

infixl 4 $>

untilM :: Monad m => m Bool -> m ()
untilM m = m >>= bool (untilM m) (pure ())
