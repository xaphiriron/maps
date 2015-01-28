module Utility.Tuple where

import Control.Applicative

liftSnd :: Functor f => (a, f b) -> f (a, b)
liftSnd (a, fb) = fmap ((,) a) fb

liftFst :: Functor f => (f a, b) -> f (a, b)
liftFst (fa, b) = fmap (flip (,) b) fa

liftTuple :: Applicative f => (f a, f b) -> f (a, b)
liftTuple (fa, fb) = (,) <$> fa <*> fb
