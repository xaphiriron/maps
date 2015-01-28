module Data.BTree
	( BTree(..)
	, unfoldTree
	, unfoldTreeM
	) where

import Control.Applicative
import Data.Maybe (fromMaybe)

data BTree a
	= BNode a (BTree a) (BTree a)
	| Leaf
	deriving (Eq, Show, Read)

instance Functor BTree where
	fmap f Leaf = Leaf
	fmap f (BNode a lb rb) = BNode (f a) (fmap f lb) (fmap f rb)

unfoldTree :: (b -> (a, (Maybe b, Maybe b))) -> b -> BTree a
unfoldTree f b = let (a, (mleft, mright)) = f b
	in BNode a
		(fromMaybe Leaf $ unfoldTree f <$> mleft)
		(fromMaybe Leaf $ unfoldTree f <$> mright)

unfoldTreeM :: (Applicative m, Monad m) => (b -> m (a, (Maybe b, Maybe b))) -> b -> m (BTree a)
unfoldTreeM f b = do
	(a, (mleft, mright)) <- f b
	BNode a
		<$> fromMaybe (return Leaf) (unfoldTreeM f <$> mleft)
		<*> fromMaybe (return Leaf) (unfoldTreeM f <$> mright)
