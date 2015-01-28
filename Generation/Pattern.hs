module Generation.Pattern
	( Language (..)
	, Shaped(..)
	, generate
	) where

import Data.List
import Data.Function

{-
-- p = pattern
-- a = map
-- m = generating monad
-- l = map boundary limit (to declare a subpattern has a smaller domain than the initial pattern)
-}
data Language b a m l = Language
	{ empty :: a l
	, zero :: b
	, pattern :: b -> a l -> m (a l, [(l, b)])
	}

data Shaped a l = Shaped
	{ extract :: a l -> l
	, set :: l -> a l -> a l
	}

generate :: (Monad m, Ord l) => Language b a m l -> Shaped a l -> m (a l)
generate lang shape = sortedAccumM (pattern lang) shape (empty lang)
	$ [((extract shape) (empty lang), zero lang)]

-- FIXME: ... this should be sorting on `b` rather than `l`.
sortedAccumM :: (Monad m, Ord l) => (b -> a l -> m (a l, [(l, b)])) -> Shaped a l -> a l -> [(l, b)] -> m (a l)
sortedAccumM _ _ acc [] = return acc
sortedAccumM advance shape acc ((b, p):remaining) = do
	let l = (extract shape) acc
	(acc', new) <- advance p ((set shape) b acc)
	let acc'' = (set shape) l acc'
	sortedAccumM advance shape acc'' $ sortBy (compare `on` fst) $ new ++ remaining
