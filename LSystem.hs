module LSystem
	( LSystem(..)
	, evolve
	) where

data LSystem a = LSystem
	{ advance :: a -> [a]
	}

evolve :: LSystem a -> Int -> [a] -> [a]
evolve l i b = head . drop i . iterate (advance l =<<) $ b

{-
KINDS OF LSYSTEMS:

the basic kind (w/ parametric possiblities)
evolving in a monad (a -> m [a])
prelookup and postlookup (respecting stack motions) -- something like Maybe a -> a -> Maybe a -> [a]
same as above only in a monad!!

...i can see this getting thorny

the kind that evolve based on a "condition" on a "map" (e.g., evolve in the direction of light by checking light on the location and growing or withering based on that) (wait is this just a specific case of "in a (state) monad"?)
-}