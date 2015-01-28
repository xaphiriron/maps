module BSP.Color
	( colorDungeon
	) where

import BSP.BSP
import Data.BTree

colorDungeon :: BSPDungeon -> Int -> Int -> Maybe Bool
colorDungeon (BSPDungeon w h splits) x y =
	if x < 0 || x > w || y < 0 || y > h
		then Nothing
		else if x == 0 || x == w || y == 0 || y == h
			then Just True
			else Just $ introspect splits x y

introspect :: BTree Split -> Int -> Int -> Bool
introspect Leaf x y = False
introspect (BNode s l r) x y = case s of
	Vertical v
		| x < v -> introspect l x y
		| x > v -> introspect r (x-v) y
		| otherwise -> True
	Horizontal h
		| y < h -> introspect l x y
		| y > h -> introspect r x (y-h)
		| otherwise -> True
