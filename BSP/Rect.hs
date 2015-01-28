module BSP.Rect
	( Rect (..)
	, rectOverlap
	) where

import Linear.V2

-- the top left coordinate is always a valid tile. size is the number of additional spaces that are valid. i.e., Rect (V2 x y) (V2 0 0) is a valid, one-tile room
data Rect = Rect
	{ topLeft :: V2 Int
	, size :: V2 Int
	}
	deriving (Eq, Ord, Show, Read)

-- the result will be Nothing if the rooms don't overlap; otherwise it'll be a normally-formatted rect of the intersection of the rect sizes
-- the equations given only work if the first room has a less than/equal x & y coord vs the second room. rather than fix the equation we just fix the data. note that since the resulting overlap is the sum of two separate one-dimensional calculations, changing the x/y values around in this way doesn't change the result, even though it wildly changes the shape of the rooms
rectOverlap :: Rect -> Rect -> Maybe Rect
rectOverlap r1@(Rect (V2 x1 y1) (V2 w1 h1)) r2@(Rect (V2 x2 y2) (V2 w2 h2)) =
	case (x2 < x1, y2 < y1) of
		(True, True) -> rectOverlap r2 r1
		(True, False) -> rectOverlap
			(Rect (V2 x2 y1) (V2 w2 h1))
			(Rect (V2 x1 y2) (V2 w1 h2))
		(False, True) -> rectOverlap
			(Rect (V2 x1 y2) (V2 w1 h2))
			(Rect (V2 x2 y1) (V2 w2 h1))
		_ -> case (min w2 (x1 + w1 - x2), min h2 (y1 + h1 - y2)) of
			(xSize, ySize)
				| xSize < 0 || ySize < 0 -> Nothing
				| otherwise -> Just $ Rect (V2 x2 y2) (V2 xSize ySize)
