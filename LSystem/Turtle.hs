{-# LANGUAGE TemplateHaskell #-}

module LSystem.Turtle
	( TurtleAction(..)
	) where

import Control.Lens
import Control.Applicative
import Control.Arrow
import Control.Monad.Writer

-- iirc this `angle` is in radians and mine is in degrees
import Linear.V2 hiding (angle)
import Linear.Vector ((^*))

data TurtleAction
	= PenDown | PenUp
	| Forward Float | Backward Float
	| TurnLeft Float | TurnRight Float
	| StackPush | StackPop
	deriving (Eq, Ord, Show, Read)

data Turtle = Turtle
	{ _heading :: Float
	, _position :: V2 Float
	, _pen :: Bool
	, _stack :: Maybe Turtle
	-- penColor
	}
	deriving (Eq, Ord, Show, Read)

data Line = Line
	{ pts :: (V2 Float, V2 Float)
	}
	deriving (Eq, Ord, Show, Read)

makeLenses ''Turtle

data Dictionary a b = TD
	{ resolve :: a -> b
	}


turtleDict :: Dictionary Char (Maybe TurtleAction)
turtleDict = TD
	{ resolve = \a -> case a of
		'v' -> Just $ PenDown
		'^' -> Just $ PenUp
		'F' -> Just $ Forward 1
		'f' -> Just $ Forward 1
		'-' -> Just $ TurnLeft 60
		'+' -> Just $ TurnRight 60
		'T' -> Just $ Forward 1
		'[' -> Just $ StackPush
		']' -> Just $ StackPop
		'L' -> Nothing
		'X' -> Nothing
		'Y' -> Nothing
		_ -> error "idk?"
	}

turtleActionW :: Turtle -> TurtleAction -> Writer [Maybe Line] Turtle
turtleActionW t act = writer . second pure $ turtleAction t act

angle :: (Floating a) => a -> V2 a
angle deg = uncurry V2 . (sin &&& cos) $ deg / 180 * pi

turtleAction :: Turtle -> TurtleAction -> (Turtle, Maybe Line)
turtleAction t act = case act of
	PenDown -> (pen .~ True $ t, Nothing)
	PenUp -> (pen .~ False $ t, Nothing)
	Forward x ->
		let nt = position %~ (+ (angle (t ^. heading) ^* x)) $ t
		in (nt, if t ^. pen
			then Just $ Line (t ^. position, nt ^. position)
			else Nothing)
	Backward x ->
		let nt = position %~ (subtract (angle (t ^. heading) ^* x)) $ t
		in (nt, if t ^. pen
			then Just $ Line (t ^. position, nt ^. position)
			else Nothing)
	TurnLeft x -> (heading %~ (subtract x) $ t, Nothing)
	TurnRight x -> (heading %~ (+ x) $ t, Nothing)
	StackPush -> (t { _stack = Just t }, Nothing)
	StackPop -> case t ^. stack of
		Nothing -> error "stack underflow"
		Just t' -> (t', Nothing)
