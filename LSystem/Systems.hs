module LSystem.Systems
	( koch
	, kochSnowflake
	) where

import Control.Applicative

import LSystem.Base
import LSystem.Turtle

koch :: LSystem TurtleAction
koch = LSystem
	{ advance = \x -> case x of
		Forward f -> let f' = f / 3 in
			[ Forward f'
			, TurnLeft 60
			, Forward f'
			, TurnRight 120
			, Forward f'
			, TurnLeft 60
			, Forward f'
			]
		a -> pure a
	}

kochSnowflake :: Float -> [TurtleAction]
kochSnowflake a =
	[ Forward a
	, TurnRight 120
	, Forward a
	, TurnRight 120
	, Forward a
	, TurnRight 120
	]
