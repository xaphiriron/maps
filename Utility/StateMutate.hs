module Utility.StateMutate
	( stateMutate
	) where

import Control.Monad.State

stateMutate :: (g -> (a, g)) -> State g a
stateMutate f = do
	g <- get
	let (a, g') = f g
	put g'
	return a
