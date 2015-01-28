module InductiveGraph
	( subgraphs
	, combine
	, randomNode
	, randomizeEdges
	, randomizeEdges'
	, lemap
	, liftContext
	, EdgeConnection(..)
	, cutEdges
	, removeEdgesInward
	, alterNode
	, alterNodes
	, openPath
	, neighborBlob'
	, clearPath
	, clearPaths
	, undirSelected
	, setEdges
	, inflectAround
	, nodesAndEdges
	) where

import Data.Graph.Inductive

import Control.Arrow
import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.State
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>), mconcat, Endo(..))
import Data.List (partition, nub)

import Utility.Rand
import Utility.Tuple

import Utility.StateMutate

-- | decompose a graph into its strongly-connected subgraphs + a list of dangling edges (edges between nodes in different components).
-- | prop> uncurry combine . subgraphs = id
subgraphs :: (DynGraph gr, Eq b) => gr a b -> ([gr a b], [LEdge b])
subgraphs gr =
	  second (nub . concat)
	. unzip
	. map foo
		$ gsel
			<$> contextFilters gr
			<*> pure gr
	where
		contextFilters :: Graph gr => gr a b -> [Context a b -> Bool]
		contextFilters = map ((. node') . flip elem) . scc
		foo :: (DynGraph gr) => [Context a b] -> (gr a b, [LEdge b])
		foo cs =
			  (buildGr *** concat)
			. unzip
			. map extractDanglingEdges
				$ cs
			where
				inSubgraph :: Node -> Bool
				inSubgraph = (`elem` map node' cs)
				-- contexts w/ invalid edges removed and also returned
				extractDanglingEdges :: Context a b -> (Context a b, [LEdge b])
				extractDanglingEdges (in_, n, l, out) =
					( (inAllowed, n, l, outAllowed)
					, inDisconnected <> outDisconnected
					)
					where
						-- this is the correct edge order (that is to say, an LEdge is of the format (to, from, label). if other edge order code conflicts, it's wrong.)
						(inAllowed, inDisconnected) =
							  second (map (\(lbl, to) -> (to, n, lbl)))
							. partition (inSubgraph . snd)
								$ in_
						(outAllowed, outDisconnected) =
							  second (map (\(lbl, to) -> (n, to, lbl)))
							. partition (inSubgraph . snd)
								$ out

-- | recompose graphs as given above back into one graph
combine :: (DynGraph gr, Eq b) => [gr a b] -> [(Node, Node, b)] -> gr a b
combine grs es =
	  insEdges es
	. buildGr
	. concatMap (gsel (const True))
		$ grs

randomNode :: (RandomGen g, Graph gr) => gr a b -> g -> (Node, g)
randomNode gr g = first fst . choose (labNodes gr) $ g

randomizeEdges :: (RandomGen g, DynGraph gr, Random r) => (r, r) -> gr a e -> g -> (gr a r, g)
randomizeEdges range gr g = (gmap (liftContext replaceFunc) gr, g')
	where
		(rs, g') =
			runRand g
				(replicateM (length es) . liftRand $ randomR range)
		es = edges gr
		replaceFunc e = fromMaybe (error ":(") . lookup e
			$ zip es rs

randomizeEdges' :: (RandomGen g, DynGraph gr, Random r) => (r, r) -> gr a e -> Rand g (gr a r)
randomizeEdges' range gr =
	ufold foo (return empty) (emap (const $ liftRand $ randomR range) gr)

foo :: (Monad m, Functor m, DynGraph gr) => Context a (m b) -> m (gr a b) -> m (gr a b)
foo (mea, n, a, meb) mgr = do
	ea <- sequence . fmap liftFst $ mea
	eb <- sequence . fmap liftFst $ meb
	liftM ((ea, n, a, eb) &) mgr

choose :: RandomGen g => [a] -> g -> (a, g)
choose xs g = (xs !! fst ng, snd ng)
	where
		ng = randomR (0, ln - 1) g
		ln = length xs

lemap :: DynGraph gr => (LEdge b -> c) -> gr a b -> gr a c
lemap f = gmap edgeUpdate
	where
		edgeUpdate (in_, n, l, out) =
			( fmap (\(l, from) -> (f (n, from, l), from)) in_
			, n
			, l
			, fmap (\(l, to) -> (f (to, n, l), to)) out
			)

-- lift a function operating on raw edge data into one operating on contexts
liftContext :: ((Node, Node) -> b) -> Context n a -> Context n b
liftContext f (in_, n, l, out) = (in_', n, l, out')
	where
		-- i don't actually know if this is necessary (in that while clearly a function can do different things depending on the edge order, i don't know if the Data.Graph.Inductive-provided `edges` function constructs edges in this order or in this way in general)
		in_' = (\(_, e) -> (f (e, n), e)) <$> in_
		out' = (\(_, e) -> (f (n, e), e)) <$> out

data EdgeConnection = Incoming | Outgoing | All
	deriving (Eq)

-- remove all edges from nodes not in ns to nodes in ns (so that the nodes in ns still have the same connectivity, but there aren't any paths into the section defined by `ns`)
removeEdgesInward :: [Node] -> Context n a -> Context n a
removeEdgesInward ns (in_, n, l, out) =
	if n `elem` ns
		then (filter ((`elem` ns) . snd) in_, n, l, out)
		else (in_, n, l, filter (not . (`elem` ns) . snd) out)

cutEdges :: EdgeConnection -> [Node] -> Context n a -> Context n a
cutEdges cut ns (in_, n, l, out) =
	if n `elem` ns
		then
			( if cut == Outgoing then in_ else []
			, n, l
			, if cut == Incoming then out else []
			)
		else
			( if cut == Incoming
				then in_
				else filter (not . (`elem` ns) . snd) in_
			, n, l
			, if cut == Outgoing
				then out
				else filter (not . (`elem` ns) . snd) out
			)

-- internally Gr is basically `Gr a b = Array Node (Context a b)` but using the general graph interface i think we have to do something more indirect
alterNode :: DynGraph gr => Node -> (a -> a) -> gr a b -> gr a b
alterNode n f =
	let updateOne (in_, n', l, out) = if n == n'
		then (in_, n', f l, out)
		else (in_, n', l, out)
	in gmap updateOne

alterNodes :: DynGraph gr => [Node] -> (a -> a) -> gr a b -> gr a b
alterNodes ns f =
	let update (in_, n, l, out) = if n `elem` ns
		then (in_, n, f l, out)
		else (in_, n, l, out)
	in gmap update

-- the following functions do something with a path or node in the graph, and then return a list of important edges + the graph with some related set of important edges removed. generally, they "remove" nodes "used" by some action, so as to ensure that the edges used here won't be reused and e.g., allow a cross-cut through a dungeon, making it possible to skip most of it.

-- we want to remove edges inward AND remove edges between path nodes that aren't on the `zip path $ tail path` route, in case we have a path that touches itself. so like... we have a list of edges to keep, and a set of nodes, and we want to remove all edges between that set of nodes that aren't on the edge-to-keep list.
openPath :: DynGraph gr => [Node] -> gr a b -> ([Node], gr a b)
openPath path gr =
	( path
	, gmap (removeEdgesInward path) gr
	)

neighborBlob' :: DynGraph gr => Node -> gr a b -> ([Node], gr a b)
neighborBlob' node gr =
	( node : ns
	, gmap (removeEdgesInward $ node : ns) . delEdges toRemove $ gr
	)
	where
		(_, toRemove) = divide es
		ns = suc gr node
		-- all edges between the core & neighboring nodes
		adjEdges = (,) node <$> ns
		-- edges between two neighboring nodes. we remove half of these.
		es = filter (\(a, b) -> a `elem` ns && b `elem` ns)
			$ edges gr

divide :: [a] -> ([a], [a])
divide [] = ([], [])
divide [a] = ([a], [])
divide (a:b:rs) = ((a :) *** (b :)) $ divide rs

{- | okay the goal here is to generate an infinite sequence of distinct (a -> a) functions based on the acc value, hence the `:: c -> (a -> a, c)` function + accSequence
this is because we want the functions to be separate based on the paths, not on the nodes of the paths (every room on the same path should get the same function)
-}
accSequence :: (g -> (a, g)) -> g -> [a]
accSequence f = (fst .) . runState . sequence . repeat . stateMutate $ f

lookupBy :: (c -> a -> Bool) -> c -> [(a, b)] -> Maybe b
lookupBy _ _ [] = Nothing
lookupBy f c ((a, b):vs)
	| f c a = Just b
	| otherwise = lookupBy f c vs

clearPath :: (DynGraph gr, Real b)
	=> (a -> a) -> Node -> Node -> gr a b -> ([Node], gr a b)
clearPath f start end gr = (touched, gmap apply gr')
	where
		(touched, gr') = openPath (sp start end gr) gr
		apply (in_, n, l, out)
			| n `elem` touched = (in_, n, f l, out)
			| otherwise = (in_, n, l, out)

clearPaths :: (DynGraph gr, Real b)
	=> (c -> (a -> a, c)) -> c -> [Node] -> Node -> gr a b -> ([Node], gr a b)
clearPaths f base starts end gr = (nub . concat $ touched, gr')
	where
		(touched, grs) = unzip . fmap (\s -> openPath (sp s end gr) gr) $ starts
		gr' =
			  gmap apply
			. undirSelected (concat touched)
			. gmap (appEndo . mconcat . fmap (Endo . removeEdgesInward) $ touched)
				$ gr
		accFuncs = zip touched $ accSequence f base
		apply (in_, n, l, out) =
			case lookupBy elem n accFuncs of
				Nothing -> (in_, n, l, out)
				Just af -> (in_, n, af l, out)

-- make all edges between nodes in `ns` undirected. like `undir`, but only between selected nodes.
undirSelected :: (DynGraph gr, Eq b) => [Node] -> gr a b -> gr a b
undirSelected ns = gmap reverseEdges
	where
		reverseEdges (in_, n, l, out)
			| n `notElem` ns = (in_, n, l, out)
			| otherwise = (in_ <> fromOut, n, l, out <> fromIn)
				where
					fromIn = mapMaybe
						(\(l, i) -> if i `elem` ns && i `notElem` fmap snd out
							then Just (l, i)
							else Nothing) in_
					fromOut = mapMaybe
						(\(l, o) -> if o `elem` ns && o `notElem` fmap snd in_
							then Just (l, o)
							else Nothing) out

setEdges :: DynGraph gr => b -> gr a c -> gr a b
setEdges = emap . const

-- | increase the weight of an edge based on its proximity to a given node. creates an area to path around, when looking for shortest paths.
inflectAround :: (DynGraph gr, Num b) => Node -> gr a b -> gr a b
inflectAround n gr = lemap foo gr
	where
		foo (a, b, l) = case max <$> lookup a distances <*> lookup b distances of
			Nothing -> l
			Just x -> let xi = fromIntegral (maxDistance - x)
				in l + xi * xi
		maxDistance = maximum . fmap snd $ distances
		distances = level n gr

nodesAndEdges :: Graph gr => gr a b -> ([a], [b])
nodesAndEdges =
	    (map snd . labNodes)
	&&& (map (\(a, b, l) -> l) . labEdges)

