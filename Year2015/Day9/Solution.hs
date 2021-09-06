module Year2015.Day9.Solution where

import Data.Foldable (minimumBy, maximumBy)
import Data.Function ((&))

solve :: String -> IO ()
solve input = print shortestDistance >> print longestDistance
  where graph = parseEdges input
        shortestDistance = minimum (distance <$> allPaths graph shortest)
        longestDistance = maximum (distance <$> allPaths graph longest)

parseEdges :: String -> Graph String Int
parseEdges input = foldr (\e@(Edge v1 v2 _) g -> g & addVertice v1 & addVertice v2 & addEdge e) emptyGraph allEdges
  where allEdges = toEdge <$> lines input
        toEdge l = let [v1, "to", v2, "=", d] = words l in Edge v1 v2 (read d)

data Edge vertice a = Edge vertice vertice a deriving (Eq, Show)

compareDistance :: Ord a => Edge v a -> Edge v a -> Ordering
compareDistance (Edge _ _ a1) (Edge _ _ a2) = compare a1 a2

type Path v a = [Edge v a]

shortest :: Ord a => Path v a -> Edge v a
shortest = minimumBy compareDistance

longest :: Ord a => Path v a -> Edge v a
longest = maximumBy compareDistance

distance :: Num a => Path v a -> a
distance = foldr (\(Edge _ _ d) acc -> acc + d) 0

data Graph vertice a = Graph { vertices :: [vertice], edges :: [Edge vertice a] }
  deriving (Eq, Show)

emptyGraph :: Graph edge vertice
emptyGraph = Graph { edges = [], vertices = [] }

addVertice :: (Eq e, Eq v) => v -> Graph v e -> Graph v e
addVertice v g = if elem v $ vertices g then g else g { vertices = v:vertices g }

removeVertice :: Eq v => v -> Graph v e -> Graph v e
removeVertice v g = g {
  vertices = filter (/= v) (vertices g),
  edges = filter (\(Edge v1 v2 _) -> v /= v1 && v /= v2) (edges g)
}

addEdge :: (Eq v, Eq e) => Edge v e -> Graph v e -> Graph v e
addEdge e g = if elem e $ edges g then g else g { edges = e:edges g }

reachableFrom :: Eq v => v -> Graph v e -> [Edge v e]
reachableFrom v = fmap directTo . filter (\(Edge v1 v2 _) -> v1 == v || v2 == v) . edges
  where directTo e@(Edge v1 v2 a) | v1 /= v && v2 == v = Edge v2 v1 a
                                  | otherwise = e

allPaths :: (Eq v, Ord a) => Graph v a -> (Path v a -> Edge v a) -> [Path v a]
allPaths g cmp = fmap (greedyBy g cmp) (vertices g)

greedyBy :: (Eq v, Ord a) => Graph v a -> ([Edge v a] -> Edge v a) -> v -> [Edge v a]
greedyBy (Graph _ []) _ _ = []
greedyBy g cmp start = path
  where path = next:greedyBy (removeVertice start g) cmp end
        next@(Edge _ end _) = cmp $ reachableFrom start g

