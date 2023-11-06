{-# LANGUAGE FlexibleInstances #-}
module Data.Graph.Algorithm.Subgraphs.Enumeration (search) where
-- original paper https://match.pmf.kg.ac.rs/electronic_versions/Match41/match41_145-149.pdf
-- thanks to Daniel Wagner for help with profiling the algorithm

import Data.Graph.Inductive
import Data.List
import Data.Tree

uniq = map head . group . sort . map (\(a, b) -> (min a b, max a b))
delEdgeLU (from, to) = delEdge (from, to) . delEdge (to, from)
insEdgeDU (from, to) = insEdge (from, to, ()) . insNodeU to . insNodeU from where
    insNodeU n g = if gelem n g then g else insNode (n, ()) g

nextEdges subgraph remaining
    | isEmpty subgraph = uniq (edges remaining)
    | otherwise = uniq $ do
        n  <- nodes subgraph
        n' <- suc remaining n
        return (n, n')

search :: DynGraph gr => gr () () -> Tree (gr () ())
search_ subgraph remaining
    = Node subgraph
    . snd . mapAccumL step remaining
    $ nextEdges subgraph remaining
    where
    step r e = let r' = delEdgeLU e r in (r', search_ (insEdgeDU e subgraph) r')

search = search_ (empty)

