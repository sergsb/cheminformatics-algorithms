{-# LANGUAGE NoMonomorphismRestriction #-}
module VF2_test where
import Test.Hspec
import Data.List (permutations,nubBy)
import qualified Data.Map as Map
import Test.QuickCheck
import Data.Graph.Inductive (nodes,undir,mkGraph, labNodes, labEdges, Node, LEdge, Graph,Gr)
import Data.Graph.Algorithm.Isomorphism.VF2
import Data.Graph.Inductive.Arbitrary

filterWrong :: [LEdge b] -> [LEdge b]
filterWrong lst = nubBy (\(a,b,_) (a',b',_) -> (a == a' && b == b') || ( a == b' && b == a')) $ filter (\(a,b,_) -> a /= b) lst

data TestInstance = A | B | C deriving (Eq, Ord, Show)
instance Arbitrary TestInstance where
    arbitrary = elements [A, B, C] 

arbitraryUndirectedGraph = fmap undir $ arbitraryGraphWith filterWrong

prop_isIsomorph_self :: Property
prop_isIsomorph_self = forAll (resize 5 $ arbitraryUndirectedGraph :: Gen (Gr TestInstance TestInstance)) $ \g ->
    isIsomorph g g


--check if g1 and g2 are isomorphic
permuteNodes :: Graph gr => gr a b -> [Node] -> gr a b
permuteNodes graph permutation
  | length nodes_internal /= length permutation = error "Permutation list size must match the number of nodes in the graph."
  | otherwise = mkGraph newNodes newEdges
  where
    nodes_internal = labNodes graph
    nodeMap = Map.fromList $ zip (map fst nodes_internal) permutation
    lookupNode old = Map.findWithDefault (error "Node mapping failed.") old nodeMap
    newNodes = map (\(n, l) -> (lookupNode n, l)) nodes_internal
    newEdges = map (\(from, to, l) -> (lookupNode from, lookupNode to, l)) $ labEdges graph

prop_permutedIsomorph :: Property
prop_permutedIsomorph = forAll (resize 5 (arbitraryUndirectedGraph :: Gen (Gr Int Char))) $ \graph -> 
    let nodePermutations = take 5 $ permutations (nodes graph) -- Limiting the number of permutations
        props = map (\perm -> 
                        let permutedGraph = permuteNodes graph perm
                        in isIsomorph graph permutedGraph === True) 
                   nodePermutations 
    in conjoin props -- Combine all properties

-- main :: IO ()
-- main = hspec $ do
--     describe "VF2" $ do
--         it "Self isomorphism" $ do
--             property prop_isIsomorph_self
--     describe "VF2" $ do
--         it "Permutation isomorphism" $ do
--             property prop_permutedIsomorph

