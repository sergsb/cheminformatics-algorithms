module Enumeration_test where
import Data.Graph.Inductive.Graph
import Data.List (nub)  
import Data.Tree (levels)
import Test.QuickCheck
import Data.Graph.Algorithm.Subgraphs.Enumeration (search)
import Data.Graph.Inductive

mkUUGraph :: [(Int, Int)] -> Gr () ()
mkUUGraph es = mkUGraph ns (es ++ map swap es) where
    ns = nub (map fst es ++ map snd es)
    swap (a, b) = (b, a)

sampleGraph = mkUUGraph origPaper
origPaper = [(1, 2), (1, 5), (1, 6), (2, 3), (3, 4), (4, 5)]

processOrigPaper = map length . tail . levels . search $ sampleGraph

prop_orig_paper :: Property
prop_orig_paper = processOrigPaper === [6,7,8,9,6,1] -- from original paper https://match.pmf.kg.ac.rs/electronic_versions/Match41/match41_145-149.pdf

c60 = [(1,4),(1,3),(1,2),(2,6),(2,5),(3,10),(3,7),(4,24),(4,21),(5,8),(5,7),(6,28),(6,25),
    (7,9),(8,11),(8,12),(9,16),(9,13),(10,20),(10,17),(11,14),(11,13),(12,28),(12,30),(13,15),
    (14,43),(14,30),(15,44),(15,18),(16,18),(16,17),(17,19),(18,47),(19,48),(19,22),(20,22),(20,21),
    (21,23),(22,31),(23,32),(23,26),(24,26),(24,25),(25,27),(26,35),(27,36),(27,29),(28,29),(29,39),
    (30,40),(31,32),(31,33),(32,34),(33,50),(33,55),(34,37),(34,55),(35,36),(35,37),(36,38),(37,57),
    (38,41),(38,57),(39,40),(39,41),(40,42),(41,59),(42,45),(42,59),(43,44),(43,45),(44,46),(45,51),
    (46,49),(46,51),(47,48),(47,49),(48,50),(49,53),(50,53),(51,52),(52,60),(52,54),(53,54),(54,56),(55,56),(56,58),(57,58),(58,60),(59,60)] 


prop_fullerene_c60 = (length . concat . take 9. levels $ search (mkUUGraph c60)) === 102333
