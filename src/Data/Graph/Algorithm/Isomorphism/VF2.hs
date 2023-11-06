module Data.Graph.Algorithm.Isomorphism.VF2 where
import Data.Graph.Inductive (context, lab, match, neighbors, Node, Graph, nodes, out, subgraph,suc,inn',out',edges)
import Data.List (intersect, nub, (\\), sort,find)
import Data.Tree
import Data.Maybe (isJust)

isIsomorph :: (Graph gr, Eq a, Eq b) => gr a b -> gr a b -> Bool
isIsomorph g1 g2 | (((length $ nodes g1) /= (length $ nodes g2)) || ((length $ edges g1) /= (length $ edges g2))) = False
                 | otherwise = isJust $ find (\n -> (length n) == (length (nodes g1)) ) $ flatten $ match' g1 g2 []

match' :: (Eq a,Eq b,Graph gr) => gr a b -> gr a b -> [(Int,Int)] -> Tree [(Int,Int)]
match' g1 g2 s = Node s (map (\p -> match' g1 g2 (p:s)) pSet) where
                (m1,m2) = (map fst s,map snd s)
                (t1',t2') = ((nub $ concatMap (suc g1.fst) s) \\ m1,take 1.sort $ (nub ( concatMap (suc g2.snd) s) \\ m2))
                (t1,t2) = (if null t1' then nodes g1 \\ m1 else t1',if null t2' then take 1.sort $ nodes g2 \\ m2 else t2')
                pSet = filter (\p -> radj p && rinout p) $ [(n,m) | n <- t1, m <- t2, lab g1 n == lab g2 m]
                radj (n,m) = ((length s) - (length $ s \\ ([(i,j)| i <- s1, j <- s2 , (getEdgeLab g1 (n,i)) == (getEdgeLab g2 (m,j))   ]))) == (length s2) where
                    (s1,s2) = (((suc g1 n) `intersect` m1),((suc g2 m) `intersect` m2))
                rinout (n,m) = (length $ nub $ (suc g1 n) `intersect` t1) >= (length $ nub $ (suc g2 m) `intersect` t2)
                getEdgeLab g (i,j) = map (\(_,_,b) -> b) $ out' (context g i) `intersect` inn' (context g j)
