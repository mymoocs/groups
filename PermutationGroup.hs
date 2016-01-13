{-# LANGUAGE FlexibleInstances#-}

module PermutationGroup where

import Group    
import Data.List
import qualified  Data.Map as M

newtype Permutation a = P [a]

instance (Show a, Ord a) => Show (Permutation a) where
    show (P xs) = "\n"++showRow domain ++ showRow xs
        where
          showRow ys = "\n| " ++ (concat $ map ((++" ") . show) ys) ++ "|"
          domain = sort xs


newtype Pa a = Pa ([a],[a])   
instance (Show a, Ord a) => Show (Pa a) where
    show (Pa (ds, xs)) = showRow ds ++ showRow xs
                      where
                        showRow ys = "\n| " ++ (concat $ map ((++" ") . show) ys) ++ "|"

--type I = Pa ([0..], [0..])


pa  = Pa ([0,3,7,1,5,2,4,6,8,9], [3,7,1,5,0,8,2,9,4,6])
pa1 = Pa ([1,2,3], [3,2,1])     

instance Group (Pa Integer) where
    mulId  =  pa
    mulInv (Pa (d,p)) = Pa (p,d)           
    mul (Pa (d1,p1)) (Pa (d2,p2))
        | (not . null) (d1 \\ d2) = Nothing -- check for domains equality
        | (not . null) (d2 \\ d1) = Nothing -- check for domains equality
        | null p3 = Nothing                 -- should not happen actually
        | otherwise = (Just .  Pa) (d1, p3)
        where
          p3 = shift $ zipWith (,) d1 p1
          map2 = M.fromList (zipWith (,) d2 p2)
          shift [] = []       
          shift ((x,y):xs) =case M.lookup y map2 of
                              Just v -> v:shift xs
                              Nothing -> []

                   
               
-- page 15
q = Pa ([0,1,2,3,4],[2,3,4,0,1])
p = Pa ([2,3,4,0,1],[4,0,1,3,2])    

    
psort (Pa (d,p)) = Pa (d',p')
    where
      (d',p') = (unzip . sort) $ zipWith (,)  d p


                
toCycle (Pa (d,p)) = cycle (safeHead d) [] []   
    where
      z = zipWith (,) d p
      m = M.fromList z
      cycle Nothing _ css  = map nub . nub $ reverse css     
      cycle (Just x) cs css =
          case M.lookup x m of
            Just v -> if elem v cs
                      then newCycle (safeHead (d \\(concat css))) []  ((cs++[x]):css)
                      else cycle (Just v)  (cs++[x]) css
            Nothing -> error "invalid permutation"
      newCycle  = cycle 

cP = Pa ([0..9],[3,5,8,7,2,0,9,1,4,6])
cP1 = Pa ([0..9],[2,4,0,5,9,1,8,7,6,3])

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
              
valid :: Pa a -> Bool
valid (Pa (ds,ps)) = undefined 
