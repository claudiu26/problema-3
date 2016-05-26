-- import Data.List --maximum
-- import Data.List.Split
-- import Data.Char
-- import Data.Ord --comparing
-- import Control.Monad
-- import Control.Monad.Trans

module Cuvant
	where
	
-- 3a. 
pal::(a->Bool)->[a]->([a], [a])
pal p xs = ( filter p xs, filter (not.p) xs )

-- 3b.
esteCuvant cuvant
	| cuvant == sort(cuvant) = True
	| otherwise = False
	
-- 3c. 
cuvinte cuvant_initial = do 
		let subcuv = nub(subsequences cuvant_initial)
		in let lista_finala = filter esteCuvant subcuv
		in maximumBy (comparing length)(lista_finala)
										
-- 3d.
toateCuvintele cuvant_initial = do 
		let subcuv = nub(subsequences cuvant_initial)
		in let lista_finala = filter esteCuvant subcuv
		in mapM_ putStrLn lista_finala
