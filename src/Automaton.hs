module Automaton(Automaton(..),runAutomaton) where

import Data.Maybe
import Data.Set(Set)
import Data.Map(Map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as May
    
data Automaton q a =
    Automaton { state :: Set q,           -- automaton states
                sigma :: Set a,           -- alphabet
                delta :: Map q (Map a q), -- function 
                initial :: q,             -- initial state
                accepting :: Set q        -- accepting states
              }
    deriving (Show,Read)

iterar :: (Ord q, Ord a) => Map q (Map a q) -> q -> a -> Maybe q
iterar m q cc = Map.lookup q m >>= (\m' -> Map.lookup cc m')   

runDelta :: (Ord q, Ord a) => Map q (Map a q) -> q -> [a] -> Maybe q
runDelta m q0 [x]    = iterar m q0 x
runDelta m q0 (x:xs) = iterar m q0 x >>= \q' -> runDelta m q' xs 

runAutomaton :: (Ord a, Ord q) => Automaton q a -> [a] -> Bool
runAutomaton auto xs = maybe False (\x -> Set.member x (accepting auto)) ans
	       where ans = runDelta (delta auto) (initial auto) xs  


auto1 = (read "Automaton {state = fromList [0,1,2,3], sigma = fromList \"a\", delta = fromList [(0,fromList [('a',1)]),(1,fromList [('a',2)]),(2,fromList [('a',3)])], initial = 0, accepting = fromList [3]}") :: Automaton Int Char

-- Automata: que lee la siguiente expresion regular aa*
auto2 = (read "Automaton {state = fromList [0,1], sigma = fromList \"a\", delta = fromList [(0,fromList [('a',1)]),(1,fromList [('a',1)])], initial = 0, accepting = fromList [1]}") :: Automaton Int Char

auto3 = (read "Automaton {state = fromList [0,1,2], sigma = fromList \"ab\", delta = fromList [(0,fromList [('a',1)]),(1,fromList [('a',2),('b',1)])], initial = 0, accepting = fromList [2]}") :: Automaton Int Char
