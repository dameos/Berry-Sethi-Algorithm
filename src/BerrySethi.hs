module BerrySethi(berrySethi) where 

import RegExpr 
import Automaton
import NumSym  
import Data.Set(Set)
import Data.Map(Map)
import qualified Data.Set as Set  
import qualified Data.Map as Map
import qualified Data.Maybe as May
import Text.ParserCombinators.Parsec

berrySethi :: (Ord a) => RegExpr a -> Automaton (Set (NumSym a)) a
berrySethi reg = comBerry (Automaton {state = Set.empty, sigma = getAlpha(reg), delta = Map.empty, initial = inicial(regN), accepting = Set.singleton(final(regN))}) (regN)
            where regN = NumSym.toNumReg(reg)

nulo :: (Ord a)    =>  RegExpr a -> Bool
nulo (Kleene a)     = True 
nulo (Empty)        = False
nulo (Sym a)        = False
nulo (Con r a)      = nulo r && nulo a 
nulo (Union r a)    = nulo r || nulo a

inicial :: (Ord a) => RegExpr a -> Set  a
inicial (Kleene a)   = inicial a  
inicial (Empty)      = Set.empty
inicial (Sym a)      = Set.singleton(a)
inicial (Con r a)    = if (nulo(r)) then Set.union (inicial r) (inicial a) else inicial(r) 
inicial (Union r a)  = Set.union (inicial r) (inicial a)

final :: (Ord a)   => RegExpr  a -> Set a
final (Kleene a)     = final a 
final (Empty)        = Set.empty
final (Sym a)        = Set.singleton(a)
final (Con r a)      = if (nulo(a)) then Set.union (final r) (final a) else final(a)
final (Union r a)    = Set.union (final r) (final a)

dig :: (Ord a)  => RegExpr a -> Set (a,a) 
dig (Empty)      = Set.empty
dig (Sym a)      = Set.empty
dig (Con r a)    = Set.unions [dig r, dig a, (conSet(final r) (inicial a))] 
dig (Union r a)  = Set.union (dig r) (dig a)
dig (Kleene a)   = Set.unions [dig a, (conSet(final a) (inicial a))]

conSet :: (Ord a) => Set a -> Set a -> Set (a,a)
conSet s s1 = Set.fromList [(a,b) | a <- Set.toList s, b <- Set.toList s1]  

followers :: (Ord a) => Set (a,a) -> Map.Map a (Set a)
followers s = Set.foldr f Map.empty s
           where f (k,v) = Map.insertWith (Set.union) k (Set.singleton v)

getAlpha :: (Ord a) => RegExpr a -> Set a
getAlpha (Empty)     = Set.empty
getAlpha (Kleene a)  = getAlpha a
getAlpha (Sym a)     = Set.singleton(a)
getAlpha (Union r a) = Set.union (getAlpha r) (getAlpha a)
getAlpha (Con r a)   = Set.union (getAlpha r) (getAlpha a)

filtrar :: (Ord a) => NumSym a -> a ->  Bool 
filtrar  NEmpty  car  = False
filtrar (NS i a) car  = if a == car then True else False
filtrar  NTerm   car  = False 

comBerry :: (Ord a) => Automaton (Set (NumSym a)) a -> RegExpr(NumSym a)  -> Automaton (Set (NumSym a)) a
comBerry auto reg = f (iteracion(auto, Set.empty, Set.singleton(initial auto),reg))
              where  f (a,b,c,d) = a

iteracion :: (Ord a) => (Automaton (Set (NumSym a)) a, Set (Set (NumSym a)), Set(Set (NumSym a)), RegExpr (NumSym a)) -> (Automaton (Set (NumSym a)) a, Set (Set (NumSym a)), Set(Set (NumSym a)), RegExpr (NumSym a))
iteracion (auto, v, nv, reg)  = if(Set.null(nv')) then (auto',v',nv', reg) else iteracion(auto', v', nv', reg)
                         where (auto', v', nv', reg) = iteracion2(auto, v, nv, reg)

iteracion2 :: (Ord a) => (Automaton (Set (NumSym a)) a, Set (Set (NumSym a)), Set(Set (NumSym a)), RegExpr (NumSym a)) -> (Automaton (Set (NumSym a)) a, Set (Set (NumSym a)), Set(Set (NumSym a)), RegExpr (NumSym a))
iteracion2 (auto, v, nv, reg) =  (auto', v', nv'', reg) 
                         where  q     = Set.elemAt 0 nv
                                v'    = Set.insert q v
                                nv'   = Set.delete q nv
                                fol   = followers $ (dig reg)
                                q'    = Set.foldl f (Set.empty) (sigma auto)
                                f a b = Set.insert (b, fol') a
                                        where fol' =  Set.map (\x -> fol Map.! x) q'' 
                                              q'' =  Set.filter (\x -> filtrar x b) q
                                est   = Set.unions $ Set.toList (Set.map (\(x,y) -> y) q')
                                nv''  = Set.union est nv'
                                sig   = Set.foldl (\ac (x,y) -> Map.union (ac) (Map.singleton x (Set.unions $ Set.toList y))) Map.empty q'
                                auto' = Automaton{state = Set.union (est) (state auto), sigma = sigma auto, delta = Map.insert (q) (sig) (delta auto), initial = initial auto, accepting = accepting auto}

expr = (Union (Kleene (Con (Sym 'a') (Sym 'b'))) (Kleene (Con (Sym 'a') (Sym 'c'))))



