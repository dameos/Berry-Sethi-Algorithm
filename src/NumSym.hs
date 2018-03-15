module NumSym where

import RegExpr
import Parser

data NumSym a = NEmpty
              | NS Int a 
              | NTerm 
              deriving(Ord, Eq)
        
instance Show a => Show (NumSym a) where
    show (NEmpty) = "NEmpty"
    show (NS i a) = "NS" ++ show a ++ "_" ++ show i
    show (NTerm ) = "-|"


fromRegToNumReg :: (Int, RegExpr a) -> (Int, RegExpr (NumSym a))
fromRegToNumReg (i, Empty)     = (i, Sym(NEmpty))
fromRegToNumReg (i, Sym a)     = (i+1, Sym(NS i a))
fromRegToNumReg (i, Con l r)   = (i'', Con l' r')
                               where (i',l')  = fromRegToNumReg(i,l)
                                     (i'',r') = fromRegToNumReg(i',r)
fromRegToNumReg (i, Union l r) = (i'', Union l' r')
                               where (i', l') = fromRegToNumReg(i, l)
                                     (i'',r') = fromRegToNumReg(i',r)
fromRegToNumReg (i, Kleene l)  = (i', Kleene(l'))
                               where (i', l') = fromRegToNumReg(i,l)

toNumReg :: RegExpr a -> RegExpr(NumSym a)
toNumReg a = snd $ fromRegToNumReg(0,a)
	