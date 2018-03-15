--RegExpr := Concat 
--         | Concat '|' Concat
--Concat  := Kleene 
--         | Kleene '.' Kleene
--Kleene  := Factor  
--         | Factor '*' 
--Factor  := a
--         | 0
--         | '('RegExpr')'
-- (ab*)*

module Parser(parseRegExpr) where 

import RegExpr
import Text.ParserCombinators.Parsec
import Control.Applicative((<$),(<$>), (<**>))

parseRegExpr :: SourceName -> String -> Either ParseError (RegExpr Char)
parseRegExpr s c = parse pRegExpr s c   

pRegExpr :: GenParser Char st (RegExpr Char)
pRegExpr =  pConcat `chainl1` pUnionOp

pConcat :: GenParser Char st (RegExpr Char)
pConcat = pKleene `chainl1` pConcatOp

pKleene :: GenParser Char st (RegExpr Char)
pKleene = pFactor <**> option id pKleeneOp	

pUnionOp :: GenParser Char st ((RegExpr Char) -> (RegExpr Char) -> (RegExpr Char))
pUnionOp = Union <$ char '|'

pConcatOp :: GenParser Char st ((RegExpr Char) -> (RegExpr Char) -> (RegExpr Char))
pConcatOp = Con <$ char '.'
        <?> "expected something else" 

pKleeneOp :: GenParser Char st ((RegExpr Char) -> (RegExpr Char))
pKleeneOp = Kleene <$ char '*'

pFactor :: GenParser Char st (RegExpr Char)
pFactor =  Sym <$> pSym
       <|> Empty <$ char '0'
       <|> do { char '('
              ; x <- (pRegExpr)
              ; char ')' 
              ; return x}
       <?> "expected something else"

pSym :: GenParser Char st Char
pSym = do l <- letter
          return $ l

{-pParens :: GenParser Char st (RegExpr Char)
pParens = between (char '(') (char ')') -}