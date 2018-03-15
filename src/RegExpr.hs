module RegExpr ( RegExpr (..) ) where

import qualified Data.Foldable as F
import Data.Set (Set)
import qualified Data.Set as Set

data RegExpr a = Empty
               | Sym a 
               | Con (RegExpr a) (RegExpr a)
               | Union (RegExpr a) (RegExpr a)
               | Kleene (RegExpr a)
                 deriving (Show)

