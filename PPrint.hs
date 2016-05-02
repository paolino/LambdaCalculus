{-# LANGUAGE NoMonomorphismRestriction #-}
module PPrint where

import Lambda
import Data.Char

lshow = return
pprint' _ (T x) = lshow x

pprint' False (x :\ y@(_ :\ _)) = "(λ" ++ lshow x ++ pprint' True y++")"
pprint' True (x :\ y@(_ :\ _)) =  lshow x ++ pprint' True y

pprint' False (x :\ y@(_ :# _)) = "(λ" ++ lshow x ++ "." ++ pprint' True y ++ ")"
pprint' True (x :\ y@(_ :# _)) = lshow x ++ "." ++ pprint' True y

pprint' False (x :\ y) = "(λ" ++ lshow x ++ "." ++ pprint' False y ++ ")"
pprint' True (x :\ y) = lshow x ++ "." ++ pprint' False y


pprint' False (x@(_ :#_) :# y) = "(" ++ pprint' True x ++ pprint' False y ++ ")"
pprint' True (x@(_ :# _) :# y) = pprint' True x ++ pprint' False y

pprint' False (x :# y) = "(" ++ pprint' False x ++ pprint' False y ++ ")"
pprint' True (x :# y) = pprint' False x ++ pprint' False y

pprint ::  Expr Char -> String
pprint = pprint' False

