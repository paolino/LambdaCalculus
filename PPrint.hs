{-# LANGUAGE NoMonomorphismRestriction #-}
module PPrint where

import Lambda
import Data.Char
import Data.List (find)
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


match :: [(Expr Char,String)] -> Expr Char -> Maybe String
match db x = (++ " "). (" "++) .snd <$> find ((=:= x).fst) db

pprint'' _ _ (T x) = lshow x

pprint'' db False l@(x :\ y@(_ :\ _)) = maybe ("(λ" ++ lshow x ++ pprint'' db True y++")") id $ match db l
pprint'' db True l@(x :\ y@(_ :\ _)) =  maybe (lshow x ++ pprint'' db True y) ("." ++) $ match db l

pprint'' db False l@(x :\ y@(_ :# _)) = maybe ("(λ" ++ lshow x ++ "." ++ pprint'' db True y ++ ")") id $ match db l
pprint'' db True l@(x :\ y@(_ :# _)) = maybe (lshow x ++ "." ++ pprint'' db True y) ("." ++) $ match db l

pprint'' db False l@(x :\ y) = maybe ( "(λ" ++ lshow x ++ "." ++ pprint'' db False y ++ ")") id $ match db l
pprint'' db True l@(x :\ y) = maybe ( lshow x ++ "." ++ pprint'' db False y) id $ match db l


pprint'' db False (x@(_ :#_) :# y) = "(" ++ pprint'' db True x ++ pprint'' db False y ++ ")"
pprint'' db True (x@(_ :# _) :# y) = pprint'' db True x ++ pprint'' db False y

pprint'' db False (x :# y) = "(" ++ pprint'' db False x ++ pprint'' db False y ++ ")"
pprint'' db True (x :# y) = pprint'' db False x ++ pprint'' db False y

pprintdb db = pprint'' db False
