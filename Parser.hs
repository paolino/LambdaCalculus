{-# LANGUAGE FlexibleContexts #-}
module Parser where 

import Lambda

import Text.Parsec.String
import Data.Char
import Text.Parsec
import Data.List

terms = concat $ map show [X .. B]
readTerm :: Char -> Parser V
readTerm x = maybe (unexpected "no parse") return  . lookup (toUpper x) $ zip terms [X .. B]

term,lambda,combine,exprP :: Parser (Expr V)
exprP = spaces >> (lambda <|> try combine <|>  term)

parens term = do
        char '('
        t <- term 
        char ')'
        return t

term = T <$> value <|> parens ( T <$> value)
value = letter >>= readTerm
lambda = do
    char '\\' 
    ls <- many value
    char '.'
    c <- exprP
    return $ foldr (:\) c ls
    
combine = chainl1 (spaces >> (try term <|> parens exprP)) (return (:#))
    
combineTerm = (:#) <$> exprP <*> exprP

parsing = parse exprP "lambda parser"
