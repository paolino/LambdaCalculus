{-# LANGUAGE FlexibleContexts #-}
module Parser where 

import Lambda

import Text.Parsec.String
import Data.Char
import Text.Parsec
import Data.List

terms = ['A' .. 'Z']
readTerm :: Char -> Parser Char
readTerm  x = if toUpper x `elem` terms then return x else unexpected "no parse"

term,lambda,combine,exprP :: Parser (Expr Char)
exprP = spaces >> (lambda <|> try combine <|>  term)

parens term = do
        char '('
        t <- term 
        char ')'
        return t

term = T <$> value <|> parens ( T <$> value)
value = letter >>= readTerm
lambda = do
    char '\\' <|> char '!' <|> char '/' <|> char 'λ' <|> char '^'
    ls <- many value
    char '.'
    c <- exprP
    return $ foldr (:\) c ls
    
combine = chainl1 (spaces >> (try term <|> parens exprP)) (return (:#))
    
combineTerm = (:#) <$> exprP <*> exprP

parsing = parse exprP "lambda parser"
