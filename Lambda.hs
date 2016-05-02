{-#LANGUAGE StandaloneDeriving,MultiParamTypeClasses,GeneralizedNewtypeDeriving,ViewPatterns, DeriveFunctor, TemplateHaskell,NoMonomorphismRestriction #-}
module Lambda where

import Data.List (nub, delete)
import Control.Monad.Reader (runReader, local, asks, Reader, MonadReader,join)
import Control.Lens (makeLenses, over, view, ASetter')
import Control.Arrow ((&&&))
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Data.Char (toLower)
-- import Test.Hspec


-- Lambda expressions 
data Expr a     = T a -- terminal name
                | a :\ Expr a -- lambda parameter abstraction
                | Expr a :# Expr a -- expression composition
                deriving (Show, Functor, Eq)

(#) = (:#)
(\.) = (:\)
infixr 1 \.
infixr 1 :\

-- replacing action parameters
data Replace a = Replace {
    target :: a, -- name target to replace
    expr :: Expr a, -- expression replacing the target
    dontuse :: [a] -- shared free variables of the expr
    } deriving (Show)

-- make a replace from a target and a replacing expression
replace :: Eq a =>  a -> Expr a -> Replace a
replace x = Replace x <*> nub . (x:) . freevars 

-- free variables of an expression , with duplicates 
freevars :: Eq a => Expr a -> [a]
freevars (T x) = [x]
freevars (x :\ t) = filter (/= x) $ freevars t
freevars (e :# e') = freevars e ++ freevars e'

-- computational environment, serving fresh renaming via Reader interface
newtype Freshes a b = Freshes (Reader [a] b) deriving (Functor, Applicative, Monad, MonadReader [a])

-- run, given a list of fresh names
withFreshes :: [a] -> Freshes a b -> b
withFreshes xs (Freshes r) = runReader r xs

-- (==) is noisy
eq :: Eq a => a -> a -> Bool
eq = (==)

-- change occurrences of x in p in expr, using Functor.
-- should be correct to use Functor Expr as even lambda introduction of the target will be substituted, 
-- overhead for not ignoring shadowing

alphasub :: Eq a => a -> a -> Expr a -> Expr a
alphasub p y (T ((==y) -> True)) =  T p
alphasub p y l@(((==y) -> True) :\ _) = l
alphasub p y (l :\ t) = l :\ alphasub p y t 
alphasub p y (s :# t) = alphasub p y s :# alphasub p y t 
alphasub p y x = x

-- main logic for beta-reduction. this solution unshadows all, changing all abstraction names
captures :: Eq a =>  Expr a -> Replace a -> Freshes a (Expr a)
captures (T x) (Replace (eq x -> True) r _)  = return r -- replace
captures v@(T _) _ = return v -- keep old
captures l@(x :\ _) (Replace (eq x -> True) _ _)  = return l -- the introduction cancels the replacement
captures (x :\ t) r = do
        p <- asks $ head . filter (not . (`elem` dontuse r)) . (x:)  -- throw away free variables in t, hostinate alpha 
        (\.) p <$> captures (alphasub p x t) r -- actual alpha transform
captures (t :# s) r = (#) <$> captures t r <*> captures s r -- let captures through both

-- single reduction step, hunting and collapsing  x \. y :# z pattern (lambda application)
reduction :: Eq a => Expr a -> Freshes a (Expr a)
reduction v@(T x) = return v -- reduction branch over
reduction (x :\ t) = (\.) x <$> reduction t -- lambda through
-- reduction ((x :\ t) :# y) = application x t y >>= reduction -- aggressive
-- reduction ((x :\ t) :# y) = reduction y >>= application x t -- mild
reduction ((x :\ t) :# y) = join (application x <$> reduction t <*> reduction y)
                                where application x t = captures t . replace x 
reduction ( e :# e') = (#) <$> reduction e <*> reduction e' -- application through

--  beta-reduction steps
betas :: Eq a => Expr a -> Freshes a [Expr a]
betas e = (e :) <$> do 
    e' <- reduction e
    if e == e' then return []
    else betas e'

beta :: Eq a => Expr a -> Freshes a (Expr a) 
beta e = last <$> betas e

---------------------------- alpha equality check -------------------

-- checking through alpha
data LambdaAlpha a  = BothFresh -- both not appear in corrispondance
                    | LeftFresh a -- x appear alone and the lost y is reported
                    | RightFresh a -- opposite case
                    | NoFresh a a -- both appears but not next each other
                    | Shadows -- full shadowing , both appear next to each other

-- state built on matching lambdas abstractions
data AlphaState a = AlphaState {
    _lostLeft :: [a], -- left abandoned by right shadowing
    _correspond :: [(a,a)], -- valid corrispondence
    _lostRight :: [a] -- right abandoned by left shadowing
    } deriving (Show)

makeLenses ''AlphaState

-- insert a new corrispondence keeping state clean, all bound variables appear at most once per side 
insert :: Eq a => (a,a) -> AlphaState a -> AlphaState a
insert (x,y) (AlphaState ls bs rs) = AlphaState (delete x ls) (((x,y):) .filter (\(x',y') -> not (x == x' || y == y')) $ bs) (delete y rs)

-- helper for matchAbstracted
correspondent   :: Eq a 
                => ASetter' (AlphaState a)  [a] -- correct one of the lost lists 
                -> [(a, a)] -- correspondence list
                -> a -- the element (fst) chosen
                -> Maybe (AlphaState a -> AlphaState a) -- Nothing is not found
correspondent f xs x = over f . (:) <$> lookup x xs

-- compute the tagged insertions of losts
matchAbstracted     :: Eq a 
                    => AlphaState a -- state
                    -> (a, a) -- new abstractions pair
                    -> LambdaAlpha (AlphaState a -> AlphaState a) -- tagged state change
matchAbstracted (view correspond -> c) ((`elem` c) -> True) = Shadows
matchAbstracted 
    ((id &&& map swap) . view correspond -> (xs,ys)) 
    (correspondent lostLeft xs -> y, correspondent lostRight ys -> x) = fromMaybe BothFresh $ 
        NoFresh <$> x <*> y <|>  LeftFresh <$> x  <|>  RightFresh <$> y        

-- zip the expressions 
(=:=) :: (Show a, Eq a) => Expr a -> Expr a -> Bool
(=:=) = alpha' $ AlphaState [] [] [] where
    -- terminals
    alpha' as (T x) (T y) = case matchAbstracted as (x,y)  of -- abuse match dropping meaning 
        Shadows -> True -- bounded corrispondents
        BothFresh ->    not (x `elem` view lostLeft as) -- x is free var
                    &&  not (y `elem` view lostRight as) -- y is free var
                    &&  x == y 
        _ -> error (show ("babbo",as))
    -- lambdas
    alpha' as (x :\ tx) (y :\ ty) = (\f -> alpha' (f as) tx ty) $ case matchAbstracted as (x,y)  of
        Shadows -> id
        z -> insert (x,y)  . case z of
            BothFresh -> id  
            LeftFresh y' -> y' 
            RightFresh x' -> x'
            NoFresh x' y' ->  y' . x'
    -- composition
    alpha' as (x1 :# x2) (y1 :# y2) = alpha' as x1 y1 && alpha' as x2 y2
    alpha' _ _ _ = error "mamma" 

------------ running ----------------------------------------------------

run :: Enum a => a -> Freshes a b -> b
run x = withFreshes (enumFrom x)

runBeta :: Eq a => [a] -> Expr a -> Expr a
runBeta xs = withFreshes xs . beta

runBetas xs = withFreshes xs . betas
----------- composing -------------------

a %# b = fmap (:#) a <*> b 

--------- famous -----------------------------------------------------

self (x:_) = x \. (T x # T x)
omega k = self k # self k
id_ (x:_) = x \. T x

zero (x:y:_) = x \. y \. T y
suc (x:z:w:_) = x \. z \. w  \. T z # (T x # T z # T w)
plus k@(x:y:z:w:_) = x \. y \. z \. w \. T x # T z # (T y # T z # T w)
false (x:y:_) = x \. y \. T y -- zero
true  (x:y:_) = x \. y \. T x -- const
and_ (x:y:_) = x \. y \. T x # T y # T x
not_ k@(x:_) = x \. T x # false k # true k
or_ (x:y:_) = x \. y \. T x # T x # T y




        

