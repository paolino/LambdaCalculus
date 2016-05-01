
{-# LANGUAGE RecursiveDo, NoMonomorphismRestriction #-}

import Reflex
import Reflex.Dom
import Lambda
import Parser
import Control.Arrow
import Data.Char
import Control.Monad (forM_)

(<$$) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
(<$$) f = ((f <$) <$>)

buttonX x = fmap (const $ toLower . head $ show x) <$> button (show x)
buttons = mapM buttonX [X .. B]

main = mainWidget $ do

    elClass "h2" "head" $ text "Lambda calculi"

    ss <- divClass "standard" $ do 
        f <- false  <$$ button "false"
        t <- true   <$$ button "true"
        a <- and_   <$$ button "and"
        i <- id_    <$$ button "id"
        z <- zero   <$$ button "zero"
        s <- suc    <$$ button "succ"
        p <- plus   <$$ button "plus"
        let tra x r =  reverse ("(" ++ pprint (x [X ..]) ++ ")") ++ r
        return $ map (fmap tra) [f,t,a,z,i,s,p]

    cs <- divClass "add" $ do
        l <- "(\\"  <$$  button "lambda"
        p <- "."    <$$  button "."
        o <- "("    <$$  button "("
        c <- ")"    <$$  button ")"
        bs <- buttons
        let tra x r =  reverse x ++ r
        return $ map (fmap tra) [l,p,o,c] ++ map (fmap (:)) bs

    ts <- divClass "control" $ do
        b <-  tail      <$$ button "back"
        t <-  const ""  <$$ button "reset"   
        return [t,b]

    t <- foldDyn ($) "" (leftmost $ cs ++ ts ++ ss) >>= mapDyn reverse

    divClass "edit" $ do 
        divClass "title" $ text "Expression"
        dynText t
    
    let r p = case parsing p of
            Left e -> divClass "edit" $ do
                        divClass "title" $ text "Not Parsed"
                        text $ show e
            Right e -> do
                divClass "edit" $ do
                    divClass "title" $ text "Beta reduction steps"
                    elClass "ol" "steps" $ 
                        forM_ (runBetas [X ..] e) $ elClass "li" "steps" . text . pprint
    
    mapDyn r t >>= dyn
    
    return () 

