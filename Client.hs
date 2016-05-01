
{-# LANGUAGE RecursiveDo, NoMonomorphismRestriction, FlexibleContexts,ScopedTypeVariables #-}

import Reflex
import Reflex.Dom
import Lambda
import Parser hiding (value)
import Control.Arrow
import Data.Char
import Control.Monad (forM_)
import Control.Monad
import Control.Lens
import Data.Maybe
import qualified GHCJS.DOM.HTMLInputElement as J
import qualified GHCJS.DOM.Element as J
import qualified Data.Map as M

insertAt :: Int -> String -> String -> (Int, String)
insertAt n e s = let (u,v) = splitAt n s
                  in (n + length e, u ++ e ++ v)

attachSelectionStart :: MonadWidget t m => TextInput t -> Event t a -> m (Event t (Int, a))
attachSelectionStart t ev = performEvent . ffor ev $ \e -> do
  n <- J.getSelectionStart (t ^. textInput_element)
  return (n,e)

setCaret :: MonadWidget t m => TextInput t -> Event t Int -> m ()
setCaret t e = performEvent_ . ffor e $ \n -> do
  let el = t ^. textInput_element
  J.setSelectionStart el n
  J.setSelectionEnd el n

expression insertionE resetE = do
  rec insertionLocE <- attachSelectionStart t insertionE
      let newE = attachWith (\s (n,e) -> insertAt n e  s) (current (value t)) insertionLocE
      setCaret t (fmap fst newE)
      t <- textInput $ def & setValue .~ leftmost [fmap snd newE, fmap (const "") resetE]
  return t

(<$$) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
(<$$) f = ((f <$) <$>)

buttonX x = fmap (const $ toLower . head $ show x) <$> button (show x)
buttons = mapM buttonX [X .. B]

result p = case parsing p of
            Left e -> divClass "edit" $ do
                        divClass "title" $ text "Not Parsed"
                        text $ show "use ! or  \\ or / to open a lambda"
                        return Nothing
            Right e -> do
                divClass "edit" $ do
                    divClass "title" $ text "Beta reduction steps"
                    z <- fmap last . elClass "ol" "steps" $ 
                        forM (runBetas [X ..] e) $ \r -> do 
                            elClass "li" "steps" . text . pprint $ r
                            return r
                    return$ Just z

{-
    ts <- divClass "control" $ do
        b <-  tail      <$$ button "back"
        t <-  const ""  <$$ button "reset"   
        return [t,b]
-}
main = mainWidget $ do

    elClass "h2" "head" $ do
            divClass "matter" $ text "Lambda Calculus"
            divClass "source" $ do
                text "source: "
                link "http://github.com/paolino/LambdaCalculus"

    rec ss <- divClass "standard" $ do 
            f <- false  <$$ button "false"
            t <- true   <$$ button "true"
            a <- and_   <$$ button "and"
            o <- or_   <$$ button "or"
            i <- id_    <$$ button "id"
            z <- zero   <$$ button "zero"
            s <- suc    <$$ button "succ"
            p <- plus   <$$ button "plus"
            -- ns <- mapM (\(d,n) -> d  <$$ button n) news
            zs' <- listWithKey   us (\k v -> fmap (pprint . fst) <$> attachDyn v <$> button k)
            zs <- mapDyn (leftmost . M.elems) zs'          

            let tra x = pprint (x [X ..])
            return $ map (fmap tra) [f,t,a,z,i,s,p,o] ++ [switch . current $ zs]
        cs <- divClass "add" $ do
            l <- "(\\"  <$$  button "lambda"
            p <- "."    <$$  button "."
            o <- "("    <$$  button "("
            c <- ")"    <$$  button ")"
            return $ [l,p,o,c]
        (t :: Dynamic Spider String, s :: Event Spider String)  <- divClass "edit" $ do 
                (r,s) <- divClass "title" $ do
                        text "Expression"
                        r <- button "clear"
                        b <- button "store result as"
                        name <- textInput def 
                        return $ (r,fmap fst $ attachDyn (view textInput_value name) b)
                r' <- divClass "expression" $ 
                    view textInput_value <$> expression (leftmost $ ss ++ cs ) r
                return (r',s)

        (z :: Event Spider (Expr V)) <-  fmap fromJust <$> (ffilter isJust) <$> (mapDyn result t >>= dyn) -- :: _ (Event Spider (Maybe (Expr V))) 
        store :: Event Spider (Expr V, String) <- flip attach s <$> hold (T X) z
        (us :: Dynamic Spider (M.Map String (Expr V))) <- foldDyn (\(x,y) s -> M.insert y x s) M.empty store
    return () 
 
    
