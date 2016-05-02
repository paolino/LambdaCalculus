
{-# LANGUAGE RecursiveDo, NoMonomorphismRestriction, FlexibleContexts,ScopedTypeVariables #-}

import Reflex
import Reflex.Dom
import Lambda
import Parser hiding (value)
import PPrint
import Control.Arrow
import Data.Char
import Control.Monad (forM_)
import Control.Monad
import Control.Lens
import Data.List
import Data.Maybe
import qualified GHCJS.DOM.HTMLInputElement as J
import qualified GHCJS.DOM.Element as J
import qualified Data.Map as M
import Data.Monoid

variables = nub $ "xyzwnmlkij" ++ ['a' .. 'z']

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

inputW :: MonadWidget t m => m (Event t String)
inputW = do
    rec let send = ffilter (==13) $ view textInput_keypress input -- send signal firing on *return* key press
        input <- textInput $ def & setValue .~ fmap (const "") send -- textInput with content reset on send
    return $ tag (current $ view textInput_value input) send -- tag the send signal with the inputText value BEFORE resetting

(<$$) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
(<$$) f = ((f <$) <$>)


result p = case parsing p of
            Left e -> divClass "edit" $ do
                        divClass "title" $ text "Not Parsed"
                        elClass "span" "tooltip" $ text $ "Use λ or ! or  \\ or / or ^ to open a lambda"
                        return Nothing
            Right e -> do
                divClass "edit" $ do
                    divClass "title" $ text "Beta reduction"
                    z <- fmap last . elClass "ol" "steps" $ 
                        forM (withFreshes variables $ betas e) $ \r -> do 
                            elClass "li" "steps" . text . pprint $ r
                            return r
                    return$ Just z

linkNewTab :: MonadWidget t m => String -> String -> m ()
linkNewTab href s = elAttr "a" ("href" =: href <> "target" =: "_blank") $ text s

main = mainWidget $ do

    elClass "h2" "head" $ do
            divClass "matter" $ linkNewTab "https://en.wikipedia.org/wiki/Lambda_calculus" "Lambda Calculus"
            divClass "source" $ linkNewTab "http://github.com/paolino/LambdaCalculus" "source code"

    rec ss <- divClass "standard" $ do 
            f <- false  <$$ button "false"
            t <- true   <$$ button "true"
            a <- and_   <$$ button "and"
            o <- or_    <$$ button "or"
            i <- id_    <$$ button "id"
            z <- zero   <$$ button "zero"
            s <- suc    <$$ button "succ"
            p <- plus   <$$ button "plus"
            zs' <- listWithKey   us (\k v -> fmap (pprint . fst) <$> attachDyn v <$> button k)
            zs <- mapDyn (leftmost . M.elems) zs'          
            op <- "(" <$$ button "("
            cl <- ")" <$$ button ")"
            let tra x = pprint (x variables)
            return $ map (fmap tra) [f,t,a,z,i,s,p,o] ++ [switch . current $ zs] ++ [op,cl]

        (t :: Dynamic Spider String)  <- divClass "edit" $ do 
                r <- divClass "title" $ do
                            text "Expression"
                            button "clear"
                r' <- divClass "expression" $ 
                    view textInput_value <$> expression (leftmost ss ) (leftmost $ [r, fmap (const ()) s])
                return r'

        (z :: Event Spider (Expr Char)) <-  fmap fromJust <$> (ffilter isJust) <$> (mapDyn result t >>= dyn) -- :: _ (Event Spider (Maybe (Expr Char))) 
        store :: Event Spider (Expr Char, String) <- flip attach s <$> hold (T 'X') z
        (us :: Dynamic Spider (M.Map String (Expr Char))) <- foldDyn (\(x,y) s -> M.insert y x s) M.empty store
        s <-    divClass "edit" $ divClass "title" $ do
                 
                    s <-inputW 
                    elClass "span" "tooltip" $ text "name the result"
                    return s
    return () 
 
    
