
{-# LANGUAGE RecursiveDo, NoMonomorphismRestriction, TupleSections, FlexibleContexts,ScopedTypeVariables,OverloadedLists, ConstraintKinds #-}

import Reflex hiding (combineDyn)
import qualified Reflex as Reflex
import Reflex.Dom hiding (combineDyn)
import Lambda
import Parser hiding (value)
import PPrint (Record,pprintdb, pprint)
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
import Data.Tuple

----------------------- combinator -------------------
(<$$) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
(<$$) f = ((f <$) <$>)

---------------- order preserving map ------------------
type MM k a = M.Map (Int,k) a

-- add a new value to a map, automatically choosing an unused key
new :: Ord k => (k,a) -> MM k a -> MM k a
new (k,v) m = case M.maxViewWithKey m of
    Nothing -> [((0,k),v)] -- overloadedlists
    Just (((n,_), _), _) -> M.insert (succ n,k) v m

mmAssocs :: MM k a -> [(k,a)]
mmAssocs = map (first snd) . M.assocs

---------------- input widget -----------------------------------------------
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

inputW :: MonadWidget t m => m (Event t String)
inputW = do
    rec let send = ffilter (==13) $ view textInput_keypress input -- send signal firing on *return* key press
        input <- textInput $ def & setValue .~ fmap (const "") send -- textInput with content reset on send
    return $ tag (current $ view textInput_value input) send -- tag the send signal with the inputText value BEFORE resetting

selInputW
  :: MonadWidget t m =>
     Event t String -> Event t b -> m (Dynamic t String)
selInputW insertionE resetE = do
  rec insertionLocE <- attachSelectionStart t insertionE
      let newE = attachWith (\s (n,e) -> insertAt n e  s) (current (value t)) insertionLocE
      setCaret t (fmap fst newE)
      t <- textInput $ def & setValue .~ leftmost [fmap snd newE, fmap (const "") resetE]
  return $ view textInput_value  t

--------------- a link opening on a new tab ------
linkNewTab :: MS m => String -> String -> m ()
linkNewTab href s = elAttr "a" ("href" =: href <> "target" =: "_blank") $ text s

-------  reflex missings --------------

joinE
  :: (Reflex t, MonadHold t f) =>
     Event t (Event t a) -> f (Event t a)
joinE = fmap switch . hold never

combineDynWith = Reflex.combineDyn
combineDyn = combineDynWith (,)


justThrough :: FunctorMaybe f => f (Maybe b) -> f b
justThrough = fmapMaybe (fmap id)

liveWidget f d = f <$> (d >>= dyn) >>= fmap switch . hold never

----------------------------------------------
------------------ End of libs---------------------
-----------------------------------------------------



------------ noisy type and constraint renaming ---------
type ES = Event Spider
type DS = Dynamic Spider
type MS = MonadWidget Spider

---------------------------------------------------
var_names = nub $ "xyzwnmlkij" ++ ['a' .. 'z']


------------------ header ------------------------
header = elClass "h2" "head" $ do
            divClass "matter" $ linkNewTab "https://en.wikipedia.org/wiki/Lambda_calculus" "Lambda Calculus"
            divClass "source" $ linkNewTab "http://github.com/paolino/LambdaCalculus" "source code"


------------- the storing new button widget -----------------
record :: MS m => m (ES [Char])
record = do
        s <- inputW
        elClass "span" "tooltip" $ text "reduction name"
        return $ fmap (map toUpper) s 

----------------- a button is an equivalence relation betweena name and an expression
type Button = (String, EC)

---- button definitions as a map (the only acceptable structure for Reflex.DOM ?)
type ButtonsDefs =  MM String EC

------------------- the beta reduction widget ----------------
--
-- Given a name -> expression eq list and an expression to parse and reduce
-- if beta succeed returns the last expression and an event with the 
-- name for the new button
--
reductionW :: MS m => (Maybe ButtonsDefs, String) -> m (Maybe (ES Button))
reductionW (dbm, p) = case parsing p of
            Left e -> divClass "edit" $ do
                        divClass "title" $ text "Not Parsed"
                        elClass "span" "tooltip" $ text $ "Use λ or ! or  \\ or / or ^ to open a lambda"
                        return Nothing
            Right e -> do
                divClass "edit" $ do
                    divClass "title" $ text "Beta reduction"
                    z <- fmap last . elClass "ol" "steps" $ 
                        forM (withFreshes var_names $ betas e) $ \r -> do 
                             elClass "li" "steps"  $ do
                                    text . pprintdb (maybe [] mmAssocs dbm) $ r
                                    return r
                    s <- record                        
                    return $ Just $ fmap (flip (,) z) s

--

----------------- an initial set of expressions ----------------
boot :: ButtonsDefs

boot = foldr new M.empty $ map swap [
    (false var_names,"FALSE"),
    (true var_names,"TRUE"),
    (and_ var_names,"AND"),
    (or_ var_names,"OR"),
    (id_ var_names,"ID"),
    (zero var_names,"ZERO"),
    (suc var_names,"SUCC"),
    (plus var_names,"PLUS")
    ]
--------------------- group button widget --------------------------
--



makeButton (_,k) v = fmap (pprint . fst) <$> attachDyn v <$> button k
--
buttonsW buttonsDef = divClass "standard" $ do 
            keys <- extrakeys
            dynOfEvents <- listWithKey buttonsDef makeButton >>= mapDyn (leftmost . M.elems)           
            return . leftmost $ (switch . current $ dynOfEvents) : keys


--
----------------- the expression field widget ---------------
--
expressionW buttons new_button = divClass "edit" $ do 
                (r,c) <- divClass "title" $ do
                            text "Expression"
                            b <- button "clear"
                            c <- divClass "usenames" $ do
                                    c <- checkbox False def
                                    elClass "span" "tooltip" $ text "substitute names"
                                    return c
                            return (b,view checkbox_value c)
                (,c) <$> divClass "expression" (selInputW buttons . leftmost $ [r, new_button])


--
footer = divClass "edit" . divClass "tooltip" $ text "Ghcjs + Reflex application. Kudos to them!"

extrakeys :: MS m => m [ES String]
extrakeys = mapM (\c -> c <$$ button c) ["(" ,"(λ","x" ,"y" ,"z" ,"w" ,"n" ,"m" ,"l" ,"." ,")"] 


substituteOrNot b = first $ if b then Just else const Nothing


main = mainWidget . void $ do
    header
    rec buttons <- buttonsW buttonsDef 

        (expression :: DS String, substitute :: DS Bool)  <- expressionW  buttons (const () <$> newButton)

        bdefsAndExpr :: DS (Maybe ButtonsDefs, String) <- combineDyn buttonsDef expression >>= combineDynWith substituteOrNot substitute 
        
        newButton :: ES Button <-  liveWidget justThrough (mapDyn reductionW bdefsAndExpr)

        buttonsDef :: DS ButtonsDefs  <- foldDyn new boot newButton
    footer
 
{-
    
-}
