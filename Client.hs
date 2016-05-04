
{-# LANGUAGE RecursiveDo,StandaloneDeriving, NoMonomorphismRestriction, TupleSections, FlexibleContexts,ScopedTypeVariables,OverloadedLists, ConstraintKinds, TemplateHaskell #-}
{-# LANGUAGE GADTs#-}
import Data.Dependent.Map (DMap, DSum( (:=>) ) , fromList)
import Data.GADT.Compare.TH
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
import Data.Maybe.HT (toMaybe)
import Data.Either
import Data.GADT.Compare

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

------------------ radio checkboxes ----------------------
--
radiocheckW :: (MonadHold t m,MonadWidget t m) => Eq a => a -> [(String,a)] -> m (Event t a)
radiocheckW j xs = do
    rec  es <- forM xs $ \(s,x) -> divClass "icheck" $ do
                    let d = def & setValue .~ (fmap (== x) $ updated result)
                    e <- fmap (const x) <$>  view checkbox_change <$> checkbox (x == j) d
                    text s
                    return e
         result <- holdDyn j $ leftmost es
    return $ updated result
            
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
     Event t String -> Event t String -> Event t b -> m (Dynamic t String)
selInputW insertionE refreshE resetE = do
  rec insertionLocE <- attachSelectionStart t insertionE
      let newE = attachWith (\s (n,e) -> insertAt n e  s) (current (value t)) insertionLocE
      setCaret t (fmap fst newE)
      t <- textInput $ def & setValue .~ leftmost [fmap snd newE, fmap (const "") resetE, refreshE]
  return $ view textInput_value  t

--------------- a link opening on a new tab ------
linkNewTab :: MonadWidget t m => String -> String -> m ()
linkNewTab href s = elAttr "a" ("href" =: href <> "target" =: "_blank") $ text s

-------  reflex missings --------------

joinE :: (Reflex t, MonadHold t f) => Event t (Event t a) -> f (Event t a)
joinE = fmap switch . hold never

combineDynWith = Reflex.combineDyn
combineDyn = combineDynWith (,)

justThrough :: Reflex t => Event t (Maybe b) -> Event t b
justThrough = fmapMaybe (fmap id)

---------------- these are wrong ,wait for Unalign instance ----------------------------
pick :: (GCompare k, Reflex t) => k a -> Event t (DMap k Identity) -> Event t a
pick x r = select (fan r) x
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
        elClass "span" "tooltip" $ text "save to button"
        return $ fmap (map toUpper) s 

----------------- a button is an equivalence relation betweena name and an expression
type Button = (String, EC)

---- button definitions as a map (the only acceptable structure for Reflex.DOM ?)
type ButtonsDefs =  MM String EC

------------------- the beta reduction widget ----------------
--
-- This is a morphing (dyn) widget, in fact it does depend on a pure value. It will be
-- used inside \x -> mapDyn x d >>= dyn >>= joinE. The signature is even simpler than for
-- no-morphing as we are supported by mapDyn, but we cannot return Dynamics (DS), in fact
-- we must return m (ES a). To rely on one only event we need to use the 'merge' primitive
-- which takes care of merging events right. The ReductionE datatype is necessary to
-- tag each Event right.

-- the three types of event to generated by this widget
data ReductionE a where
    NewButton :: ReductionE Button -- a new button was requested
    NewEdit :: ReductionE EC  -- a text to substitute the expression was requested
    ChangeTactic :: ReductionE Tactic -- a change in evaluation strategy was requested
    
-- Very handy stuff ,necessary for 'merge'. Write this instances by hand is quadratic in cases on the number of constructor/types
deriveGEq ''ReductionE
deriveGCompare ''ReductionE

-- the input type for the widget
data ReductionInput = ReductionInput 
    Tactic -- the state of the tactic is kept outside the widget !! It's recovered here: persistence
    (Maybe ButtonsDefs) -- this is the name substitution store, Nothing says no substitution
    String -- this is the expression at the edit field

-- the schema is input -> m (ES output). We differentiate the morphs with Maybe. First case, there is no
-- output as the expression doesn't parse, ES never is returned.
-- Second case the resuction widget is created and the ES DMap is returned
reductionW :: MS m => ReductionInput -> m (ES (DMap ReductionE Identity))
reductionW (ReductionInput red dbm p) = maybe never id <$> case parsing p of
            Left e -> divClass "edit" $ do
                        divClass "title" $ text "Not Parsed"
                        elClass "span" "tooltip" $ text $ "Use λ or ! or  \\ or / or ^ to open a lambda"
                        return Nothing
            Right e -> do
                divClass "edit" $ do
                    divClass "title" $ do
                                text "Beta reduction"
                    r <- divClass "reduction_choice" $ radiocheckW red  [("aggressive",Aggressive) , ("mild",Mild), ("normal",Normal)] 
                    (bs,es) <- fmap unzip . elClass "ol" "steps" $ 
                        forM (withFreshes var_names $ betas red e) $ \r -> do 
                             elClass "li" "steps"  $ do
                                    text . pprintdb (maybe [] mmAssocs dbm) $ r
                                    b <- elClass "span" "up" $ button "edit"
                                    return (r, fmap (const r) b)
                    s <- record          
                    -- creation of the DMap ReductionE Identity tag :=> ES of the right type              
                    let     b = NewButton :=> fmap (flip (,) (last bs)) s
                            e = NewEdit :=> leftmost es
                            r' = ChangeTactic :=> r
                    return . Just . merge $ fromList [b,e,r']

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


extrabuttons :: MS m => m [ES String]
extrabuttons = mapM (\c -> c <$$ button c) ["(" ,"(λ","x" ,"y" ,"z" ,"w" ,"n" ,"m" ,"l" ,"." ,")"] 


makeButton :: MS m => String -> DS EC -> m (ES String)
makeButton k v = fmap (pprint . fst) <$> attachDyn v <$> button k
--
buttonsW :: (MonadWidget Spider m) => DS ButtonsDefs -> m (ES String)
buttonsW buttonsDef = divClass "standard" $ do 
            keys <- extrabuttons
            dynOfEvents <- listWithKey buttonsDef (makeButton . snd) >>= mapDyn (leftmost . M.elems)           
            return . leftmost $ (switch . current $ dynOfEvents) : keys


--
----------------- the expression field widget ---------------
--
-- expressionW :: MS m => ES String -> ES String -> m (DS String,  DS Bool)
expressionW buttons picked = divClass "edit" $ do 
                divClass "title" $ do
                                    text "Expression"
                c <- divClass "usenames" $ do
                                    c <- checkbox False def
                                    elClass "span" "tooltip" $ text "substitute names"
                                    return c
                b <- divClass "clear" $ button "clear"
                (,view checkbox_value c) <$> divClass "expression" (selInputW buttons (fmap pprint picked) b)


----------------- a dumb footer ---------------------------
--
footer :: MS m => m ()
footer = divClass "edit" . divClass "tooltip" $ text "Ghcjs + Reflex application. Kudos to them!"

main = mainWidget . void $ do

    header

    rec buttons <- buttonsW buttonsDef 

        (expression :: DS String, substitute :: DS Bool)  <- expressionW  buttons upEC
        
        -- horrible :-/ ---
        bdefsAndExpr :: DS ReductionInput <- do
              bf <- combineDynWith toMaybe substitute buttonsDef
              f <- combineDynWith ReductionInput redType bf
              combineDynWith ($) f expression
            
        reductionE :: ES (DMap ReductionE Identity) <- mapDyn reductionW bdefsAndExpr >>= dyn >>= joinE
        
        let     newButton = pick NewButton reductionE
                upEC = pick NewEdit reductionE

        redType <- holdDyn Normal $ pick ChangeTactic reductionE

        buttonsDef :: DS ButtonsDefs  <- foldDyn new boot newButton

    footer
{-
-} 
