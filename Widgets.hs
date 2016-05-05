{-# LANGUAGE RecursiveDo #-}

module Widgets where
    

import Reflex.Dom
import qualified GHCJS.DOM.HTMLInputElement as J
import qualified GHCJS.DOM.Element as J
import Control.Lens (view, (^.))
import Data.Monoid ((<>))
import Control.Monad (forM)

--------------- a link opening on a new tab ------
linkNewTab :: MonadWidget t m => String -> String -> m ()
linkNewTab href s = elAttr "a" ("href" =: href <> "target" =: "_blank") $ text s

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
            
---------------- input widgets -----------------------------------------------
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

