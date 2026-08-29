{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Widgets where

import Reflex.Dom.Core
import qualified GHCJS.DOM.HTMLInputElement as J
import Control.Lens (view, (^.), (.~), (&))
import Control.Monad (forM)
import qualified Data.Text as T

--------------- a link opening on a new tab ------
-- Text at the widget boundary now (reflex-dom-core's elAttr/text are Text,
-- not String); every call site passes literals, which OverloadedStrings
-- makes work unchanged.
linkNewTab :: MonadWidget t m => T.Text -> T.Text -> m ()
linkNewTab href s = elAttr "a" ("href" =: href <> "target" =: "_blank") $ text s

------------------ radio checkboxes ----------------------
--
radiocheckW :: (MonadHold t m,MonadWidget t m) => Eq a => a -> [(String,a)] -> m (Event t a)
radiocheckW j xs = do
    rec  es <- forM xs $ \(s,x) -> divClass "icheck" $ do
                    let d = def & setValue .~ (fmap (== x) $ updated result)
                    e <- fmap (const x) <$>  view checkbox_change <$> checkbox (x == j) d
                    text (T.pack s)
                    return e
         result <- holdDyn j $ leftmost es
    return $ updated result

---------------- input widgets -----------------------------------------------
insertAt :: Int -> String -> String -> (Int, String)
insertAt n e s = let (u,v) = splitAt n s
                  in (n + length e, u ++ e ++ v)

-- _textInput_element is a plain function in current reflex-dom-core, not a
-- Lens (the old app accessed it via `t ^. textInput_element`, which no
-- longer exists as a bare name).
attachSelectionStart :: MonadWidget t m => TextInput t -> Event t a -> m (Event t (Int, a))
attachSelectionStart t ev = performEvent . ffor ev $ \e -> do
  n <- J.getSelectionStart (_textInput_element t)
  return (n,e)

setCaret :: MonadWidget t m => TextInput t -> Event t Int -> m ()
setCaret t e = performEvent_ . ffor e $ \n -> do
  let el = _textInput_element t
  J.setSelectionStart el n
  J.setSelectionEnd el n

-- inputW/selInputW keep their public String-based types (matching Parser's
-- and PPrint's String-based Expr Char plumbing everywhere else); Text only
-- appears at the exact points where reflex-dom-core's TextInput requires it.

inputW :: MonadWidget t m => m (Event t String)
inputW = do
    rec let send = ffilter (==13) $ view textInput_keypress input -- send signal firing on *return* key press
        input <- textInput $ def & setValue .~ fmap (const "") send -- textInput with content reset on send
    return $ fmap T.unpack $ tag (current $ view textInput_value input) send -- tag the send signal with the inputText value BEFORE resetting

selInputW
  :: MonadWidget t m =>
     Event t String -> Event t String -> Event t b -> m (Dynamic t String)
selInputW insertionE refreshE resetE = do
  rec insertionLocE <- attachSelectionStart t insertionE
      let newE = attachWith (\s (n,e) -> insertAt n e (T.unpack s)) (current (value t)) insertionLocE
      setCaret t (fmap fst newE)
      t <- textInput $ def & setValue .~ leftmost [fmap (T.pack . snd) newE, fmap (const "") resetE, fmap T.pack refreshE]
  return $ fmap T.unpack $ view textInput_value t
