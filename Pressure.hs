{-# LANGUAGE RecursiveDo, TemplateHaskell, ScopedTypeVariables, ConstraintKinds#-}
import qualified Data.Map as M
import Reflex.Dom
import Reflex.Dom.Contrib.Utils ()
import Reflex.Contrib.Utils (end)
import Control.Monad (void,forM)
import GHCJS.DOM.MouseEvent
import qualified GHCJS.DOM.EventM as J
import Data.FileEmbed
import ShowCode (mainWidgetWithAssets)
import Missing (gateWith,ES, eventsOf,getEvent)


data Color = Red | White | Blue deriving (Eq,Show)-- widget state


autoRelease = do
    rec     attribs <- mapDyn (\c -> M.fromList [("class","area"),("style","background-color:" ++ show c)]) widget
            -- let signal pass when the square is in a color
            events <- fst <$> elDynAttr' "div"  attribs end >>= eventsOf
            let     [isRed,isBlue,isWhite] = map (\c -> gateWith (==c) $ current widget) [Red,Blue,White] 
            downOnWhite <- getEvent events Mousedown $ isWhite . (Red <$)
            downOnBlue  <- getEvent events Mousedown $ isBlue . (White <$)
            upOnRed     <- getEvent events Mouseup $ isRed .  (White <$)
            -- wait one second after a  downOnWhite 
            autoBlue <-  (Blue <$) <$> delay 1 downOnWhite
            -- track downOnWhites - autoBlues 
            countReds <- foldDyn ($) 0 $ leftmost [(+1) <$ downOnWhite,subtract 1 <$ autoBlue]
            -- true with only one isBlue request
            isBlueCanDo <- mapDyn (==1) countReds
            -- finally the color
            widget <- holdDyn White (leftmost [downOnWhite,downOnBlue,upOnRed,isRed $  gate (current isBlueCanDo) autoBlue])
    end

main = mainWidgetWithAssets $(embedStringFile "Pressure.css") $(embedStringFile "Pressure.hs") $ do 
    divClass "header" $ text $ "A widget that turns red on click and white on release, but if you keep it clicked for 1 second it turns blue and next click back to white. It's the core logic for implementing widget morphisms based on persistent touch."
    autoRelease


