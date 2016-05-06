
{-# LANGUAGE GADTs, ScopedTypeVariables, DataKinds, FlexibleContexts, Rank2Types, ConstraintKinds, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, NoMonomorphismRestriction #-}

module Missing where

import Data.Dependent.Map (DMap)
import Data.GADT.Compare (GCompare)
import Reflex hiding (combineDyn)
import qualified Reflex as Reflex 
import Reflex.Dom hiding (combineDyn)
import GHC.Exts (Constraint)
import Control.Monad.Identity (Identity)
import qualified GHCJS.DOM.EventM as J
import Data.IORef
import Control.Monad.Trans

-------  reflex missings --------------

type Morph t m a = Dynamic t (m a) -> m (Event t a)

joinE :: (Reflex t, MonadHold t f) => Event t (Event t a) -> f (Event t a)
joinE = fmap switch . hold never

mapMorph  :: (MonadHold t m, Reflex t) => Morph t m (Event t b) -> (a -> m (Event t b)) -> Dynamic t a -> m (Event t b)
mapMorph dyn f d = mapDyn f d >>= dyn >>= joinE

pick :: (GCompare k, Reflex t) => k a -> Event t (DMap k Identity) -> Event t a
pick x r = select (fan r) x

gateWith f = attachWithMaybe $ \allow a -> if f allow then Just a else Nothing
--------- buggy namings, wait for Dynamic functor instance ---------------

combineDynWith = Reflex.combineDyn
combineDyn = combineDynWith (,)

------------- Spider synonims

type ES = Event Spider
type DS = Dynamic Spider

-------------- Dom + spider synonyms

type MS = MonadWidget Spider

data WidgetEvents m b where
    GetEvent :: MonadIO m => (forall a. EventName a -> (ES (EventResultType a) -> ES b) -> m (ES b)) -> WidgetEvents m b


-- extract an "unique event" dispatcher for the widget, where results are collapsed in type b
eventsOf :: forall m w b. (MS m, HasDomEvent Spider w) => w -> m (WidgetEvents m b)
eventsOf w = do
    mm <- liftIO $ newIORef Nothing
    return $ GetEvent (match mm w)
                    
-- dont't export, hide the intricacy of the dispatcher here, as an example we imagine there are procedures to init Mousemove 
match :: (MS m, HasDomEvent Spider w) => IORef (Maybe (ES (EventResultType 'MousemoveTag))) -> w -> EventName a -> (ES (EventResultType a) -> ES b) -> m (ES b)
match _ w Mousedown f =  return . f $ domEvent Mousedown w -- dumb solution
match _ w Mouseup f = return . f $ domEvent Mouseup w -- again
match mm w Mousemove f = do
    m <- liftIO $ readIORef mm
    e  <- case m of 
        Nothing -> do 
                --- monadically initialize 'e' instead of what I do next
                let e = domEvent Mousemove w
                liftIO $ writeIORef mm (Just e)
                return e
        Just e -> return e
    return $ f e

-- export, a box opener for GetEvent 
getEvent        :: WidgetEvents m b 
                -> EventName a  -- event to match
                -> (ES (EventResultType a) -> ES b) -- collapser
                -> m (ES b) -- the safe event 
getEvent (GetEvent g) x f = g x f
