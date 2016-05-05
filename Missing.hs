
{-# LANGUAGE FlexibleContexts, ConstraintKinds, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, NoMonomorphismRestriction #-}

module Missing where

import Data.Dependent.Map (DMap)
import Data.GADT.Compare (GCompare)
import Reflex hiding (combineDyn)
import qualified Reflex as Reflex 
import Reflex.Dom hiding (combineDyn)
import GHC.Exts (Constraint)
import Control.Monad.Identity (Identity)

-------  reflex missings --------------

type Morph t m a = Dynamic t (m a) -> m (Event t a)

joinE :: (Reflex t, MonadHold t f) => Event t (Event t a) -> f (Event t a)
joinE = fmap switch . hold never

mapMorph  :: (MonadHold t m, Reflex t) => Morph t m (Event t b) -> (a -> m (Event t b)) -> Dynamic t a -> m (Event t b)
mapMorph dyn f d = mapDyn f d >>= dyn >>= joinE

pick :: (GCompare k, Reflex t) => k a -> Event t (DMap k Identity) -> Event t a
pick x r = select (fan r) x

--------- buggy namings, wait for Dynamic functor instance ---------------

combineDynWith = Reflex.combineDyn
combineDyn = combineDynWith (,)


