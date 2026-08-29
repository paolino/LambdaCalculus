module Missing where

import Data.Dependent.Map (DMap)
import Data.GADT.Compare (GCompare)
import Reflex
import Reflex.Dom.Core
import Control.Monad.Identity (Identity)

-------  reflex helpers not built in --------------

type Morph t m a = Dynamic t (m a) -> m (Event t a)

joinE :: (Reflex t, MonadHold t f) => Event t (Event t a) -> f (Event t a)
joinE = fmap switch . hold never

-- Dynamic has had a real Functor instance for a long time now, so this is
-- just dyn . fmap rather than the old mapDyn >>= dyn two-step.
mapMorph :: (MonadHold t m, Reflex t) => Morph t m (Event t b) -> (a -> m (Event t b)) -> Dynamic t a -> m (Event t b)
mapMorph dyn f d = dyn (f <$> d) >>= joinE

pick :: (GCompare k, Reflex t) => k a -> Event t (DMap k Identity) -> Event t a
pick x r = select (fan r) x
