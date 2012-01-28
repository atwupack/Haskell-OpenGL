-----------------------------------------------------------------------------
--
-- Module      :  Reactive.Banana.GLUT.Util
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Reactive.Banana.GLUT.Util (
    filterInc, injectB
) where

import Reactive.Banana

-- | Filter an 'Event' containung a number by an increment.
-- Events are discarded as long as the number has not been
-- incremented by at least a given value compared to the
-- last occurence of the event that passed this filter.
filterInc :: (Num a, Ord a) => Event a -> a -> Event a
filterInc e inc = result
    where
        result = filterApply ((\l n -> n >= l + inc) <$> last) e
        last =  stepper 0 result

injectB :: Behavior a -> Event b -> Event a
injectB be = apply (const <$> be)
