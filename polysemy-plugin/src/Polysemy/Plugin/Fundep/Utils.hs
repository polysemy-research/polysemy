module Polysemy.Plugin.Fundep.Utils  where

import Control.Applicative
import Data.Bifunctor
import Data.List



------------------------------------------------------------------------------
-- | Returns the head of the list iff there is exactly one element.
singleListToJust :: [a] -> Maybe a
singleListToJust [a] = Just a
singleListToJust _ = Nothing


------------------------------------------------------------------------------
-- | Like 'Control.Monad.when', but in the context of an 'Alternative'.
whenA
    :: (Monad m, Alternative z)
    => Bool
    -> m a
    -> m (z a)
whenA False _ = pure empty
whenA True ma = fmap pure ma


------------------------------------------------------------------------------
-- | Count the number of times 'a' is present in the list.
countLength ::  Eq a => [a] -> [(a, Int)]
countLength as =
  let grouped = group as
   in zipWith (curry $ bimap head length) grouped grouped

