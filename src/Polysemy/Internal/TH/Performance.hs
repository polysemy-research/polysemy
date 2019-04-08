{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Internal.TH.Performance
  ( inlineRecursiveCalls
  ) where

import Control.Monad
import Data.Bool
import Data.Maybe (maybeToList, mapMaybe)
import Data.Monoid (Any (..))
import Generics.SYB
import Language.Haskell.TH

------------------------------------------------------------------------------
-- | GHC has a really hard time inlining recursive calls---such as those used in
-- interpreters for higher-order effects. This can have disastrous repercussions
-- for your performance.
--
-- Fortunately there's a solution, but it's ugly boilerplate. You can enable
-- @-XTemplateHaskell@ and use 'inlineRecursiveCalls' to convince GHC to make
-- these functions fast again.
--
-- @
-- inlineRecursiveCalls [d|
--   factorial :: Int -> Int
--   factorial 0 = 1
--   factorial n = n * factorial (n - 1)
--   |]
-- @
inlineRecursiveCalls :: Q [Dec] -> Q [Dec]
inlineRecursiveCalls m = do
  decs <- m
  let types   = mapMaybe getType decs
      inlines = mapMaybe hasInline decs
  fmap join $ traverse (loopbreaker types inlines) decs


isRecursive :: Name -> [Clause] -> Bool
isRecursive n cs =
  getAny $
    everything
      (<>)
      (mkQ (Any False) $ withRec (const $ Any False) (Any True) n)
      cs


withRec :: (Exp -> a) -> a -> Name -> Exp -> a
withRec unmatched matched n = \case
  VarE n' | n == n' -> matched
  a                 -> unmatched a


getType :: Dec -> Maybe (Name, Type)
getType (SigD n t) = Just (n, t)
getType _ = Nothing


hasInline :: Dec -> Maybe (Name)
hasInline (PragmaD (InlineP n Inline _ _)) = Just n
hasInline _ = Nothing


loopbreaker :: [(Name, Type)] -> [Name] -> Dec -> Q [Dec]
loopbreaker types inlined (FunD n cs)
  | isRecursive n cs = do
      nLB <- newName $ mconcat
               [ "___"
               , nameBase n
               , "___loop_breaker"
               ]
      pure $
        [ FunD n $ everywhere (mkT $ withRec id (VarE nLB) n) cs
        , FunD nLB [Clause [] (NormalB $ VarE n) []]
        , PragmaD $ InlineP nLB NoInline FunLike AllPhases
        ] ++ maybeToList (fmap (SigD nLB) $ lookup n types)
          ++ bool [PragmaD $ InlineP n Inline FunLike AllPhases]
                  []
                  (elem n inlined)
loopbreaker _ _ z = pure [z]

