{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE PatternSynonyms   #-}

#if __GLASGOW_HASKELL__ < 806

module Polysemy.Plugin.InlineRecursiveCalls where

#else

module Polysemy.Plugin.InlineRecursiveCalls
  ( inlineRecCallsAction
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans.State
import Data.Generics
import Data.Maybe
import Data.Monoid

import Bag
import GhcPlugins hiding ((<>))
import HsSyn


------------------------------------------------------------------------------
-- | Forces compiler to inline functions by creating loopbreaker with
-- NO_INLINE pragma, changing recursive calls to use it and by adding INLINE
-- pragma to the original function.
inlineRecCallsAction :: (MonadUnique m, HasDynFlags m)
                     => HsGroup GhcRn -> m (HsGroup GhcRn)
inlineRecCallsAction group = do
  dyn_flags <- getDynFlags

  ( if optLevel dyn_flags > 0
      then inlineRecCalls
      else pure
    ) group

------------------------------------------------------------------------------
inlineRecCalls :: MonadUnique m => HsGroup GhcRn -> m (HsGroup GhcRn)
inlineRecCalls group@(hs_valds -> XValBindsLR (NValBinds binds sigs)) = do

  (binds', extra_sigs) <- second concat . unzip
                      <$> traverse inlineRecCall (pairTypes binds sigs)

  pure group{ hs_valds = XValBindsLR $ NValBinds binds' $ sigs ++ extra_sigs }

inlineRecCalls _ = error "inlineRecCalls: expected renamed group"

------------------------------------------------------------------------------
-- | Pairs single binds with their types if they have one specified.
pairTypes :: [(RecFlag, LHsBinds GhcRn)]
          -> [LSig GhcRn]
          -> [((RecFlag, LHsBinds GhcRn), Maybe (LHsSigWcType GhcRn))]
-- TODO: better way of doing this would be great
pairTypes binds sigs = flip evalState sigs $ foldM go [] binds where
  go pairs = \case
    b@(_, bagToList -> [L _ FunBind{ fun_id = L _ fun_name }]) -> do
      sig <- findMapPop $ \case
        L _ (TypeSig _ sig_names bind_type)
          | elem fun_name $ unLoc <$> sig_names -> Just bind_type
        _                                       -> Nothing
      pure $ (b, sig) : pairs

    b -> pure $ (b, Nothing) : pairs

------------------------------------------------------------------------------
-- | Takes binding group and type of binding inside if there's only one and
-- returns group with inlining of recursive calls and signatures to be added
-- to the environment.
inlineRecCall :: MonadUnique m
              => ((RecFlag, LHsBinds GhcRn), Maybe (LHsSigWcType GhcRn))
              -> m ((RecFlag, LHsBinds GhcRn), [LSig GhcRn])
inlineRecCall (b, mtype)
  | (Recursive, (bagToList -> [
        L fun_loc fun_bind@FunBind{ fun_id = L _ fun_name, fun_matches }
      ])) <- b
  = do
      (loopb_name, loopb_decl) <- loopbreaker fun_name

      pure $ case maybeReplaceVarNames fun_name loopb_name fun_matches of
        Just fun_matches' -> (,)
          ( Recursive
          , listToBag
              [ L fun_loc fun_bind{ fun_matches = fun_matches' }
              , loopb_decl
              ]
          )
          (  [ inline alwaysInlinePragma                           fun_name
             , inline defaultInlinePragma{ inl_inline = NoInline } loopb_name
             ]
          ++ maybeToList (loopbreakerSig loopb_name <$> mtype)
          )
        Nothing -> (b, [])
    -- We ignore mutually recursive and other bindings
  | otherwise = pure (b, [])

------------------------------------------------------------------------------
-- | Creates loopbreaker and it's name from name of original function.
loopbreaker :: MonadUnique m => Name -> m (Name, LHsBind GhcRn)
loopbreaker fun_name =
  (id &&& loopbreakerDecl fun_name) <$> loopbreakerName fun_name

------------------------------------------------------------------------------
loopbreakerName :: MonadUnique m => Name -> m Name
loopbreakerName (nameOccName -> occNameFS -> orig_fs) =
  flip mkSystemVarName (orig_fs <> "__polysemy_loopbreaker") <$> getUniqueM

------------------------------------------------------------------------------
loopbreakerDecl :: Name -> Name -> LHsBind GhcRn
loopbreakerDecl fun_name loopb_name =
  noLoc $ mkTopFunBind Generated (noLoc loopb_name)
    [ mkSimpleMatch (mkPrefixFunRhs $ noLoc loopb_name) [] $
        nlHsVar fun_name
    ]

------------------------------------------------------------------------------
-- | Creates loopbreaker type signature from type of original function.
loopbreakerSig :: Name -> LHsSigWcType GhcRn -> LSig GhcRn
loopbreakerSig loopb_name fun_type =
  noLoc $ TypeSig NoExt [noLoc loopb_name] fun_type

------------------------------------------------------------------------------
inline :: InlinePragma -> IdP GhcRn -> LSig GhcRn
inline how name = noLoc $ InlineSig NoExt (noLoc name) how

------------------------------------------------------------------------------
-- | Returns value with every variable expression id replaced or 'Nothing' if
-- there's none to be replaced.
maybeReplaceVarNames :: Data a => Name -> Name -> a -> Maybe a
maybeReplaceVarNames from to expr =
  case everywhereM (mkM go) expr of
    (Any True, expr') -> Just expr'
    _                 -> Nothing
 where
  go :: HsExpr GhcRn -> (Any, HsExpr GhcRn)
  go (HsVar _ (L loc name))
    | name == from = (Any True, HsVar NoExt $ L loc to)
  go e             = (Any False, e)

------------------------------------------------------------------------------
findMapPop :: Monad m => (a -> Maybe b) -> StateT [a] m (Maybe b)
findMapPop = state . go where
  go _ []     = (Nothing, [])
  go f (x:xs) = case f x of
    Nothing -> second (x:) $ go f xs
    just    -> (just, xs)

#endif
