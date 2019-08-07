{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | This module provides Template Haskell functions for automatically generating
-- effect operation functions (that is, functions that use 'send') from a given
-- effect algebra. For example, using the @FileSystem@ effect from the example in
-- the module documentation for "Polysemy", we can write the following:
--
-- @
-- data FileSystem m a where
--   ReadFile  :: 'FilePath' -> FileSystem 'String'
--   WriteFile :: 'FilePath' -> 'String' -> FileSystem ()
--
-- 'makeSem' ''FileSystem
-- @
--
-- This will automatically generate (approximately) the following functions:
--
-- @
-- readFile :: 'Member' FileSystem r => 'FilePath' -> 'Sem' r 'String'
-- readFile a = 'send' (ReadFile a)
--
-- writeFile :: 'Member' FileSystem r => 'FilePath' -> 'String' -> 'Sem' r ()
-- writeFile a b = 'send' (WriteFile a b)
-- @
module Polysemy.Internal.TH.Effect
  ( makeSem
  , makeSem_
  ) where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Polysemy.Internal.CustomErrors (DefiningModule)
import Polysemy.Internal.TH.Common


-- TODO: write tests for what should (not) compile

------------------------------------------------------------------------------
-- | If @T@ is a GADT representing an effect algebra, as described in the
-- module documentation for "Polysemy", @$('makeSem' ''T)@ automatically
-- generates a smart constructor for every data constructor of @T@. This also
-- works for data family instances. Names of smart constructors are created by
-- changing first letter to lowercase or removing prefix @:@ in case of
-- operators. Fixity declaration is preserved for both normal names and
-- operators.
--
-- @since 0.1.2.0
makeSem :: Name -> Q [Dec]
makeSem = genFreer True


------------------------------------------------------------------------------
-- | Like 'makeSem', but does not provide type signatures and fixities. This
-- can be used to attach Haddock comments to individual arguments for each
-- generated function.
--
-- @
-- data Output o m a where
--   Output :: o -> Output o m ()
--
-- makeSem_ ''Output
--
-- -- | Output the value \@o\@.
-- output :: forall o r
--        .  Member (Output o) r
--        => o         -- ^ Value to output.
--        -> Sem r ()  -- ^ No result.
-- @
--
-- Because of limitations in Template Haskell, signatures have to follow some
-- rules to work properly:
--
-- * 'makeSem_' must be used /before/ the explicit type signatures
-- * signatures have to specify argument of 'Sem' representing union of
-- effects as @r@ (e.g. @'Sem' r ()@)
-- * all arguments in effect's type constructor have to follow naming scheme
-- from data constructor's declaration:
--
-- @
-- data Foo e m a where
--   FooC1 :: Foo x m ()
--   FooC2 :: Foo (Maybe x) m ()
-- @
--
-- should have @x@ in type signature of @fooC1@:
--
-- @fooC1 :: forall x r. Member (Foo x) r => Sem r ()@
--
-- and @Maybe x@ in signature of @fooC2@:
--
-- @fooC2 :: forall x r. Member (Foo (Maybe x)) r => Sem r ()@
--
-- * all effect's type variables and @r@ have to be explicitly quantified
-- using @forall@ (order is not important)
--
-- These restrictions may be removed in the future, depending on changes to
-- the compiler.
--
-- Change in (TODO(Sandy): version): in case of GADTs, signatures now only use
-- names from data constructor's type and not from type constructor
-- declaration.
--
-- @since 0.1.2.0
makeSem_ :: Name -> Q [Dec]
makeSem_ = genFreer False
-- NOTE(makeSem_):
-- This function uses an ugly hack to work --- it changes names in data
-- constructor's type to capturable ones. This allows user to provide them to
-- us from their signature through 'forall' with 'ScopedTypeVariables'
-- enabled, so that we can compile liftings of constructors with ambiguous
-- type arguments (see issue #48).
--
-- Please, change this as soon as GHC provides some way of inspecting
-- signatures, replacing code or generating haddock documentation in TH.


------------------------------------------------------------------------------
-- | Generates declarations and possibly signatures for functions to lift GADT
-- constructors into 'Sem' actions.
genFreer :: Bool -> Name -> Q [Dec]
genFreer should_mk_sigs type_name = do
  checkExtensions [ScopedTypeVariables, FlexibleContexts, DataKinds]
  (dt_name, cl_infos) <- getEffectMetadata type_name
  tyfams_on  <- isExtEnabled TypeFamilies
  def_mod_fi <- sequence [ tySynInstDCompat
                             ''DefiningModule
                             Nothing
                             [pure $ ConT dt_name]
                             (LitT . StrTyLit . loc_module <$> location)
                         | tyfams_on
                         ]
  decs <- traverse (genDec should_mk_sigs) cl_infos

  let sigs = if should_mk_sigs then genSig <$> cl_infos else []

  pure $ join $ def_mod_fi : sigs ++ decs


------------------------------------------------------------------------------
-- | Generates signature for lifting function and type arguments to apply in
-- its body on effect's data constructor.
genSig :: ConLiftInfo -> [Dec]
genSig cli
  =  maybe [] (pure . flip InfixD (cliFunName cli)) (cliFunFixity cli)
  ++ [ SigD (cliFunName cli) $ quantifyType
       $ ForallT [] (member_cxt : cliFunCxt cli)
       $ foldArrowTs sem
       $ fmap snd
       $ cliFunArgs cli
     ]
  where
    member_cxt = makeMemberConstraint (cliUnionName cli) cli
    sem        = makeSemType (cliUnionName cli) (cliEffRes cli)


------------------------------------------------------------------------------
-- | Builds a function definition of the form
-- @x a b c = send (X a b c :: E m a)@.
genDec :: Bool -> ConLiftInfo -> Q [Dec]
genDec should_mk_sigs cli = do
  let fun_args_names = fmap fst $ cliFunArgs cli

  pure
    [ PragmaD $ InlineP (cliFunName cli) Inlinable ConLike AllPhases
    , FunD (cliFunName cli)
        [ Clause (VarP <$> fun_args_names)
                 (NormalB $ makeUnambiguousSend should_mk_sigs cli)
                 []
        ]
    ]

