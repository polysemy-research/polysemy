{-# language AllowAmbiguousTypes, BangPatterns, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}

-- | Description: Interpreters for 'Meta'
module Polysemy.Meta (
  -- * Effect
  Meta,
  MetaEffect,
  (:%),
  mkIntoMeta,

  -- * Constructors
  sendMeta,

  -- * Last-resort constructor
  sendMetaVia,

  -- * Interpreters
  interpretMeta,
  metaToMeta,
  metaToMetaUsing,
  metaIntoMeta,

  -- * 'interpretMeta' Combinators
  MetaHandler,
  runMeta,
  runMeta',
  runExposeMeta,
  runExposeMeta',

  -- * Last-resort 'interpretMeta' combinators
  runMetaUsing,
  runMetaUsing',
  runExposeMetaUsing,
  runExposeMetaUsing',

  -- * Auxiliary type classes
  DepMember,
  AllSemRaises,
  ) where

import Data.Unique
import Control.Monad

import Polysemy
import Polysemy.Bundle
import Polysemy.Membership
import Polysemy.Internal
import Polysemy.Internal.Utils
import Polysemy.Internal.Meta
import Polysemy.Internal.HigherOrder
import System.IO.Unsafe
import Unsafe.Coerce

data MetaHandler metaeffect t rH l :: Effect where
  MetaHandlerMetaRun
    :: forall metaeffect t rH l m a
     . MetaRun m a -> MetaHandler metaeffect t rH l m a
  ProcessMeta
    :: forall eff z metaeffect t rH l m a
     . Unique
    -> ElemOf '(z, eff) l
    -> t ()
    -> z a
    -> MetaHandler metaeffect t rH l m
        (Sem (Meta metaeffect ': MetaHandler metaeffect t rH l ': rH) (t a))

data Box where
  Box :: a -> Box

newUnique' :: Box -> IO Unique
newUnique' (Box _) = newUnique
{-# NOINLINE newUnique' #-}

processMeta :: forall eff z metaeffect t rH l r a
             . ElemOf '(z, eff) l
            -> t ()
            -> z a
            -> Sem (MetaHandler metaeffect t rH l ': r)
                   (Unique,
                    Sem (Meta metaeffect ': MetaHandler metaeffect t rH l ': rH)
                        (t a))
processMeta pr t z = do
  let !uniq = unsafePerformIO (newUnique' (Box z))
      {-# NOINLINE uniq #-}
  m <- send (ProcessMeta uniq pr t z)
  return (uniq, m)
{-# NOINLINE processMeta #-}

exposeMetaRun :: forall eff metaeffect t rH l r a
               . ElemOf (MetaHandler metaeffect t rH l) r
              -> Unique -> Sem r a -> Sem (eff ': r) a
exposeMetaRun pr uniq =
  raise
  >>> interceptUsingH (There pr) \case
        MetaHandlerMetaRun (MetaRun uniq' act)
          | uniq' == uniq -> propagateUsing Here (unsafeCoerce act)
        metarun -> propagateUsing (There pr) metarun

class DepMember eff z l | z l -> eff where
  depMembership :: ElemOf '(z, eff) l

instance {-# OVERLAPPING #-}
      eff ~ eff'
  => DepMember eff z ('(z, eff') ': l) where
  depMembership = Here

instance DepMember eff z l
      => DepMember eff z (_t : l) where
  depMembership = There depMembership

runMeta :: forall r eff q metaeff z t rH l a mh
         . ( DepMember eff q l, Raise (mh ': rH) r
           , mh ~ MetaHandler metaeff t rH l)
        => q a
        -> Sem (eff
                ': HigherOrder z t (Meta metaeff) (mh ': rH)
                ': r) a
runMeta = runMetaUsing depMembership

runMetaUsing :: forall r eff q metaeff z t rH l a mh
              . (Raise (mh ': rH) r, mh ~ MetaHandler metaeff t rH l)
             => ElemOf '(q, eff) l
             -> q a
             -> Sem (eff
                     ': HigherOrder z t (Meta metaeff) (mh ': rH)
                     ': r) a
runMetaUsing pr = runExposeMetaUsing pr >=> raise . restoreH

runMeta' :: forall r eff q metaeff z t rH l a mh
          . (DepMember eff q l, Raise (mh ': rH) r,
             mh ~ MetaHandler metaeff t rH l)
         => q a
         -> Sem (eff
                 ': Meta metaeff
                 ': HigherOrder z t (Meta metaeff) (mh ': rH)
                 ': r) a
runMeta' = runMetaUsing' depMembership

runMetaUsing' :: forall r eff q metaeff z t rH l a mh
               . (Raise (mh ': rH) r, mh ~ MetaHandler metaeff t rH l)
              => ElemOf '(q, eff) l
              -> q a
              -> Sem (eff
                      ': Meta metaeff
                      ': HigherOrder z t (Meta metaeff) (mh ': rH)
                      ': r) a
runMetaUsing' pr = runExposeMetaUsing' pr >=> raise . raise . restoreH

runExposeMeta :: forall r eff q metaeff z t rH l a mh
               . (DepMember eff q l, Raise (mh ': rH) r,
                  mh ~ MetaHandler metaeff t rH l)
              => q a
              -> Sem (eff
                      ': HigherOrder z t (Meta metaeff) (mh ': rH)
                      ': r) (t a)
runExposeMeta = runExposeMetaUsing depMembership

runExposeMeta' :: forall r eff q metaeff z t rH l a mh
                . (DepMember eff q l, Raise (mh ': rH) r,
                  mh ~ MetaHandler metaeff t rH l)
               => q a
               -> Sem (eff
                       ': Meta metaeff
                       ': HigherOrder z t (Meta metaeff) (mh ': rH)
                       ': r) (t a)
runExposeMeta' = runExposeMetaUsing' depMembership

runExposeMetaUsing :: forall r eff q metaeff z t rH l a mh
                    . (Raise (mh ': rH) r, mh ~ MetaHandler metaeff t rH l)
                   => ElemOf '(q, eff) l
                   -> q a
                   -> Sem (eff
                           ': HigherOrder z t (Meta metaeff) (mh ': rH)
                           ': r) (t a)
runExposeMetaUsing pr q = do
  let mhMembership = There $ raiseMembership @(mh ': rH) @r Here
  t <- raise $ exposeH $ return ()
  (uniq, z) <- subsumeUsing (There mhMembership) (processMeta pr t q)
  InterpreterH interp <- raise getInterpreterH
  exposeMetaRun mhMembership uniq (raise_ (interp z))

runExposeMetaUsing' :: forall r metaeff eff q z t rH l a mh
                    . (Raise (mh ': rH) r, mh ~ MetaHandler metaeff t rH l)
                    => ElemOf '(q, eff) l
                    -> q a
                    -> Sem (eff
                            ': Meta metaeff
                            ': HigherOrder z t (Meta metaeff) (mh ': rH)
                            ': r) (t a)
runExposeMetaUsing' pr q = do
  let mhMembership = There $ There $ raiseMembership @(mh ': rH) @r Here
  t <- raise $ raise $ exposeH $ return ()
  (uniq, z) <- subsumeUsing (There mhMembership) (processMeta pr t q)
  z
    & mapMembership \case
        Here -> Here
        There pr' -> raiseMembership pr'
    & exposeMetaRun mhMembership uniq

newtype ProcessorH z t e r =
  ProcessorH (forall x. z x -> t () -> Sem (e ': r) (t x))

getProcessorH :: Sem (HigherOrder z t e rH ': r) (ProcessorH z t e rH)
getProcessorH = liftWithH $ \lwr ->
  return $ ProcessorH $ \z t -> lwr (restoreH t >> runH z)

interpretMeta
  :: forall metaeff r
   . (   forall l t z x mh
       . (Traversable t, mh ~ MetaHandler metaeff t r l)
      => metaeff l z x
      -> Sem (HigherOrder z t (Meta metaeff) (mh ': r) ': mh ': r) x
     )
  -> InterpreterFor (Meta metaeff) r
interpretMeta h =
    interpretH \case
      MetaRun q _ ->
        errorWithoutStackTrace $
          "Unhandled MetaRun with unique hash " ++ show (hashUnique q)
  . reinterpretH \case
      MetaMetaRun metarun -> propagate metarun
      SendMeta (metaeff :: metaeff l z x) to1 to2 -> do
        ProcessorH processor <- getProcessorH
        (_ :: TypeParamsH k t meta (MetaRun ': r)) <- getTypeParamsH
        let
          metaRunToMetaHandler
            :: forall y mh
             . mh ~ MetaHandler metaeff t r l
            => Sem (meta ': MetaRun ': r) y
            -> Sem (meta ': mh ': r) y
          metaRunToMetaHandler =
            (raiseUnder2 . raiseUnder2)
            >>> interpretH \case
              MetaMetaRun metarun ->
                propagateUsing (There (There Here)) (MetaHandlerMetaRun metarun)
              sendmeta -> propagate sendmeta
            >>> transformUsing (There Here) MetaHandlerMetaRun

          metaHandlerToOther
            :: forall r'
             . (forall n y. MetaRun n y -> Bundle r' n y)
            -> InterpreterFor (MetaHandler metaeff t r l) r'
          metaHandlerToOther toBdl = interpretH \case
            MetaHandlerMetaRun metarun | Bundle pr act <- toBdl metarun ->
              propagateUsing pr act
            ProcessMeta uniq pr t q -> return $
              processor (to2 uniq pr q) t
              & metaRunToMetaHandler

          rewriteHigherOrder
            :: forall r' y mh
             . mh ~ MetaHandler metaeff t r l
            => Sem (HigherOrder z t meta (mh ': r) ': r') y
            -> Sem (HigherOrder k t meta (MetaRun ': r) ': r') y
          rewriteHigherOrder = reinterpret \case
            WithProcessorH main -> withProcessorH $ \lwr -> return $
              main (metaRunToMetaHandler . lwr . to1)
            GetInterpreterH -> do
              InterpreterH interp <- getInterpreterH
              return $ InterpreterH $
                sink @'[_]
                >>> raiseUnder2
                >>> metaHandlerToOther (Bundle (There Here))
                >>> interp
                >>> rewrite MetaHandlerMetaRun
            LiftWithH main -> liftWithH $ \lwr -> return $
              main (lwr . rewriteHigherOrder)
            RestoreH t -> restoreH t

        h metaeff
          & (raiseUnder2 . raiseUnder2)
          & rewriteHigherOrder
          & subsumeUsing (There Here)
          & metaHandlerToOther (Bundle membership)
  where
    sink :: forall mid l r' y before after
          . (before ~ l ': (Append mid r'), after ~ Append mid (l ': r'),
             Subsume before after
            )
         => Sem before y -> Sem after y
    sink = subsume_
