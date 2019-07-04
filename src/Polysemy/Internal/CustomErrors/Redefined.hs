{-# LANGUAGE ConstraintKinds #-}

------------------------------------------------------------------------------
-- | This code is copied verbatim from 'Type.Errors' due to limitations in the
-- (GHC 8.6) plugin machinery. See
-- <https://github.com/polysemy-research/polysemy/issues/152 #152> for more
-- info.
module Polysemy.Internal.CustomErrors.Redefined where

import Type.Errors hiding (IfStuck, WhenStuck, UnlessStuck)


------------------------------------------------------------------------------
-- | @'IfStuck' expr b c@ leaves @b@ in the residual constraints whenever
-- @expr@ is stuck, otherwise it 'Eval'uates @c@.
type family IfStuck (expr :: k) (b :: k1) (c :: Exp k1) :: k1 where
  -- The type pattern @_ Foo@ is interpretered by the compiler as being of
  -- any kind. This is great and exactly what we want here, except that things
  -- like @forall s. Maybe s@ will get stuck on it.
  --
  -- So instead, we just propagate out 100 of these type variables and assume
  -- that 100 type variables ought to be enough for anyone.
  IfStuck (_ AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind) b c = b
  IfStuck a                     b c = Eval c

data AnythingOfAnyKind


------------------------------------------------------------------------------
-- | Like 'IfStuck', but specialized to the case when you don't want to do
-- anything if @expr@ isn't stuck.
type WhenStuck expr b   = IfStuck expr b NoErrorFcf


------------------------------------------------------------------------------
-- | Like 'IfStuck', but leaves no residual constraint when @expr@ is stuck.
-- This can be used to ensure an expression /isn't/ stuck before analyzing it
-- further.
type UnlessStuck expr c = IfStuck expr NoError c

