{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_HADDOCK not-home, prune #-}

-- | Description: Class 'InsertAtIndex' that allows stack extension at a numeric index
module Polysemy.Internal.Index where

import GHC.TypeLits (Nat)
#if __GLASGOW_HASKELL__ < 902
import Type.Errors (ErrorMessage (ShowType), TypeError)
import Polysemy.Internal.CustomErrors (type (%), type (<>))
#endif

import Polysemy.Internal.Sing (SList (SCons, SEnd))

------------------------------------------------------------------------------
-- | Infer a partition of the result type @full@ so that for the fixed segments
-- @head@ and @tail@, the new segment @inserted@ contains the missing effects
-- between them.
class InsertAtIndex (index :: Nat) (head :: [k]) (tail :: [k]) (oldTail :: [k]) (full :: [k]) (inserted :: [k]) where
  insertAtIndex :: SList inserted

instance inserted ~ '[] => InsertAtIndex index head oldTail oldTail full inserted where
  insertAtIndex = SEnd

instance {-# INCOHERENT #-} (
    InsertAtIndex index head tail oldTail full insertedTail,
    inserted ~ (e ': insertedTail)
  ) => InsertAtIndex index head (e ': tail) oldTail full inserted where
    insertAtIndex = SCons (insertAtIndex @_ @index @head @tail @oldTail @full)

-- Broken on 9.2.
-- It appears that instance matching is done with an abstract value for @oldTail@, thus not matching the correct
-- instance and finding only this one, causing a false positive for the @TypeError@.
--
-- KingoftheHomeless: This is 100% a GHC bug. The coherent instance
-- /is found and used/ when this instance is not present, and both the other
-- instances are strictly more specific than this one. The presence of this
-- instance should not change instance resolution in the cases where any of
-- the other instances is a valid candidate, and yet it does!
#if __GLASGOW_HASKELL__ < 902
instance {-# INCOHERENT #-} TypeError (InsertAtFailure index oldTail head full)
       => InsertAtIndex index head tail oldTail full inserted where
  insertAtIndex = error "unreachable"


type InsertAtFailure index soughtTail head full =
  "insertAt: Failed to insert effects at index " <> 'ShowType index
  % "There is a mismatch between what's been determined as the head and tail between the newly inserted effects,"
  <> " and the actual desired return type."
  % "Determined head before inserted effects:"
  % "\t" <> 'ShowType head
  % "Determined tail after the inserted effects:"
  % "\t" <> 'ShowType soughtTail
  % "Actual desired return type:"
  % "\t" <> 'ShowType full
  % "Make sure that the index provided to insertAt is correct, and that the desired return type simply requires"
  <> " inserting effects."
#endif
