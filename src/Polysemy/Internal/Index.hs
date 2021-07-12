{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.Index where

import GHC.TypeLits (Nat)
import Type.Errors (TypeError, ErrorMessage(ShowType))

import Polysemy.Internal.CustomErrors (type (<>), type (%))
import Polysemy.Internal.Sing (SList (SEnd, SCons))

------------------------------------------------------------------------------
-- | Infer a partition of the result type @full@ so that for the fixed segments
-- @head@ and @tail@, the new segment @inserted@ contains the missing effects
-- between them.
class InsertAtIndex (index :: Nat) (head :: [k]) (tail :: [k]) (oldTail :: [k]) (full :: [k]) (inserted :: [k]) where
  insertAtIndex :: SList inserted

instance inserted ~ '[] => InsertAtIndex index head oldTail oldTail full inserted where
  insertAtIndex = SEnd
  {-# INLINE insertAtIndex #-}

instance {-# INCOHERENT #-} (
    InsertAtIndex index head tail oldTail full insertedTail,
    inserted ~ (e ': insertedTail)
  ) => InsertAtIndex index head (e ': tail) oldTail full inserted where
    insertAtIndex = SCons (insertAtIndex @_ @index @head @tail @oldTail @full)
    {-# INLINE insertAtIndex #-}

instance {-# INCOHERENT #-} TypeError (InsertAtFailure index oldTail head full)
       => InsertAtIndex index head tail oldTail full inserted where
  insertAtIndex = error "unreachable"

type family InsertAtUnprovidedIndex where
  InsertAtUnprovidedIndex = TypeError (
    "insertAt: You must provide the index at which the effects should be inserted as a type application."
    % "Example: insertAt @5"
    )

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
