{-# LANGUAGE TemplateHaskell #-}

module Polysemy.RPC where

import Data.List
import Polysemy
import Polysemy.Error
import Polysemy.Internal.TH.EffectLib
import Language.Haskell.TH


data RPC m a where
  SendMessage :: String -> RPC m ()
  RecvMessage :: RPC m String

makeSem ''RPC

data RPCError
  = MissingEffect String
  | MissingConstructor String
  | DecodingError
  deriving (Eq, Ord, Show)

sendSomething :: (Show a, Member RPC r) => a -> Sem r ()
sendSomething = sendMessage . show

recvSomething :: (Read a, Member RPC r) => Sem r a
recvSomething = fmap read $ recvMessage


-- dispatchLabeler :: (Member RPC r, Member Labeler r) => Sem r ()
-- dispatchLabeler = do
--   recvMessage >>= \case
--     "GetLinkRects" -> getLinkRects >>= sendSomething
--     "SetLabels"    -> do
--       labels <- recvSomething
--       setLabels labels
--     _ -> throw ()

getCtorName :: ConLiftInfo -> String
getCtorName cli = nameBase $ cliConName cli

qSendSomething :: Exp
qSendSomething = VarE 'sendSomething

qSendMessage :: Exp
qSendMessage = VarE 'sendMessage

qRecvSomething :: Exp
qRecvSomething = VarE 'recvSomething

qRecvMessage :: Exp
qRecvMessage = VarE 'recvMessage

qMissingEffect :: Exp
qMissingEffect = ConE 'MissingEffect

qMissingConstructor :: Exp
qMissingConstructor = ConE 'MissingConstructor

qDecodingError :: Exp
qDecodingError = ConE 'DecodingError

mkDispatch :: ConLiftInfo -> [Stmt]
mkDispatch cli = fmap (\n -> BindS (VarP n) qRecvSomething) arg_names
              ++ [NoBindS result]
  where
    arg_names = fmap fst $ cliArgs cli
    call = foldl1' AppE $ VarE (cliFunName cli)
                        : fmap VarE arg_names
    result =
      case cliResType cli of
        TupleT 0 -> call
        _        -> VarE '(>>=) `AppE` call `AppE` VarE 'sendSomething


makeDispatch :: Name -> String -> Q [Dec]
makeDispatch ty n = do
  let name = mkName n
  clis <- snd <$> getEffectMetadata ty
  (++) <$> sigDispatch name (head clis)
       <*> genDispatch name clis


sigDispatch :: Name -> ConLiftInfo -> Q [Dec]
sigDispatch n cli = do
  let r = cliUnionName cli
  pure
      $ pure
      $ SigD n
      $ ForallT [PlainTV r]
          [ makeMemberConstraint r cli
          , makeMemberConstraint' r $ ConT ''RPC
          , makeMemberConstraint' r $ ConT ''Error `AppT` ConT ''RPCError
          ]
      $ makeSemType r
      $ TupleT 0


genDispatch :: Name -> [ConLiftInfo] -> Q [Dec]
genDispatch name clis = do
  ctor <- newName "ctor"
  other <- newName "other"
  let doblock = DoE
        [ BindS (VarP ctor) qRecvMessage
        , NoBindS
          $ CaseE (VarE ctor)
          $ [ Match (LitP $ StringL $ getCtorName cli) (NormalB $ DoE $ mkDispatch cli) []
            | cli <- clis
            ]
            ++
            [ flip (Match (VarP other)) []
            $ NormalB
            $ AppE (VarE 'throw)
            $ qMissingConstructor `AppE` VarE other
            ]
        ]
  pure
    [ FunD name
    $ pure
    $ flip (Clause []) []
    $ NormalB
    $ doblock
    ]




