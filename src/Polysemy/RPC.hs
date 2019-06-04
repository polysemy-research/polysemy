{-# LANGUAGE TemplateHaskell #-}

module Polysemy.RPC where

import Polysemy
import Polysemy.Error
import Polysemy.Internal.TH.EffectLib
import Language.Haskell.TH.Datatype
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

getEffName :: ConLiftInfo -> String
getEffName cli = nameBase $ cliEffName cli

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

qInterpret :: Exp
qInterpret = VarE 'interpret


genRunRPC :: Name -> [ConLiftInfo] -> Q [Dec]
genRunRPC name clis = do
  let interpreter =
        AppE qInterpret $ LamCaseE $ do
          cli <- clis
          let args = fmap fst $ cliArgs cli
          pure
              . flip (Match (ConP (cliConName cli) $ fmap VarP args)) []
              . NormalB
              . DoE
              $ [ NoBindS $ AppE qSendMessage $ LitE $ StringL $ getEffName cli
                , NoBindS $ AppE qSendMessage $ LitE $ StringL $ getCtorName cli
                ]
                ++
                fmap (NoBindS . AppE qSendSomething . VarE ) args
                ++
                case cliResType cli of
                  TupleT 0 -> []
                  _        -> [NoBindS qRecvSomething]
  pure
    [ FunD name
    $ pure
    $ flip (Clause []) []
    $ NormalB
    $ interpreter
    ]

sigRunRPC :: Name -> ConLiftInfo -> Q [Dec]
sigRunRPC n cli = do
  a <- newName "a"
  let r = cliUnionName cli
  pure
      $ pure
      $ SigD n
      $ quantifyType $ ForallT []
          [ makeMemberConstraint' r $ ConT ''RPC
          ]
      $ makeInterpreterType cli r
      $ VarT a

-- runLabelerOverRPC :: Labeler :r@> a -> '[RPC] >@r@> a
-- runLabelerOverRPC = interpret \case
--   GetLinkRects -> do
--     sendMessage "Labeler"
--     sendMessage "GetLinkRects"
--     recvSomething

--   SetLabels labels -> do
--     sendMessage "Labeler"
--     sendMessage "SetLabels"
--     sendSomething labels




mkDispatch :: ConLiftInfo -> [Stmt]
mkDispatch cli = fmap (\n -> BindS (VarP n) qRecvSomething) arg_names
              ++ [NoBindS result]
  where
    arg_names = fmap fst $ cliArgs cli
    call = makeUnambiguousSend True cli
    result =
      case cliResType cli of
        TupleT 0 -> call
        _        -> VarE '(>>=) `AppE` call `AppE` qSendSomething


makeDispatch :: Name -> String -> String -> Q [Dec]
makeDispatch ty n1 n2 = do
  let name1 = mkName n1
      name2 = mkName n2
  clis <- snd <$> getEffectMetadata ty
  (\a b c d -> a ++ b ++ c ++ d)
       <$> sigDispatch name1 (head clis)
       <*> genDispatch name1 clis
       <*> sigRunRPC name2 (head clis)
       <*> genRunRPC name2 clis


sigDispatch :: Name -> ConLiftInfo -> Q [Dec]
sigDispatch n cli = do
  let r = cliUnionName cli
  pure
      $ pure
      $ SigD n
      $ quantifyType $ ForallT []
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




