{-# LANGUAGE TemplateHaskell #-}

module Polysemy.RPC
  ( makeDispatch
  , makeRunRPC
  ) where

import Control.Monad
import Data.List
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

qSerializer :: Type
qSerializer = ConT ''Show

qDeserializer :: Type
qDeserializer = ConT ''Read

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

_qMissingEffect :: Exp
_qMissingEffect = ConE 'MissingEffect

qMissingConstructor :: Exp
qMissingConstructor = ConE 'MissingConstructor

_qDecodingError :: Exp
_qDecodingError = ConE 'DecodingError

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

getAllParamsCtx :: [ConLiftInfo] -> [Pred]
getAllParamsCtx clis =
  join $ fmap (\p -> [qSerializer `AppT` p, qDeserializer `AppT` p]) $ nub $ do
    c <- clis
    cliResType c : fmap snd (cliArgs c)

sigRunRPC :: Name -> [ConLiftInfo] -> Q [Dec]
sigRunRPC n clis = do
  a <- newName "a"
  let r = cliUnionName cli
      cli = head clis
  pure
      $ pure
      $ SigD n
      $ quantifyType $ ForallT []
          ( makeMemberConstraint' r (ConT ''RPC)
          : getAllParamsCtx clis
          )
      $ makeInterpreterType cli r
      $ VarT a




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

makeRunRPC :: Name -> String -> Q [Dec]
makeRunRPC ty n = do
  let name = mkName n
  clis <- snd <$> getEffectMetadata ty
  (++)
       <$> sigRunRPC name clis
       <*> genRunRPC name clis


makeDispatch :: Name -> String -> Q [Dec]
makeDispatch ty n = do
  let name = mkName n
  clis <- snd <$> getEffectMetadata ty
  (++)
       <$> sigDispatch name clis
       <*> genDispatch name clis


sigDispatch :: Name -> [ConLiftInfo] -> Q [Dec]
sigDispatch n clis = do
  let cli = head clis
      r = cliUnionName cli
  pure
      $ pure
      $ SigD n
      $ quantifyType $ ForallT []
          (
          [ makeMemberConstraint r cli
          , makeMemberConstraint' r $ ConT ''RPC
          , makeMemberConstraint' r $ ConT ''Error `AppT` ConT ''RPCError
          ] ++ getAllParamsCtx clis
          )
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




