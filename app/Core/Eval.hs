{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval (eval) where

import Control.Algebra (Has)
import Control.Effect.Fail (Fail, fail)
import Control.Effect.State (State, get)
import Core (Core (..), ValueDB)
import qualified Data.Binary
import Data.Map (lookup)

type LocalContext = [Core]

sub :: Core -> Data.Binary.Word16 -> Core -> Core
sub replacement depth t = case t of
  LocalRef index -> if depth == index then replacement else LocalRef index
  Lambda body -> Lambda (sub replacement (depth + 1) body)
  App f x -> App (sub replacement depth f) (sub replacement depth x)
  Record r -> Record $ fmap (sub replacement depth) r
  RecordAccess record field -> RecordAccess (sub replacement depth record) field
  t -> t

reduce :: (Has (State ValueDB) sig m, Has Fail sig m, MonadFail m) => LocalContext -> Core -> m Core
reduce localCtx (LocalRef index) =
  case drop (fromIntegral index) localCtx of
    (v : _) -> pure v
    [] -> fail "Local reference out of bounds"
reduce l (GlobalRef index) = do
  globalCtx <- get
  maybe (fail "Global reference not found") (reduce l) (Data.Map.lookup index globalCtx)
reduce localCtx (Lambda body) = pure (Lambda body)
reduce localCtx (App func arg) = do
  evaluatedFunction <- reduce localCtx func
  case evaluatedFunction of
    (Lambda body) -> reduce (arg : localCtx) (sub arg 0 body)
    t -> App t <$> reduce localCtx arg
reduce localCtx (RecordAccess baseRecord fieldName) = do
  evalResult <- reduce localCtx baseRecord
  case evalResult of
    Record r -> maybe (fail "field missing") (reduce localCtx) (Data.Map.lookup fieldName r)
    _ -> fail "Record access on non-record"
reduce localCtx (Record r) = pure (Record r)
reduce _ Exit = pure Exit
reduce _ GetChar = pure GetChar
reduce _ PutChar = pure PutChar

eval :: (Has (State ValueDB) sig m, Has Fail sig m, MonadFail m) => Core -> m Core
eval = reduce []