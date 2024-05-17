{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval (evalLazy, evalNormalOrder) where

import Control.Algebra (Has)
import Control.Effect.Fail (Fail, fail)
import Control.Effect.State (State, get)
import Core (Core (..), ValueDB, displayCore)
import qualified Data.Binary
import Data.Map (lookup)

type LocalContext = [Core]

evalLazy :: (Has (State ValueDB) sig m, Has Fail sig m, MonadFail m) => Core -> m Core
evalLazy (GlobalRef index) = get >>= (maybe (fail "Global reference not found") evalLazy . Data.Map.lookup index)
evalLazy (App func arg) = do
  evaluatedFunction <- evalLazy func
  case evaluatedFunction of
    (Lambda body) -> evalLazy (open body arg)
    t -> App t <$> evalLazy arg
evalLazy (RecordAccess baseRecord fieldName) = do
  Record r <- evalNormalOrder baseRecord
  maybe (fail ("field " ++ fieldName ++ " missing from " ++ displayCore baseRecord)) evalLazy (Data.Map.lookup fieldName r)
evalLazy t = pure t

applyToFree :: (Data.Binary.Word16 -> Data.Binary.Word16) -> Core -> Core
applyToFree f = go 0
  where
    go depth (LocalRef i) = LocalRef (if i < depth then i else f i)
    go depth (Lambda t) = Lambda (go (depth + 1) t)
    go depth (App t1 t2) = App (go depth t1) (go depth t2)
    go depth (Record r) = Record (fmap (go depth) r)
    go depth (RecordAccess record field) = RecordAccess (go depth record) field
    go _ t = t

open :: Core -> Core -> Core
open a b = go 0 a
  where
    go d (LocalRef i)
      | i < d = LocalRef i -- bound variable, do not touch
      | i == d = applyToFree (+ d) b -- we are substituting this variable
      | i > d = LocalRef (i - 1) -- decrementing free vars since we are opening the lambda here
    go d (Lambda t) = Lambda (go (d + 1) t)
    go d (App t1 t2) = App (go d t1) (go d t2)
    go depth (Record r) = Record (fmap (go depth) r)
    go depth (RecordAccess record field) = RecordAccess (go depth record) field
    go _ t = t

normalOrderStep :: (Has (State ValueDB) sig m, Has Fail sig m, MonadFail m) => Core -> m (Maybe Core)
normalOrderStep (GlobalRef index) = get >>= (maybe (fail "Global reference not found") (pure . Just) . Data.Map.lookup index)
normalOrderStep (App (Lambda body) arg) = pure (Just (open body arg))
normalOrderStep (App func arg) = do
  evaluatedFunction <- normalOrderStep func
  case evaluatedFunction of
    Just newFunction -> pure (Just (App newFunction arg))
    Nothing -> fmap (App func) <$> normalOrderStep arg
normalOrderStep (RecordAccess baseRecord fieldName) = do
  (Record r) <- evalNormalOrder baseRecord
  maybe (fail ("field " ++ fieldName ++ " missing from " ++ displayCore baseRecord)) (pure . Just) (Data.Map.lookup fieldName r)
normalOrderStep (Lambda body) = fmap (fmap Lambda) (normalOrderStep body)
normalOrderStep t = pure Nothing

evalNormalOrder :: (Has (State ValueDB) sig m, Has Fail sig m, MonadFail m) => Core -> m Core
evalNormalOrder term = normalOrderStep term >>= maybe (pure term) evalNormalOrder
