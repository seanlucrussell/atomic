{-# LANGUAGE FlexibleContexts #-}

module Exec (exec) where

import Control.Algebra (Has)
import Control.Effect.Fail (Fail, fail)
import Control.Effect.State (State)
import Control.Monad.Cont (MonadIO (liftIO))
import Core
  ( Core (App, GetChar, PutChar),
    RepresentableInCore (embed, extract),
    ValueDB,
    displayCore,
  )
import Eval (evalLazy, evalNormalOrder)

exec :: (Has (State ValueDB) sig m, Has Fail sig m, MonadFail m, MonadIO m) => Core -> m Core
exec c = do
  evaluated <- evalLazy c
  case evaluated of
    (App GetChar cont) -> do
      char <- liftIO getChar
      exec (App cont (embed char))
    (App (App PutChar char) cont) -> do
      charNorm <- evalNormalOrder char
      maybe (fail ("Expecting char for put value, got " ++ displayCore charNorm)) pure (extract charNorm) >>= liftIO . putChar
      exec cont
    t -> evalLazy t
