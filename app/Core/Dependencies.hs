module Dependencies where

import Control.Algebra (Has)
import Control.Applicative (liftA2)
import Control.Effect.Fail (Fail)
import Control.Effect.State (State, get)
import Control.Monad (foldM)
import Core
import Data.Map (lookup)
import Data.Set (Set, empty, singleton, union)

dependencies :: (Has (State ValueDB) sig m, Has Fail sig m, MonadFail m) => Core -> m (Set Int)
dependencies core = case core of
  GlobalRef n -> get >>= maybe (fail "missing dependency when looking up value") (fmap (union (singleton n)) . dependencies) . Data.Map.lookup n
  Lambda co -> dependencies co
  App fn arg -> liftA2 union (dependencies fn) (dependencies arg)
  Record map -> foldM (\acc -> fmap (union acc) . dependencies) empty map
  RecordAccess base _ -> dependencies base
  _ -> pure empty