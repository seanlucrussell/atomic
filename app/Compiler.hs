{-# LANGUAGE TupleSections #-}

module Compiler where

import Control.Applicative ((<|>))
import Control.Effect.Fail (Fail, fail)
import Control.Effect.State (Has, State, modify)
import Control.Monad.Cont (MonadIO (liftIO))
import Core (Core (..), RepresentableInCore (embed, extract), ValueDB, displayCore)
import Data.Coerce (coerce)
import qualified Data.Functor
import qualified Data.Hashable
import Data.List (elemIndex)
import qualified Data.Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Eval (eval)
import Exec (exec)
import qualified GHC.Generics
import qualified Parser as P

type GlobalNames = Map P.Name Core

type LocalNames = [P.Name]

compileTerm ::
  ( Has (State ValueDB) sig m,
    Has Fail sig m,
    MonadIO m,
    MonadFail m
  ) =>
  GlobalNames ->
  LocalNames ->
  P.Term ->
  m Core
compileTerm g e t = case t of
  P.App a b -> App <$> comp e a <*> comp e b
  P.Lambda n t -> Lambda <$> comp (n : e) t
  P.Ref n -> refLookup g e n
  P.MapLiteral m ->
    Record . Data.Map.fromList <$> mapM (\(P.MapEntry (P.Name name) value) -> comp e value Data.Functor.<&> (name,)) m
  P.MapAccess a (P.Name b) -> RecordAccess <$> comp e a <*> pure b
  P.GetChar -> pure GetChar
  P.PutChar -> pure PutChar
  P.GlobalRef n -> pure (GlobalRef n)
  P.Exit -> pure Exit
  P.StringLiteral s -> pure (embed s)
  P.ChrLiteral c -> pure (embed c)
  P.NatLiteral n -> pure (embed n)
  P.Import path -> do
    compiledPath <- comp e path
    evaluatedPath <- eval compiledPath
    case extract evaluatedPath of
      Nothing -> fail ("expecting file path for import, instead got " ++ show path)
      Just s -> GlobalRef <$> compileFile s
  where
    comp = compileTerm g

refLookup ::
  ( Has (State ValueDB) sig m,
    Has Fail sig m,
    MonadIO m,
    MonadFail m
  ) =>
  GlobalNames ->
  LocalNames ->
  P.Name ->
  m Core
refLookup g e n =
  fromMaybe
    (fail ("ref " ++ coerce n ++ " undefined"))
    (pure . LocalRef . fromIntegral <$> elemIndex n (coerce e) <|> pure <$> Data.Map.lookup n g)

addValueToDB ::
  Has (State ValueDB) sig m =>
  Core ->
  m Int
addValueToDB c = do
  let hashVal = Data.Hashable.hash c
  modify (Data.Map.insert hashVal c)
  pure hashVal

stateCompile ::
  ( Has (State ValueDB) sig m,
    Has Fail sig m,
    MonadIO m,
    MonadFail m
  ) =>
  GlobalNames ->
  P.Instruction ->
  m Int
stateCompile g (P.Assign name currentLine remainingLines) = do
  hashedVal <- case currentLine of
    P.GlobalRef n -> pure n
    _ -> compileTerm g [] currentLine >>= addValueToDB
  stateCompile (Data.Map.insert name (GlobalRef hashedVal) g) remainingLines
stateCompile g (P.UseFrom names term remainingInstructions) = do
  compiledTerm <- compileTerm g [] term
  evaluatedTerm <- eval compiledTerm
  case extract evaluatedTerm :: Maybe (Map String Core) of
    Nothing -> fail ("expecting record in use-from, instead got " ++ displayCore evaluatedTerm)
    Just a ->
      case names of
        [P.Name "everything"] ->
          stateCompile
            (Data.Map.union (Data.Map.fromList [(P.Name n, v) | (n, v) <- Data.Map.toList a]) g)
            remainingInstructions
        _ ->
          let nameValues = [(name, Data.Map.lookup name a) | (P.Name name) <- names]
              missingNames = [name | (name, Nothing) <- nameValues]
           in if null missingNames
                then
                  stateCompile
                    (Data.Map.union (Data.Map.fromList [(P.Name n, v) | (n, Just v) <- nameValues]) g)
                    remainingInstructions
                else fail ("names " ++ show missingNames ++ " not defined in " ++ displayCore evaluatedTerm)
stateCompile g (P.Reveal term) = compileTerm g [] term >>= addValueToDB

parseAndCompile ::
  ( Has (State ValueDB) sig m,
    Has Fail sig m,
    MonadFail m,
    MonadIO m
  ) =>
  FilePath ->
  String ->
  m Int
parseAndCompile path sourceText = P.parseProgram path sourceText >>= stateCompile Data.Map.empty

compileFile ::
  ( Has (State ValueDB) sig m,
    Has Fail sig m,
    MonadFail m,
    MonadIO m
  ) =>
  FilePath ->
  m Int
compileFile path = liftIO (readFile path) >>= parseAndCompile path
