{-# LANGUAGE FlexibleContexts #-}

module Main where

import Compiler (stateCompile)
import Control.Algebra (Has)
import Control.Carrier.Fail.Either (Fail, FailC, runFail)
import Control.Carrier.Lift (LiftC, runM)
import Control.Carrier.State.Strict (State, StateC, get, put, runState)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Core (Core (..), ValueDB, displayCore)
import Data.List (intercalate)
import Data.Map (empty)
import Data.Set (toList)
import Database (displayMainDB, readMainDB, writeMainDB)
import Dependencies (dependencies)
import Eval (evalNormalOrder)
import Exec (exec)
import Options.Applicative
import Parser (parseProgram)

data Command = Compile String | Exec String | DumpDB | Dependencies String

parseCommand :: Parser Command
parseCommand =
  subparser
    ( command "compile" (info compileOptions (progDesc "Compile a file"))
        <> command "exec" (info execOptions (progDesc "Execute a file"))
        <> command "dumpdb" (info dumpDBOptions (progDesc "Dump the database"))
        <> command "dependencies" (info dependenciesOptions (progDesc "List dependencies for a file"))
    )
  where
    compileOptions = Compile <$> argument str (metavar "FILENAME" <> help "File to compile")
    execOptions = Exec <$> argument str (metavar "FILENAME" <> help "File to execute")
    dumpDBOptions = pure DumpDB
    dependenciesOptions = Dependencies <$> argument str (metavar "FILENAME" <> help "File to list dependencies for")

runCommand :: Command -> IO ()
runCommand (Compile filename) = mainHandler (putStrLn . displayCore . snd) (compileFile filename)
runCommand (Exec filename) = mainHandler handleExecutionResult (execFile filename >>= evalNormalOrder)
runCommand DumpDB = displayMainDB
runCommand (Dependencies filename) = mainHandler (putStrLn . intercalate "\n" . fmap (displayCore . GlobalRef) . toList . snd) (compileFile filename >>= dependencies)

handleExecutionResult :: (ValueDB, Core) -> IO ()
handleExecutionResult (db, core) = do
  writeMainDB db
  case core of
    Exit -> pure ()
    _ -> putStrLn (displayCore core)

main :: IO ()
main = execParser opts >>= runCommand
  where
    opts =
      info
        (parseCommand <**> helper)
        ( fullDesc
            <> progDesc "A program with different command-line options"
            <> header "programName - a program with command-line options"
        )

parseAndCompile :: (Has (State ValueDB) sig m, Has Fail sig m, MonadFail m, MonadIO m) => FilePath -> String -> m Int
parseAndCompile path sourceText = parseProgram path sourceText >>= stateCompile Data.Map.empty

execFile :: (Has (State ValueDB) sig m, Has Fail sig m, MonadFail m, MonadIO m) => FilePath -> m Core
execFile path = compileFile path >>= exec

compileFile :: (Has (State ValueDB) sig m, Has Fail sig m, MonadFail m, MonadIO m) => FilePath -> m Core
compileFile path = do
  sourceText <- liftIO (readFile path)
  moduleID <- parseAndCompile path sourceText
  db <- get
  liftIO $ writeMainDB db
  pure (GlobalRef moduleID)

withMainDB :: (Has (State ValueDB) sig m, MonadFail m, MonadIO m) => m ()
withMainDB = liftIO readMainDB >>= maybe (fail "Failed to read main value database") put

runner :: StateC ValueDB (FailC (LiftC m)) a -> m (Either String (ValueDB, a))
runner = runM . runFail . runState Data.Map.empty

mainHandler :: ((ValueDB, a) -> IO ()) -> StateC ValueDB (FailC (LiftC IO)) a -> IO ()
mainHandler onSuccess system = runner (withMainDB >> system) >>= either putStrLn onSuccess