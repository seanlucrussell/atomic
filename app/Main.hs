{-# LANGUAGE FlexibleContexts #-}

module Main where

import Compiler (stateCompile)
import Control.Algebra (Has)
import Control.Carrier.Fail.Either (Fail, FailC, runFail)
import Control.Carrier.Lift (LiftC, runM)
import Control.Carrier.State.Strict (State, StateC, get, put, runState)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Core (Core (..), ValueDB, displayCore, displayProgram)
import Data.Map (empty)
import Exec (exec)
import Options.Applicative
import Parser (parseProgram)
import Text.Read (readMaybe)

data Command = Compile String | Exec String | DumpDB

parseCommand :: Parser Command
parseCommand =
  subparser
    ( command "compile" (info compileOptions (progDesc "Compile a file"))
        <> command "exec" (info execOptions (progDesc "Execute a file"))
        <> command "dumpdb" (info dumpDBOptions (progDesc "Dump the database"))
    )
  where
    compileOptions = Compile <$> argument str (metavar "FILENAME" <> help "File to compile")
    execOptions = Exec <$> argument str (metavar "FILENAME" <> help "File to execute")
    dumpDBOptions = pure DumpDB

runCommand :: Command -> IO ()
runCommand (Compile filename) = mainHandler (putStrLn . displayCore . snd) (compileFile filename)
runCommand (Exec filename) = mainHandler (writeMainDB . fst) (execFile filename)
runCommand DumpDB = displayMainDB

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

-- other useful operations

writeToFile :: Show a => FilePath -> a -> IO ()
writeToFile path = writeFile path . show

readFromFile :: Read a => FilePath -> IO (Maybe a)
readFromFile path = readMaybe <$> readFile path

mainDBLocation :: FilePath
mainDBLocation = "artifacts"

readMainDB :: IO (Maybe ValueDB)
readMainDB = readFromFile mainDBLocation

writeMainDB :: ValueDB -> IO ()
writeMainDB = writeToFile mainDBLocation

displayMainDB :: IO ()
displayMainDB = readMainDB >>= putStrLn . maybe (error "no db") displayProgram

-- it would be nifty to display a dependency graph for our terms. that could be a killer feature here

-- one entry - we just eval a term
-- could be e.g. `atomic '#IMPORT "path/local-file.a"'

-- we gonna want to have multiple different value DB sources. need to spec a simple communication protcol.
-- thinking we could have a local caching server, remote servers (perhaps several), and other systems

-- a major challenge is the way we pair documentation to the ValueDB. how
-- are people supposed to discover relevant information? something we need to think about

-- also, next steps require us to be able to push and pull from a db
-- for now lets suppose that a db is append only. don't worry about space. all you can do is push

-- obvious important thing to do is find the transitive closure of dependencies for a core expression