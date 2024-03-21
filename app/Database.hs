{-# LANGUAGE FlexibleContexts #-}

module Database where

import Core (Core (..), ValueDB, displayProgram)
import Text.Read (readMaybe)

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
