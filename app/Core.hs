{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Core where

import qualified Control.Monad as Data.Map
import qualified Data.Binary
import Data.ByteString.Lazy (unpack)
import Data.Char (chr, ord)
import qualified Data.Hashable
import qualified Data.List
import Data.Map.Strict (Map, empty, foldMapWithKey, foldlWithKey, fromList, insert)
import qualified Data.Map.Strict as Data.Map
import qualified GHC.Generics
import Text.Printf (printf)

type ValueDB = Map Int Core -- Map for global references

instance Num Core where
  fromInteger :: Integer -> Core
  fromInteger = LocalRef . fromIntegral
  (+) :: Core -> Core -> Core
  (+) = error "Core datatypes aren't really numbers"
  (*) :: Core -> Core -> Core
  (*) = error "Core datatypes aren't really numbers"
  abs :: Core -> Core
  abs = error "Core datatypes aren't really numbers"
  signum :: Core -> Core
  signum = error "Core datatypes aren't really numbers"
  negate :: Core -> Core
  negate = error "Core datatypes aren't really numbers"

data Core
  = LocalRef Data.Binary.Word16
  | GlobalRef Int
  | Lambda Core
  | App Core Core
  | Record (Map String Core)
  | RecordAccess Core String
  | GetChar
  | PutChar
  | Exit
  deriving (GHC.Generics.Generic, Eq, Show, Read)

(...) :: Core -> Core -> Core
(...) = App

instance Data.Hashable.Hashable Core

instance Data.Binary.Binary Core

displayCore :: Core -> String
displayCore c = case c of
  LocalRef n -> show n
  GlobalRef n -> printf "<%x>" n
  Lambda co -> "\\" ++ displayCore co
  App co co' -> "(" ++ displayCore co ++ " " ++ displayCore co' ++ ")"
  RecordAccess co s -> displayCore co ++ "." ++ s
  GetChar -> "GETCHAR"
  PutChar -> "PUTCHAR"
  Exit -> "EXIT"
  Record r -> "{" ++ Data.List.intercalate "," (Data.Map.foldMapWithKey (\k v -> [k ++ ":" ++ displayCore v]) r) ++ "}"

displayCoreBits :: Core -> String
displayCoreBits = Data.List.intercalate "" . fmap (printf "%08b") . unpack . Data.Binary.encode

displayCoreHex :: Core -> String
displayCoreHex = Data.List.intercalate "" . fmap (printf "%02x") . unpack . Data.Binary.encode

displayProgramG :: (Core -> String) -> ValueDB -> String
displayProgramG f = Data.List.intercalate "\n" . Data.Map.foldMapWithKey (\k v -> [printf "<%x> = %s" k (f v)])

displayProgram :: ValueDB -> String
displayProgram = displayProgramG displayCore

displayProgramBits :: ValueDB -> String
displayProgramBits = displayProgramG displayCoreBits

displayProgramHex :: ValueDB -> String
displayProgramHex = displayProgramG displayCoreHex

class RepresentableInCore a where
  embed :: a -> Core
  extract :: Core -> Maybe a

instance RepresentableInCore Char where
  embed :: Char -> Core
  embed = embed . ord
  extract :: Core -> Maybe Char
  extract = fmap chr . extract

instance RepresentableInCore Int where
  embed :: Int -> Core
  embed n = Lambda (Lambda (foldr App 0 (replicate n 1)))
  extract :: Core -> Maybe Int
  extract (Lambda (Lambda b)) = coreToIntBody b
    where
      coreToIntBody 0 = Just 0
      coreToIntBody (App 1 b) = fmap (+ 1) (coreToIntBody b)
      coreToIntBody _ = Nothing
  extract _ = Nothing

instance RepresentableInCore a => RepresentableInCore [a] where
  embed :: RepresentableInCore a => [a] -> Core
  embed = foldr (\h t -> Lambda (Lambda (1 ... embed h ... (t ... 1 ... 0)))) (Lambda (Lambda 0))
  extract :: RepresentableInCore a => Core -> Maybe [a]
  extract (Lambda (Lambda (App (App 1 h) (App (App t 1) 0)))) = do
    h' <- extract h
    t' <- extract t
    pure (h' : t')
  extract (Lambda (Lambda 0)) = Just []
  extract _ = Nothing

instance RepresentableInCore Core where
  embed :: Core -> Core
  embed = id
  extract :: Core -> Maybe Core
  extract = Just

instance RepresentableInCore a => RepresentableInCore (Map String a) where
  embed :: RepresentableInCore a => Map String a -> Core
  embed = Record . fmap embed
  extract :: RepresentableInCore a => Core -> Maybe (Map String a)
  extract (Record r) = let asMaybes = sequence [do b' <- b; pure (a, b') | (a, b) <- Data.Map.toList (fmap extract r)] in fmap fromList asMaybes
  extract _ = Nothing