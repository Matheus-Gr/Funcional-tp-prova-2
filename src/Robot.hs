module Robot ( readLDM
            , readLCR
            , run
            )where

import Control.Monad.State
import Parsing 
import Data.Bits (Bits(xor))
import Data.Char (isDigit)
import Data.Type.Coercion (sym)
import Control.Applicative (Alternative(empty))
import Data.Semigroup (Min(Min))



type Fuel = Int
type Point = (Int,Int)
type Material = Int

data Robot = Robot {
                energy    :: Fuel,
                position  :: Point,
                collected :: Material
              } deriving (Eq, Ord)

sampleRobot :: Robot
sampleRobot = Robot {
                energy = 100,
                position = (1,1),
                collected = 0
              }

instance Show Robot where
    show (Robot energy position collected) = 
        "Energy:" ++ show energy ++ "\n" ++
        "Position" ++ show position ++ "\n" ++ 
        "Collected:" ++ show collected

data Element = Empty         -- espaço vazio
            | Entry         -- entrada da mina
            | Wall          -- parede
            | Earth         -- terra
            | Rock          -- rocha
            | Material Int  -- material, Int indica quantidade.
            deriving (Eq,Ord)

instance Show Element where
    show Empty = ""
    show Entry = "E"
    show Wall = "%"
    show Earth = "."
    show Rock = "*"
    show (Material a)
            | a == 50 = "?"
            | a == 100 = ":"
            | a == 150 = ";"
            | otherwise = "$"


pElement :: Parser Char Element
pElement = wichElement <$> satElement

isElement :: Char -> Bool
isElement a
  | a == ' ' || a == 'E' || a == '%' || a == '.' || a == '*' || a == '?' ||  a == ':' ||  a == ';' ||  a == '$' = True
  |otherwise = False

satElement :: Parser Char Char
satElement = sat isElement

wichElement :: Char -> Element
wichElement a
  | a == 'E' = Entry
  | a == '%' = Wall
  | a == '.' = Earth
  | a == '*' = Rock
  | a == '?' = Material 50
  | a == ':' = Material 100
  | a == ';' = Material 150
  | a == '$' = Material 1
  | otherwise = Empty

type Line = [Element]

data Mine = Mine {
              linhas    :: Int,
              columns  :: Int,
              elements :: [Line]
            } deriving (Eq, Ord)

instance Show Mine where
  show = undefined

sampleLine :: Line 
sampleLine = [Wall, Wall, Entry, Wall, Wall, Wall]

sampleArrayofLines :: [Line]
sampleArrayofLines = [sampleLine, sampleLine, sampleLine]

sampleMine :: Mine
sampleMine = Mine{
              linhas = 3,
              columns = 6,
              elements = sampleArrayofLines
}

validMine :: Mine -> Bool
validMine m
  | length (elements m) == linhas m && lineChecker (elements m) (columns m) = True 
  | otherwise = False  

lineChecker :: [Line] -> Int -> Bool 
lineChecker [] _ = True  
lineChecker (x:xs) a
  | length x == a = lineChecker xs a
  | otherwise = False  

pLine :: Parser Char Line
pLine = undefined

pMine :: Parser Char Mine
pMine = undefined

data Instr = L -- move para esquerda
          | R -- move para direita
          | U -- move para cima
          | D -- move para baixo
          | C -- coleta material
          | S -- para para recarga.
          deriving (Eq,Ord,Show,Enum)

pInstr :: Parser Char Instr
pInstr = undefined

pProgram :: Parser Char [Instr]
pProgram = undefined

type Conf = (Robot, Mine)

type ConfM a = State Conf a


current :: ConfM Point
current = undefined

mine :: ConfM Mine
mine = undefined

enoughEnergy :: Int -> ConfM Bool
enoughEnergy = undefined

incEnergy :: ConfM ()
incEnergy = undefined

valid :: Instr -> ConfM Bool
valid = undefined


updateMine :: Instr -> ConfM ()
updateMine = undefined

exec :: Instr -> ConfM ()
exec = undefined

initRobot :: Mine -> Robot
initRobot = undefined

run :: [Instr] -> Mine -> Mine
run = undefined

readLDM :: String -> IO (Either String Mine)
readLDM = undefined

readLCR :: String -> IO (Either String [Instr])
readLCR = undefined


-- Language: haskell

exampleMine :: Mine
exampleMine = Mine {
  linhas = 3,
  columns = 6,
  elements = [ [Wall, Wall, Entry, Wall, Wall, Wall]
             , [Wall, Earth, Earth, Earth, Earth, Wall]
             , [Wall, Wall, Wall, Wall, Wall, Wall]
             ]
}
