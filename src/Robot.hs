module Robot ( readLDM
            , readLCR
            , run
            )where

import Control.Monad.State
import Parsing 
import Data.Bits (Bits(xor))



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
pElement = sat 

whichElement :: Char -> Element
whichElement 'E' = Entry
whichElement '%' = Wall
whichElement '.' = Earth
whichElement '*' = Rock
whichElement '?' = Material 50
whichElement ':' = Material 100
whichElement ';' = Material 150
whichElement '$' = Material 1
whichElement _   = error "Deu ruim!"


type Line = [Element]

data Mine = Mine {
              lines    :: Int,
              columns  :: Int,
              elements :: [Line]
            } deriving (Eq, Ord)

instance Show Mine where
  show = undefined


validMine :: Mine -> Bool
validMine = undefined

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
