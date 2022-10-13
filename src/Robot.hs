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

--Exercício: 1
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

--Exercício: 2
instance Show Element where
    show Empty = " "
    show Entry = "E"
    show Wall = "%"
    show Earth = "."
    show Rock = "*"
    show (Material a)
            | a == 50 = "?"
            | a == 100 = ":"
            | a == 150 = ";"
            | otherwise = "$"

--Exercício: 3
pElement :: Parser Char Element
pElement = wichElement <$> satElement

isElement :: Char -> Bool
isElement a
  | a == ' ' ||
    a == 'E' ||
    a == '%' ||
    a == '.' ||
    a == '*' ||
    a == '?' ||
    a == ':' ||
    a == ';' ||  
    a == '$' = True
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
  | a == ' ' = Empty
  | otherwise = error "Elemento invalido"

type Line = [Element]

data Mine = Mine {
              linhas    :: Int,
              columns  :: Int,
              elements :: [Line]
            } deriving (Eq, Ord)

instance Show Mine where
  show (Mine _ _ []) = [] 
  show (Mine l c e) = unlines $ map (unwords . map show) e

--Exercício: 4
validMine :: Mine -> Bool
validMine m
  | length (elements m) == linhas m &&
  lineChecker (elements m) (columns m) &&
  (leftColumnHasEntry (elements m) ||
  rightColumnHasEntry (elements m) ||
  topLineHasEntry (elements m) ||
  bottomLineHasEntry (elements m)) = True 
  | otherwise = False  

lineChecker :: [Line] -> Int -> Bool 
lineChecker [] _ = True  
lineChecker (x:xs) a
  | length x == a = lineChecker xs a
  | otherwise = False  

lineHasEntry :: Line -> Bool
lineHasEntry [] = False  
lineHasEntry (x:xs)
  | x == Entry = True
  | otherwise = lineHasEntry xs

firstElementIsEntry :: Line -> Bool
firstElementIsEntry [] = False 
firstElementIsEntry (x:xs)
  | x == Entry = True 
  | otherwise = False 

lastElementIsEntry :: Line -> Bool
lastElementIsEntry [] = False
lastElementIsEntry (x:xs)
  | last xs == Entry = True 
  | otherwise = False 

leftColumnHasEntry :: [Line] -> Bool 
leftColumnHasEntry [] = False  
leftColumnHasEntry (x:xs)
  | firstElementIsEntry x = True
  | otherwise = leftColumnHasEntry xs 

rightColumnHasEntry :: [Line] -> Bool 
rightColumnHasEntry [] = False  
rightColumnHasEntry (x:xs)
  | lastElementIsEntry x = True
  | otherwise = rightColumnHasEntry xs 

topLineHasEntry :: [Line] -> Bool
topLineHasEntry [] = False 
topLineHasEntry (x:xs)
  | lineHasEntry x = True  
  | otherwise = False 

bottomLineHasEntry :: [Line] -> Bool
bottomLineHasEntry [] = False 
bottomLineHasEntry (x:xs)
  | lineHasEntry (last xs) = True  
  | otherwise = False 

--Exercício: 6
pLine :: Parser Char Line
pLine = greedy pElement

pMine :: Parser Char Mine
pMine = f <$> endBy pLine (symbol '\n')
  where
    f a = Mine l c e
      where 
        e = filter (/= []) a
        l = length e
        c = length (head e)

data Instr = L -- move para esquerda
          | R -- move para direita
          | U -- move para cima
          | D -- move para baixo
          | C -- coleta material
          | S -- para para recarga.
          deriving (Eq,Ord,Show,Enum)

--Exercício: 8
pInstr :: Parser Char Instr
pInstr = wichIstr <$> satIstr

isIstr :: Char -> Bool
isIstr a
  | a == 'L' ||
    a == 'R' ||
    a == 'U' ||
    a == 'D' ||
    a == 'C' ||
    a == 'S' = True
  |otherwise = False

satIstr :: Parser Char Char
satIstr = sat isIstr

wichIstr :: Char -> Instr
wichIstr a
  | a == 'L' = L
  | a == 'R' = R
  | a == 'U' = U
  | a == 'D' = D
  | a == 'C' = C
  | a == 'S' = S
  | otherwise = error "Instrução inválida"

--Exercício: 9
pProgram :: Parser Char [Instr]
pProgram = greedy pInstr

type Conf = (Robot, Mine)

type ConfM a = State Conf a

current :: ConfM Point
current = do
  (r, m) <- get
  return (position r)

mine :: ConfM Mine
mine = do
  (r, m) <- get
  return m

enoughEnergy :: Int -> ConfM Bool
enoughEnergy n = do
  (r, m) <- get
  return (energy r >= n)

incEnergy :: ConfM ()
incEnergy = do
  (r, m) <- get
  put (r {energy = energy r + 1}, m)

-- valid :: Instr -> ConfM Bool
-- valid L = do (r, m) <- get; return (position r > 1)
-- valid R = do (r, m) <- get; return (position r < columns m)
-- valid U = do (r, m) <- get; return (position r > 1)
-- valid D = do (r, m) <- get; return (position r < linhas m)
-- valid C = do (r, m) <- get; return (elements m !! (position r) !! (position r) == Earth)
-- valid S = do (r, m) <- get; return (energy r < 100)

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


-- -- exercicio 11

-- valid :: Instr -> ConfM Bool
-- valid L = do
--   (r, m) <- get
--   return (position r > 1)

-- valid R = do 
--   (r, m) <- get
--   return (position r < columns m)

-- valid U = do 
--   (r, m) <- get
--   return (position r > 1)

-- valid D = do 
--   (r, m) <- get
--   return (position r < linhas m)

-- valid C = do 
--   (r, m) <- get
--   return (elements m !! (position r) !! (position r) == Earth)

-- valid S = do
--   (r, m) <- get
--   return (energy r < 100)

-- Exercício: 5
exampleMine :: Mine
exampleMine = Mine {
  linhas = 15,
  columns = 15,
  elements = [ 
            [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
            [Wall, Rock, Rock, Rock, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Wall],
            [Wall, Rock, Rock, Rock, Earth, Earth, Earth, Empty, Earth, Earth, Earth, Rock, Earth, Earth, Wall],
            [Wall, Rock, Rock, Rock, Earth, Earth, Earth, Empty, Earth, Earth, Rock, Rock, Rock, Earth, Wall],
            [Wall, Earth, Material 50, Earth, Earth, Earth, Earth, Empty, Earth, Earth, Earth, Rock, Earth, Earth, Wall],
            [Wall, Earth, Earth, Empty, Empty, Empty, Empty, Empty, Earth, Earth, Empty, Earth, Earth, Earth, Wall],
            [Wall, Earth, Earth, Earth, Earth, Empty, Earth, Earth, Earth, Earth, Empty, Earth, Earth, Earth, Wall],
            [Wall, Earth, Material 100, Earth, Earth, Empty, Earth, Earth, Earth, Earth, Empty, Earth, Earth, Earth, Wall],
            [Wall, Earth, Earth, Empty, Earth, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Earth, Earth, Wall],
            [Wall, Earth, Earth, Rock, Earth, Empty, Earth, Earth, Empty, Earth, Earth, Earth, Earth, Earth, Wall],
            [Wall, Earth, Earth, Earth, Earth, Empty, Earth, Earth, Empty, Earth, Material 150, Material 150, Earth, Earth, Wall],
            [Wall, Earth, Rock, Earth, Earth, Empty, Earth, Earth, Earth, Material 150, Material 150, Earth, Earth, Rock, Wall],
            [Wall, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Material 1, Wall],
            [Wall, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Empty, Empty, Empty, Earth, Wall],
            [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Entry, Wall]
            ]
}

sampleRobot :: Robot
sampleRobot = Robot {
                energy = 100,
                position = (1,1),
                collected = 0
}

sLine1 :: Line 
sLine1 = [Wall, Wall, Entry, Wall, Wall, Wall]

sLine2 :: Line 
sLine2 = [Entry, Wall, Wall, Wall, Wall, Wall]

sLine3 :: Line 
sLine3 = [Wall, Wall, Wall, Wall, Wall, Entry]

sLine4 :: Line 
sLine4 = [Wall, Wall, Wall, Wall, Wall, Wall]

sArrayofLines :: [Line]
sArrayofLines = [sLine1, sLine4, sLine4]

sAL1 :: [Line]
sAL1 = [sLine1, sLine4, sLine4, sLine4]

sAL2 :: [Line]
sAL2 = [sLine4, sLine4, sLine4, sLine1]

sMine :: Mine
sMine = Mine{
    linhas = 3,
    columns = 6,
    elements = sArrayofLines
}

sampleConf :: Conf
sampleConf = (sampleRobot, exampleMine)

-- sampleConfM :: ConfM
-- sampleConfM = ?