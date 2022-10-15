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
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import System.Posix.Internals (fileType)
import Data.String (IsString)
import Control.Monad.Except (ExceptT(ExceptT))
import Control.Exception

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

--Exercício: 7
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

--Exercício: 10 
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

--Exercício: 11
valid :: Instr -> ConfM Bool
valid L = do
  m <- mine
  (x,y) <- current;
  e <- elementIs (x-1, y)
  z <- enoughEnergy (energyCost e)
  return ((x > 1) && z);
valid R = do
  (x,y) <- current;
  m <- mine
  e <- elementIs (x+1, y)
  z <- enoughEnergy (energyCost e)
  return ((x > 1) && z);
valid U = do
  (x,y) <- current;
  m <- mine
  e <- elementIs (x,y+1)
  z <- enoughEnergy (energyCost e)
  return ((x > 1) && z);
valid D = do
  (x,y) <- current;
  m <- mine
  e <- elementIs (x-1, y)
  z <- enoughEnergy (energyCost e)
  return ((x > 1) && z);
valid C = do 
  p <- current
  m <- mine
  z <- enoughEnergy 10
  return (z && minerable m p)
valid S = return True

minerable :: Mine -> Point -> Bool
minerable (Mine _ _ []) _ = error "Mina vazia"
minerable (Mine _ _ e) (x,y) = isMaterial e (x-1,y) || isMaterial e (x+1,y) || isMaterial e (x,y-1) || isMaterial e (x,y+1)

isMaterial :: [Line] -> Point -> Bool 
isMaterial e (x,y)
  | e !! x !! y == Material 50 ||
    e !! x !! y == Material 100 || 
    e !! x !! y == Material 150 || 
    e !! x !! y == Material 1 = True 
  | otherwise = False

elementIs :: Point -> ConfM Element
elementIs (x,y) = do
  m <- mine
  return ((elements m !! x) !! y)

energyCost :: Element -> Int
energyCost e 
  | e == Rock = 30
  | e == Earth = 5
  | otherwise = 1

--Exercício: 12
updateMine :: Instr -> ConfM ()
updateMine = undefined

--Exercício: 13
exec :: Instr -> ConfM ()
exec = undefined

--Exercício: 14
initRobot :: Mine -> Robot
initRobot m = Robot {
  energy = 100,
  position = findEntry (elements m) 0,
  collected = 0
}

findEntry  :: [Line] -> Int -> Point
findEntry [] acc = error "Empty mine"
findEntry (x:xs) acc
  | lineHasEntry x = (acc, elemIndex' Entry x)
  | otherwise = findEntry xs ( acc + 1 )
  
elemIndex' :: Eq a => a -> [a] -> Int
elemIndex' x = fromMaybe (-1) . elemIndex x

--Exercício: 15
run :: [Instr] -> Mine -> Mine
run = undefined

--Exercício: 16
readLDM :: String -> IO (Either String Mine)
readLDM = undefined 
-- readLDM file
--   | strOrExc <- try $ readFile file
--       Left error "nao foi possivel abrir arquivo"
--       Right (map fst (runParser pMine strOrExc))

--Exercício: 17
readLCR :: String -> IO (Either String [Instr])
readLCR = undefined

--Exercício: 5
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

-- VARIAVEIS DE TESTE
-- sampleRobot :: Robot
-- sampleRobot = Robot {
--                 energy = 100,
--                 position = (1,1),
--                 collected = 0
-- }

-- sLine1 :: Line 
-- sLine1 = [Wall, Wall, Entry, Wall, Wall, Wall]

-- sLine2 :: Line 
-- sLine2 = [Entry, Wall, Wall, Wall, Wall, Wall]

-- sLine3 :: Line 
-- sLine3 = [Wall, Wall, Wall, Wall, Wall, Entry]

-- sLine4 :: Line 
-- sLine4 = [Wall, Wall, Wall, Wall, Wall, Wall]

-- sArrayofLines :: [Line]
-- sArrayofLines = [sLine1, sLine4, sLine4]

-- sAL1 :: [Line]
-- sAL1 = [sLine1, sLine4, sLine4, sLine4]

-- sAL2 :: [Line]
-- sAL2 = [sLine4, sLine4, sLine4, sLine1]

-- sMine :: Mine
-- sMine = Mine{
--     linhas = 3,
--     columns = 6,
--     elements = sArrayofLines
-- }

-- sampleConf1 :: Conf
-- sampleConf1 = (sampleRobot, exampleMine)

-- sampleConf2 :: Conf
-- sampleConf2 =  (Robot 0 (0,0) 0, Mine 0 0 [])