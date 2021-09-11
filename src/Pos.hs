module Pos where

import Debug.Trace
import Control.Applicative
import Data.Maybe

data Pos = A | B | C | D | E | F | G | H deriving (Eq, Ord, Bounded, Enum, Show)

mid :: Pos -> Maybe Pos
mid s = Just s

msucc :: Pos -> Maybe Pos
msucc H = Nothing
msucc s = Just $ succ s

mpred :: Pos -> Maybe Pos
mpred A = Nothing
mpred s = Just $ pred s

msucc2 :: Pos -> Maybe Pos
msucc2 a = do 
  a2 <- msucc a
  msucc a2

mpred2 :: Pos -> Maybe Pos
mpred2 a = do
  a2 <- mpred a
  mpred a2



type Dir = Pos -> Maybe Pos
type Vec = (Dir, Dir)
type Vecs = [Vec]

vrook::Vecs
vrook = [(mid, msucc),
          (mid, mpred),
          (msucc, mid),
          (mpred, mid)]

vbishop::Vecs
vbishop = [(msucc, msucc),
            (msucc, mpred),
            (mpred, msucc),
            (mpred, mpred)]

vqueen::Vecs
vqueen = vrook ++ vbishop

vknight::Vecs
vknight = [(msucc2, mpred),
          (msucc2, msucc),
          (mpred2, mpred),
          (mpred2, msucc),
          (msucc, mpred2),
          (msucc, msucc2),
          (mpred, mpred2),
          (mpred, msucc2)]

vapply:: Vec -> Coord -> Maybe Coord
vapply (vf, vr) 
  (Coord (File file) (Rank rank)) = do
  file2 <- vf(file)
  rank2 <- vr(rank)
  return (Coord (File file2) (Rank rank2))

vproject :: Vec -> Coord -> [Coord]
vproject v coord = coord : rest
  where rest = case vapply v coord of
          Nothing -> []
          Just c -> vproject v c

-- [a3, a4, a5]
-- [a3, a4]
-- [a3, a4, a5]
vpower :: [Coord] -> [[Coord]]
vpower coords = foldl _power [] coords

_power :: [[Coord]] -> Coord -> [[Coord]]
_power [] coord = [[coord]]
_power (x:rest) coord = (x ++ [coord]) : x : rest



csvision :: [Coord] -> Maybe Vision
csvision [] = Nothing
csvision [_] = Nothing
csvision coords = 
  Just $ Vision
  (head coords)
  (last coords) (tail $ init coords)


vecs :: Role -> Vecs
vecs King = vqueen
vecs Queen = vqueen
vecs Rook = vrook
  
visions :: Piese -> [Vision]
visions p = 
  catMaybes $
  map csvision $
  (map ($ coord p) ls) >>= vpower
  where ls =
          (map vproject (vecs . role . piece $ p))


data File = File Pos deriving (Eq, Ord)
data Rank = Rank Pos deriving (Eq, Ord)


data Coord = Coord { file :: File, 
                     rank:: Rank } deriving (Eq, Ord)

data Vision = Vision { origin :: Coord,
                       eyes :: Coord,
                       blocks :: [Coord]
                     } deriving (Eq)


directVision :: Vision -> Bool
directVision vSrc =
  null $ blocks vSrc
  

captureVision :: Vision -> Vision -> Bool
captureVision vSrc vDest = 
  eyes vSrc == origin vDest

interposeVision :: Vision -> Vision -> Bool
interposeVision vSrc vDest = 
  eyes vSrc `elem` (blocks vDest)


fleeVision :: Vision -> Vision -> Bool
fleeVision vSrc vDest =
  eyes vDest /= eyes vSrc



data Color = White | Black deriving (Eq, Show)
data Role = King | Queen | Rook deriving (Eq, Show)

data Piece = Piece { color:: Color, 
                     role:: Role } deriving (Eq)

data Piese = Piese { piece:: Piece,
                     coord:: Coord } deriving (Eq)

opposite :: Color -> Color
opposite White = Black
opposite Black = White

projection :: Role -> Bool
projection King = False
projection _ = True


data ColorMap a = ColorMap {
  white:: a,
    black:: a
  } deriving (Eq)

instance Functor ColorMap where
  fmap fn (ColorMap white black) = 
    ColorMap (fn white) (fn black)

instance Applicative ColorMap where
  pure a = ColorMap a a
  ColorMap wfs bfs <*> ColorMap w b = ColorMap (wfs w) (bfs b)


colorMapFromList :: (a -> Color) -> [a] -> ColorMap [a]
colorMapFromList fn lst = ColorMap 
                  (filter ((== White) . fn) lst)
                  (filter ((== Black) . fn) lst)
                  
data RoleMap a = RoleMap {
  king:: a,
    queen:: a,
    rook:: a }

instance Functor RoleMap where
  fmap fn (RoleMap king queen rook) = 
    RoleMap (fn king) (fn queen) (fn rook)


instance Applicative RoleMap where
  pure a = RoleMap a a a
  RoleMap fk fq fr <*> RoleMap ak aq ar = 
    RoleMap (fk ak) (fq aq) (fr ar)

roleMapFromList :: (a -> Role) -> [a] -> RoleMap [a]
roleMapFromList fn lst = RoleMap
  (filter ((== King) . fn) lst)
  (filter ((== Queen) . fn) lst)
  (filter ((== Rook) . fn) lst)


data EscapeCheck = Capture Vision |
                   Interpose Vision |
                   Flee Vision

data Check = Check Vision

whiteKing = Piece White King
blackKing = Piece Black King
whiteRook = Piece White Rook
a4 = Coord (File A) (Rank D)
a5 = Coord (File A) (Rank E)
a6 = Coord (File A) (Rank F)
b5 = Coord (File B) (Rank E)
c4 = Coord (File C) (Rank D)

blackKingb5 = Piese blackKing b5
whiteKinga4 = Piese whiteKing a4
whiteRookc4 = Piese whiteRook c4

isKing = (== King)

board = [blackKingb5,
         whiteKinga4]

-- [Piese]
-- Piese -> [Vision]
-- Vision -> Vision -> Bool

-- White Rook is checking Black King

data CoroMap a = CoroMap (ColorMap (RoleMap a))


instance Functor CoroMap where
  fmap fn (CoroMap coro) = 
    CoroMap $ fmap (fmap fn) coro

instance Applicative CoroMap where
  pure a = CoroMap (pure (pure a))
  CoroMap fgf <*> CoroMap fga = 
    CoroMap ((<*>) <$> fgf <*> fga)

flipColor :: CoroMap a -> CoroMap a
flipColor (CoroMap (ColorMap w b)) = 
  CoroMap $ ColorMap b w


mapso :: [Piese] -> CoroMap [Piese]
mapso lst = 
  CoroMap $ fmap (roleMapFromList (role.piece))
  (colorMapFromList (color.piece) lst)

-- f [a] -> (a -> [b]) -> f [b]
-- CoroMap [Piese] -> CoroMap [Vision]
-- (fmap . concatMap) visions

-- CoroMap [Vision] -> CoroMap Check

checkVision :: Vision -> Vision -> Maybe Check
--checkVision vSrc vDest | trace (show vSrc ++ " " ++ show vDest) False = undefined
checkVision vSrc vDest
  | captureVision vSrc vDest && 
    directVision vSrc = Just (Check vSrc)
  | otherwise = Nothing


-- CoroMap [Vision]

-- CoroMap [Vision] -> CoroMap [Vision -> Maybe Check]
                                

boardVis :: CoroMap [Vision]
boardVis = (fmap . concatMap) visions $ mapso board

boardVisCheck :: CoroMap [Vision -> Maybe Check] 
boardVisCheck = flipColor $ fmap (map checkVision) boardVis

-- (Applicative f) => f [a] -> f [a -> b] -> f b

bChecks :: CoroMap [Check]
bChecks  = 
  fmap catMaybes $ 
  (liftA2 (zipWith ($)) boardVisCheck) $ 
  boardVis







instance Show File where
  show (File A)  = "a"
  show (File B)  = "b"
  show (File C)  = "c"
  show (File D)  = "d"
  show (File E)  = "e"
  show (File F)  = "f"
  show (File G)  = "g"
  show (File H)  = "h"
  
instance Show Rank where
  show (Rank A)  = "1"
  show (Rank B)  = "2"
  show (Rank C)  = "3"
  show (Rank D)  = "4"
  show (Rank E)  = "5"
  show (Rank F)  = "6"
  show (Rank G)  = "7"
  show (Rank H)  = "8"

instance Show Coord where
  show (Coord f r) = show f ++ show r

instance Show Piece where
  show (Piece White King) = "K"
  show (Piece White Queen) = "Q"
  show (Piece White Rook) = "R"
  show (Piece Black King) = "k"
  show (Piece Black Queen) = "q"
  show (Piece Black Rook) = "r"

instance Show Piese where
  show (Piese p c) = show p ++ "@" ++ show c


instance (Show a) => Show (ColorMap a) where
  show (ColorMap white black) = "<" ++ show white ++ "/\\" ++ show black ++ ">"

instance (Show a) => Show (RoleMap a) where
  show (RoleMap king queen rook) = 
    "< k:" ++ show king ++ 
    " q: " ++ show queen ++ 
    " r: " ++ show rook ++
    " >"

instance (Show a) => Show (CoroMap a) where
  show (CoroMap comap) = show comap


instance Show Check where
  show (Check vision) = show vision ++ "+"

instance Show Vision where
  show (Vision origin eyes blocks) = show origin ++ "->" ++ show eyes
