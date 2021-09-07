module MyLib where

data Pos = A | B | C | D | E | F | G | H deriving (Eq, Ord, Bounded, Enum, Show)

data File = File Pos deriving (Eq, Ord)
data Rank = Rank Pos deriving (Eq, Ord)

toPath :: Pos -> Pos -> [Pos]
toPath src dest
  | src == dest = [src]
  | src < dest = src : toPath (succ src) dest
  | src > dest = src : toPath (pred src) dest

data Coord = Coord { file :: File, 
                     rank:: Rank } deriving (Eq, Ord)

data Vision = Vision { origin :: Coord,
                       eyes :: Coord,
                       blocks :: [Coord]
                     } deriving (Eq, Show)


-- b7 e7 [b7 c7 d7 e7]
-- e7 b7 [e7 d7 c7 b7]

-- 7 7 [7]
-- 7 3 [7, 6, 5, 4, 3]
-- 3 5 [3, 4, 5]
-- 

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

data ColorMap a = ColorMap {
  white:: a,
    black:: a
  } deriving (Eq)

instance Functor ColorMap where
  fmap fn (ColorMap white black) = 
    ColorMap (fn white) (fn black)

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
b5 = Coord (File B) (Rank E)
c4 = Coord (File C) (Rank D)

blackKingb5 = Piese blackKing b5
whiteKinga4 = Piese whiteKing a4
whiteRookc4 = Piese whiteRook c4

isKing = (== King)

board = [blackKingb5,
         whiteKinga4,
          whiteRookc4]

visions:: Piese -> [Vision]
visions p = []

-- [Piese]
-- Piese -> [Vision]
-- Vision -> Vision -> Bool

-- White Rook is checking Black King


mapso :: [Piese] -> ColorMap (RoleMap [Coord])
mapso lst = fmap (fmap (fmap coord))
  (fmap (roleMapFromList (role.piece)) (colorMapFromList (color.piece) lst))
  


-- [a] -> ColorMap RoleMap [a]



-- colorMapFromList :: (a -> Color) -> [a] -> ColorMap [a]
-- roleMapFromList :: (a -> Role) -> [a] -> RoleMap [a]

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
