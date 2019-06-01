-- | Library to solve the tile puzzle.

{-# language TemplateHaskell, TupleSections #-}

module Puzzle where

import           Data.Array                     ( Array
                                                , Ix
                                                )
import qualified Data.Array                    as A
import           Control.Lens
import           Control.Monad.State
import           Data.Maybe
import           Data.Tuple
import           Control.Arrow
import           Data.List

-- | A class for complementary data types, such as the edges of the
-- puzzle.
class Eq a => Complementary a where
  -- | Return the complement of the argument.
  complement :: a -> a

  -- | Check if the two arguments are complements of each other.
  isComplement :: a -> a -> Bool
  isComplement = (==) . complement

-- | Data type for the edges of the tiles.
data TileEdge = PinkFront | PinkBack
  | BlueFront | BlueBack
  | GreenFront | GreenBack
  | OrangeFront | OrangeBack
  deriving (Eq, Show)

-- | Data type describing tiles by their edges.
data Tile = Tile
  { _tileLeft :: TileEdge
  , _tileRight :: TileEdge
  , _tileTop :: TileEdge
  , _tileBottom :: TileEdge
  } deriving (Eq, Show)

makeLenses ''Tile

data Direction = West | East | North | South deriving (Eq, Show)

instance Complementary TileEdge where
        complement PinkFront   = PinkBack
        complement PinkBack    = PinkFront
        complement BlueFront   = BlueBack
        complement BlueBack    = BlueFront
        complement GreenFront  = GreenBack
        complement GreenBack   = GreenFront
        complement OrangeFront = OrangeBack
        complement OrangeBack  = OrangeFront

-- | Solve the puzzle. 'r' and 'c' are the amount rows and columns in
-- the puzzle, 'tiles' are the available tiles, and 'moves' is the index
-- is the sequential placement strategy along with which directions to
-- check.
-- Since a good placement strategy places tiles from left to right and
-- top to bottom, often only the North and West directions need to be
-- checked when placing a new tile down, which is why it is an
-- argument rather than allowing the function to test all four edges
-- when only one could suffice.
solvePuzzle
        :: Int
        -> Int
        -> [Tile]
        -> [((Int, Int), [Direction])]
        -> [Array (Int, Int) Tile]
solvePuzzle r c tiles moves =
        nubBy boardCongruent . catMaybes $ sequence . fst <$> foldr
                puzzleIteration
                [(A.listArray ((0, 0), (c - 1, r - 1)) (repeat Nothing), tiles)]
                (reverse moves)

-- | Given a placement 'idx', the directions to check 'direction', and
-- current possible boards from previous placements, return an updated
-- list with possible outcomes of those previous placements plus working
-- new tile additions.
puzzleIteration
        :: ((Int, Int), [Direction])
        -> [(Array (Int, Int) (Maybe Tile), [Tile])]
        -> [(Array (Int, Int) (Maybe Tile), [Tile])]
puzzleIteration (idx, directions) acc =
        acc >>= (\(board', tiles') -> placeTiles board' idx tiles' directions)

-- | Given a 'board', index 'idx', 'tiles', and 'directions', return a
-- list of all possible ways a tile in the list of tiles can be placed in
-- the specified index in a legal manner, along with the corresponding
-- leftover tiles.'
placeTiles
        :: Array (Int, Int) (Maybe Tile)
        -> (Int, Int)
        -> [Tile]
        -> [Direction]
        -> [(Array (Int, Int) (Maybe Tile), [Tile])]
placeTiles board idx tiles directions =
        [0 .. length tiles - 1] >>= placeTiles' board idx tiles directions

placeTiles'
        :: Array (Int, Int) (Maybe Tile)
        -> (Int, Int)
        -> [Tile]
        -> [Direction]
        -> Int
        -> [(Array (Int, Int) (Maybe Tile), [Tile])]
placeTiles' board idx tiles directions tileIdx
        | tileIdx >= length tiles
        = []
        | otherwise
        = (, take tileIdx tiles ++ drop (tileIdx + 1) tiles)
                <$> placeTile board idx (tiles !! tileIdx) directions

-- | Given a 'board', index 'idx', a 'tile', and 'directions'', return
-- a list of all working rotations of the tile when placed at the
-- specified index.
placeTile
        :: Array (Int, Int) (Maybe Tile)
        -> (Int, Int)
        -> Tile
        -> [Direction]
        -> [Array (Int, Int) (Maybe Tile)]
placeTile board idx tile directions =
        catMaybes
                $   flip (placeTile' board idx) directions
                <$> tileRotations tile

placeTile'
        :: Array (Int, Int) (Maybe Tile)
        -> (Int, Int)
        -> Tile
        -> [Direction]
        -> Maybe (Array (Int, Int) (Maybe Tile))
placeTile' board idx tile directions
        | _valid    = pure $ board A.// [(idx, pure tile)]
        | otherwise = Nothing
        where _valid = and (checkDirection board idx tile <$> directions)

-- | Check that a certain direction is satisfied when placing a tile
-- down.
checkDirection
        :: Array (Int, Int) (Maybe Tile)
        -> (Int, Int)
        -> Tile
        -> Direction
        -> Bool
checkDirection board (x, y) tile West
        | (x > x0 && x <= x1) && (y >= y0 && y <= y1) = maybe
                True
                (isComplement (tile ^. tileLeft) . view tileRight)
                (board A.! (x - 1, y))
        where ((x0, y0), (x1, y1)) = A.bounds board

checkDirection board (x, y) tile East
        | (x >= x0 && x < x1) && (y >= y0 && y <= y1) = maybe
                True
                (isComplement (tile ^. tileRight) . view tileLeft)
                (board A.! (x + 1, y))
        where ((x0, y0), (x1, y1)) = A.bounds board

checkDirection board (x, y) tile North
        | (x >= x0 && x <= x1) && (y > y0 && y <= y1) = maybe
                True
                (isComplement (tile ^. tileTop) . view tileBottom)
                (board A.! (x, y - 1))
        where ((x0, y0), (x1, y1)) = A.bounds board

checkDirection board (x, y) tile South
        | (x >= x0 && x <= x1) && (y >= y0 && y < y1) = maybe
                True
                (isComplement (tile ^. tileBottom) . view tileTop)
                (board A.! (x, y + 1))
        where ((x0, y0), (x1, y1)) = A.bounds board

checkDirection _ _ _ _ = False

tileRotations :: Tile -> [Tile]
tileRotations = take 4 . iterate rotateTile

rotateTile :: Tile -> Tile
rotateTile tile = execState
        (do
                tileLeft .= tile ^. tileTop
                tileRight .= tile ^. tileBottom
                tileTop .= tile ^. tileRight
                tileBottom .= tile ^. tileLeft
        )
        tile

-- | Check if a board is a rotation of another board.
boardCongruent
        :: (Ix b, Num b) => Array (b, b) Tile -> Array (b, b) Tile -> Bool
boardCongruent a = or . fmap (== a) . take 4 . iterate rotateBoard

rotateBoard :: (Ix a, Ix b, Num b) => Array (b, a) Tile -> Array (a, b) Tile
rotateBoard board = rotateTile <$> A.array
        ((swap *** swap) _bounds)
        (zip (swap . first (x1 + x0 -) <$> A.indices board) (A.elems board))
        where _bounds@((x0, _), (x1, _)) = A.bounds board
