module Main where

import           Puzzle


puzzle0 :: [Tile]
puzzle0 =
        [ Tile { _tileLeft   = OrangeFront
               , _tileRight  = GreenBack
               , _tileTop    = BlueBack
               , _tileBottom = PinkBack
               }
        , Tile { _tileLeft   = BlueBack
               , _tileRight  = PinkFront
               , _tileTop    = OrangeFront
               , _tileBottom = OrangeFront
               }
        , Tile { _tileLeft   = OrangeBack
               , _tileRight  = PinkBack
               , _tileTop    = GreenBack
               , _tileBottom = OrangeBack
               }
        , Tile { _tileLeft   = BlueBack
               , _tileRight  = BlueFront
               , _tileTop    = GreenBack
               , _tileBottom = PinkFront
               }
        , Tile { _tileLeft   = GreenFront
               , _tileRight  = PinkFront
               , _tileTop    = OrangeBack
               , _tileBottom = GreenBack
               }
        , Tile { _tileLeft   = PinkBack
               , _tileRight  = GreenFront
               , _tileTop    = BlueBack
               , _tileBottom = OrangeBack
               }
        , Tile { _tileLeft   = BlueFront
               , _tileRight  = PinkBack
               , _tileTop    = OrangeBack
               , _tileBottom = GreenFront
               }
        , Tile { _tileLeft   = GreenFront
               , _tileRight  = BlueFront
               , _tileTop    = PinkFront
               , _tileBottom = BlueBack
               }
        , Tile { _tileLeft   = GreenBack
               , _tileRight  = PinkFront
               , _tileTop    = BlueFront
               , _tileBottom = OrangeFront
               }
        ]

moves0 :: [((Int, Int), [Direction])]
moves0 =
        [ ((0, 0), [])
        , ((1, 0), [West])
        , ((0, 1), [North])
        , ((1, 1), [West, North])
        , ((2, 0), [West])
        , ((2, 1), [West, North])
        , ((0, 2), [North])
        , ((1, 2), [West, North])
        , ((2, 2), [West, North])
        ]

main :: IO ()
main = print $ solvePuzzle 3 3 puzzle0 moves0
