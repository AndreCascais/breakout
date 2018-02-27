module Main where

import System.Random
import Graphics.Gloss

type Ball = (Point, Vector)

fps, height, weight :: Int
fps = 30
maxX = 150
maxY = 150
height = 300
weight = 300

ballRadius :: Float
ballRadius = 10

drawBall :: Ball -> Picture
drawBall ((x, y), (vx, vy)) =
    translate x y (color blue (circleSolid ballRadius))

initialBall :: IO Ball
initialBall = do
    x <- randomRIO (- maxX, maxY)
    y <- randomRIO (- maxY, maxY)
    return ((x, y), (1, 1))

updateBall :: Float -> Ball -> Ball
updateBall f  ((x, y), (vx, vy)) = 
    ((x + vx, y + vy), (vx, vy))

window :: Display
window = InWindow "breakout" (height, weight) (0, 0)

main :: IO ()
main = do
    ball <- initialBall
    simulate window black fps ball drawBall (\_ -> updateBall)