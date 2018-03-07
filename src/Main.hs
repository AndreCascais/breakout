module Main where

import Data.Maybe
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.Vector


type Circle = (Point, Vector)
type Box = (Segment, Segment, Segment, Segment)
-- box with Point + Vector
type Segment = (Point, Vector)


type Obj = (Shape, Coords)
type Coords = (Point, Vector)

data Shape = Window Box
    | Block Box Int
    | Bar Box
    | Ball
    deriving (Show)

data IntersectionType = Corner
    | Horizontal
    | Vertical
    deriving (Eq, Show)

data GameState = GameState Obj [Obj]
    deriving (Show)

fps, height, weight :: Int
fps = 60
maxX = 400
maxY = 300
weight = 1024
height = 800
blockWeight = 50
blockHeight = 20

ballRadius :: Float
ballRadius = 10
blockDist = 2

blockColors :: [Color]
blockColors = [yellow, green, orange, red, cyan, white, blue]

makeWindow :: Float -> Float -> Point -> Obj
makeWindow x y (px, py) = (Window box, ((px, py), (0, 0)))
    where box = (((0 + px, 0 + py), (x, 0)), 
            ((x + px, 0 + py), (0, y)), 
            ((x + px, y + py), (-x, 0)), 
            ((0 + px, y + py), (0, -y)))

makeBlock :: Float -> Float -> Int -> Point -> Obj
makeBlock x y lives (px, py) = (Block box lives, ((px, py), (0, 0)))
    where box = (((0 + px, 0 + py), (x, 0)), 
            ((x + px, 0 + py), (0, y)), 
            ((x + px, y + py), (-x, 0)), 
            ((0 + px, y + py), (0, -y)))

makeLevel :: Int -> [Obj]
makeLevel 1 = [makeBlock blockWeight blockHeight 1 (-300 + x * (blockWeight + blockDist), 0) | x <- [0..10]]
makeLevel n = 
    [makeBlock blockWeight blockHeight n (-300 + x * (blockWeight + blockDist), fromIntegral (n - 1) * (blockHeight + blockDist)) | x <- [0..10]] ++ makeLevel (n - 1)

draw :: GameState -> Picture
draw (GameState ball others)  =
    pictures (map drawObj (ball : others))

drawObj :: Obj -> Picture
drawObj (shape, ((x, y), _)) = translate x y $ drawShape shape

drawShape :: Shape -> Picture
drawShape Ball = color blue ball
drawShape (Window _) = 
    color red window
drawShape (Block _ l) = color (blockColors !! (l - 1)) block

ball, block, window :: Picture
ball = circleSolid ballRadius
block = polygon [(0, 0), (blockWeight, 0), (blockWeight, blockHeight), (0, blockHeight)]
window = line [(0, 0), (maxX * 2, 0), (maxX * 2, maxY * 2), (0, maxY * 2), (0, 0)]

initialBall :: IO Circle
initialBall = do
    x <- randomRIO (- maxX, 0)
    return ((x, -maxY + 10), (400, 400))

update :: Float -> GameState -> GameState
update dt gs = 
    move dt (detectCollisions dt gs)

detectCollisions :: Float -> GameState -> GameState
detectCollisions dt (GameState b others) = GameState b' others'
    where
        intersected = map updateObj [obj | obj <- others, isJust (intersectType dt b obj)]
        nonIntersected = [obj | obj <- others, isNothing (intersectType dt b obj)]
        others' = nonIntersected ++ filter isAlive intersected
        b' = updateBall dt b intersected

isAlive :: Obj -> Bool
isAlive (Window _, _) = True
isAlive (Block _ 0, _) = False
isAlive (Block _ _, _) = True

updateObj :: Obj -> Obj
updateObj (Block box lives, coordsBlock) =
    (Block box (lives - 1), coordsBlock)
updateObj o = o

move:: Float -> GameState -> GameState
move dt (GameState b segList) = GameState (moveObj dt b) (map (moveObj dt) segList)

moveObj :: Float -> Obj -> Obj
moveObj dt (shape, coords) = (shape, moveCoords dt coords)

moveCoords :: Float -> Coords -> Coords
moveCoords dt ((x, y), (vx, vy)) = ((x + vx * dt, y + vy * dt), (vx, vy))

updateBall :: Float -> Obj -> [Obj] -> Obj
updateBall _ c [] = c
updateBall dt (Ball, (p, (vx, vy))) (obj:xs) = updateBall dt newCoords xs
    where
        coords = (p, (vx, vy))
        intType = intersectType dt (Ball, coords) obj
        newCoords
            | intType == Just Horizontal = (Ball, (p, (-vx, vy)))
            | intType == Just Vertical = (Ball, (p, (vx, -vy)))
            | otherwise = (Ball, (p, (vx, vy)))

--intersectCorner :: Float -> Ball -> Obj -> Bool
--intersectCorner dt ((x, y), (vx, vy)) (Block )

intersectType :: Float -> Obj -> Obj -> Maybe IntersectionType
intersectType dt (Ball, coord) (Window (s1, s2, s3, s4), (pos, _))
        | intersectsSegment dt coord s1 || intersectsSegment dt coord s3 = Just Vertical
        | intersectsSegment dt coord s2 || intersectsSegment dt coord s4 = Just Horizontal
        | otherwise = Nothing
intersectType dt (Ball, coord) (Block (s1, s2, s3, s4) _, (pos, _))
        | intersectsSegment dt coord s1 || intersectsSegment dt coord s3 = Just Vertical
        | intersectsSegment dt coord s2 || intersectsSegment dt coord s4 = Just Horizontal
        | otherwise = Nothing


intersectsSegment :: Float -> Circle -> Segment -> Bool
intersectsSegment dt ((x, y), (vx, vy)) ((a, b), (c, d)) =
    not ( magV (px, py) > magV (c, d) || not (sameQuad (px, py) (c, d))) && --case when the point is outside the segment, will catch that with corner
    magV (x + dx - (a + px), y + dy - (b + py)) <= ballRadius
    where 
        (px, py) = projectVector (x + dx - a, y + dy - b) (c, d)
        dx = vx * dt
        dy = vy * dt

-- project a into b
projectVector :: Vector -> Vector -> Vector
projectVector a b = mulSV scalar (normalizeV b)
    where scalar = dotV a (mulSV (1 / magV b) b)

-- checks if a and b are in same quad, axis count for either quad
sameQuad :: Vector -> Vector -> Bool
sameQuad (a, b) (c, d) = sameSignal a c && sameSignal b d

-- checks if two floats have the same signal
sameSignal :: Float -> Float -> Bool
sameSignal 0 b = True
sameSignal a 0 = True
sameSignal a b = a / abs a == b / abs b


initialState :: IO GameState
initialState = do
    ballCoords <- initialBall
    return (GameState (Ball, ballCoords) (window:level))
    where
        window = makeWindow (fromIntegral 800) (fromIntegral 600) (-maxX, -maxY)
        level = makeLevel 4

gameWindow :: Display
gameWindow = InWindow "breakout" (weight, height) (0, 0)

main :: IO ()
main = do
    initial <- initialState
    simulate gameWindow black fps initial draw (\_ -> update)