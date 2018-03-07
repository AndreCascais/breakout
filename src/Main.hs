module Main where

import Data.Maybe
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game


type Circle = (Point, Vector)

type Segment = (Point, Vector)

type Coords = (Point, Vector)

data Obj = Window Point
    | Block Point Int
    | Bar Coords
    | Ball Coords
    deriving (Show)

data IntersectionType = Corner
    | Horizontal
    | Vertical
    deriving (Eq, Show)

data GameState = GameState Obj Obj [Obj]
    deriving (Show)

fps, height, weight :: Int
fps = 60
maxX = 400
maxY = 300
weight = 1024
height = 800


ballRadius, blockWeight, blockHeight, barWeight, barHeight :: Float
ballRadius = 10
blockDist = 2
blockWeight = 50
blockHeight = 20
barWeight = 200
barHeight = 20
barSpeed = 200


blockColors :: [Color]
blockColors = [yellow, green, orange, red, cyan, white, blue]

makeWindow :: Point -> Obj
makeWindow = Window

makeBlock :: Point -> Int -> Obj
makeBlock = Block

makeBar :: Point -> Obj
makeBar p = Bar (p, (0, 0))

makeLevel :: Int -> [Obj]
makeLevel 1 = [makeBlock (-300 + x * (blockWeight + blockDist), 0) 1| x <- [0..10]]
makeLevel n = 
    [makeBlock (-300 + x * (blockWeight + blockDist), fromIntegral (n - 1) * (blockHeight + blockDist)) n | x <- [0..10]] ++ makeLevel (n - 1)

draw :: GameState -> Picture
draw (GameState bar ball others)  =
    pictures (map drawObj (bar : ball : others))

drawObj :: Obj -> Picture
drawObj (Window (x, y)) = translate x y $ color red window
drawObj (Block (x, y) l) = translate x y $ color (blockColors !! (l - 1)) block
drawObj (Ball ((x, y), _)) = translate x y $ color blue ball
drawObj (Bar ((x, y), _)) = translate x y $ color white bar

ball, block, window :: Picture
ball = circleSolid ballRadius
block = polygon [(0, 0), (blockWeight, 0), (blockWeight, blockHeight), (0, blockHeight)]
window = line [(0, 0), (maxX * 2, 0), (maxX * 2, maxY * 2), (0, maxY * 2), (0, 0)]
bar = polygon [(0, 0), (barWeight, 0), (barWeight, barHeight), (0, barHeight)]

initialBall :: IO Circle
initialBall = do
    x <- randomRIO (- maxX, 0)
    return ((x, -maxY + barHeight * 2), (200, 200))

update :: Float -> GameState -> GameState
update dt gs = 
    move dt (detectCollisions dt gs)

detectCollisions :: Float -> GameState -> GameState
detectCollisions dt (GameState bar b others) = GameState bar b' others'
    where
        intersected = map updateObj [obj | obj <- others, isJust (intersectType dt b obj)]
        nonIntersected = [obj | obj <- others, isNothing (intersectType dt b obj)]
        others' = nonIntersected ++ filter isAlive intersected
        b' = updateBall dt b (bar:intersected)

isAlive :: Obj -> Bool
isAlive (Block _ 0) = False
isAlive obj = True

updateObj :: Obj -> Obj
updateObj (Block p l) = Block p (l - 1)
updateObj obj = obj

move:: Float -> GameState -> GameState
move dt (GameState bar b others) = GameState (moveObj dt bar) (moveObj dt b) (map (moveObj dt) others)

moveObj :: Float -> Obj -> Obj
moveObj dt (Ball ((x, y), (vx, vy))) = Ball ((x + vx * dt, y + vy * dt), (vx, vy))
moveObj dt (Bar ((x, y), (vx, vy)))
    | x + vx * dt + barWeight >= maxX = Bar ((maxX - barWeight, y + vy * dt), (vx, vy))
    | x + vx * dt <= -maxX = Bar ((-maxX, y + vy * dt), (vx, vy))
    | otherwise = Bar ((x + vx * dt, y + vy * dt), (vx, vy))
moveObj _ obj = obj

updateBall :: Float -> Obj -> [Obj] -> Obj
updateBall _ c [] = c
updateBall dt (Ball (p, (vx, vy))) (obj:xs) = updateBall dt newCoords xs
    where
        coords = (p, (vx, vy))
        intType = intersectType dt (Ball coords) obj
        newCoords
            | intType == Just Horizontal = Ball (p, (-vx, vy))
            | intType == Just Vertical = Ball (p, (vx, -vy))
            | otherwise = Ball (p, (vx, vy))

--intersectCorner :: Float -> Ball -> Obj -> Bool
--intersectCorner dt ((x, y), (vx, vy)) (Block )

intersectType :: Float -> Obj -> Obj -> Maybe IntersectionType
intersectType dt (Ball coord) obj
        | intersectsSegment dt coord s1 || intersectsSegment dt coord s3 = Just Vertical
        | intersectsSegment dt coord s2 || intersectsSegment dt coord s4 = Just Horizontal
        | otherwise = Nothing
        where
            (s1, s2, s3, s4) = makeSegments' obj
            makeSegments' (Window p) = makeSegments p (2 * maxX, 2 * maxY)
            makeSegments' (Bar ((px, py), (vx, vy))) = makeSegments (px + dt * vx, py + dt * vy) (barWeight, barHeight)
            makeSegments' (Block p l) = makeSegments p (blockWeight, blockHeight)
            makeSegments (px, py) (vx, vy) = (((px, py), (vx, 0)),
                                                ((px + vx, py), (0, vy)),
                                                ((px + vx, py + vy), (-vx, 0)),
                                                ((px, py + vy), (0, -vy)))

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

handle :: Event -> GameState -> GameState 
handle (EventKey (SpecialKey KeyLeft) Down _ _) (GameState (Bar (p, (vx, vy))) b others) = 
    GameState (Bar (p, (-barSpeed, 0))) b others
handle (EventKey (SpecialKey KeyRight) Down _ _) (GameState (Bar (p, _)) b others) = 
    GameState (Bar (p, (barSpeed, 0))) b others
handle (EventKey (SpecialKey KeyLeft) Up _ _) (GameState (Bar (p, _)) b others) = 
        GameState (Bar (p, (0, 0))) b others
handle (EventKey (SpecialKey KeyRight) Up _ _) (GameState (Bar (p, _)) b others) = 
        GameState (Bar (p, (0, 0))) b others
handle ev gs = gs

initialState :: IO GameState
initialState = do
    ballCoords <- initialBall
    return (GameState bar (Ball ballCoords) (window:level))
    where
        window = makeWindow (-maxX, -maxY)
        level = makeLevel 4
        bar = makeBar (-barWeight / 2, -maxY)

gameWindow :: Display
gameWindow = InWindow "breakout" (weight, height) (0, 0)

main :: IO ()
main = do
    initial <- initialState
    play gameWindow black fps initial draw handle update