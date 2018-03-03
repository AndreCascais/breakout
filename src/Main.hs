module Main where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.Vector


type Circle = (Point, Vector)
type Box = (Segment, Segment, Segment, Segment)
type Segment = (Point, Vector)


type Obj = (Shape, Coords)
type Coords = (Point, Vector)

data Shape = Window Box
    | Block Box
    | Bar Box
    | Ball
    deriving (Show)

data IntersectionType = Corner
    | Horizontal
    | Vertical
    | Null
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
blockDist = 1

makeWindow :: Float -> Float -> Point -> Obj
makeWindow x y (px, py) = (Window box, ((px, py), (0, 0)))
    where box = (((0 + px, 0 + py), (x, 0)), 
            ((x + px, 0 + py), (0, y)), 
            ((x + px, y + py), (-x, 0)), 
            ((0 + px, y + py), (0, -y)))

makeBlock :: Float -> Float -> Point -> Obj
makeBlock x y (px, py) = (Block box, ((px, py), (0, 0)))
    where box = (((0 + px, 0 + py), (x, 0)), 
            ((x + px, 0 + py), (0, y)), 
            ((x + px, y + py), (-x, 0)), 
            ((0 + px, y + py), (0, -y)))

makeLevel :: Int -> [Obj]
makeLevel 1 = [makeBlock blockWeight blockHeight (-300 + x * (blockWeight + blockDist), 0) | x <- [0..10]]

draw :: GameState -> Picture
draw (GameState ball others)  =
    pictures (map drawObj (ball : others))


drawObj :: Obj -> Picture
drawObj (shape, ((x, y), _)) = translate x y $ drawShape shape

drawShape :: Shape -> Picture
drawShape Ball = color blue ball
drawShape (Window _) = 
    color red window
drawShape (Block _) = color green block

ball, block, window :: Picture
ball = circleSolid ballRadius
block = polygon [(0, 0), (blockWeight, 0), (blockWeight, blockHeight), (0, blockHeight)]
window = line [(0, 0), (maxX * 2, 0), (maxX * 2, maxY * 2), (0, maxY * 2), (0, 0)]



initialBall :: IO Circle
initialBall = do
    x <- randomRIO (- maxX, 0)
    --y <- randomRIO (- maxY, maxY)
    return ((x, -maxY + 10), (200, 200))


update :: Float -> GameState -> GameState
update dt (GameState b segList) = GameState b' segList
    where b' = moveObj dt (updateObj dt b segList)

moveObj :: Float -> Obj -> Obj
moveObj dt (shape, coords) = (shape, moveCoords dt coords)

moveCoords :: Float -> Coords -> Coords
moveCoords dt ((x, y), (vx, vy)) = ((x + vx * dt, y + vy * dt), (vx, vy))

updateObj :: Float -> Obj -> [Obj] -> Obj
updateObj dt (Ball, coords) segList = (Ball, updateBall dt coords intersections)
    where 
        intersections = map (intersectType dt coords) segList


--newBall :: Float -> Circle -> [Circle] -> Circle
--newBall dt ((x, y), (vx, ))

updateBall :: Float -> Circle -> [IntersectionType] -> Circle 
updateBall _ c [] = c
updateBall dt (p, (vx, vy)) (intType:xs) = updateBall dt newCoords xs
   -- | intType == Horizontal = updateBall dt ((x - dx, y + dy), (-vx, vy))
   -- | intType == Vertical = ((x + dx, y - dy), (vx, -vy))
   -- | otherwise = ((x + dx, y + dy), (vx, vy))
    where
       -- intType = intersectType dt ((x, y), (vx, vy)) 
        newCoords
            | intType == Horizontal = (p, (-vx, vy))
            | intType == Vertical = (p, (vx, -vy))
            | otherwise = (p, (vx, vy))
        --dx = vx * dt
        --dy = vy * dt

--intersectCorner :: Float -> Ball -> Obj -> Bool
--intersectCorner dt ((x, y), (vx, vy)) (Block )

intersectType :: Float -> Circle -> Obj -> IntersectionType
intersectType dt ball (Window (s1, s2, s3, s4), (pos, _))
        | intersectsSegment dt ball s1 = Vertical
        | intersectsSegment dt ball s2 = Horizontal
        | intersectsSegment dt ball s3 = Vertical
        | intersectsSegment dt ball s4 = Horizontal
        | otherwise = Null
intersectType dt ball (Block (s1, s2, s3, s4), (pos, _))
        | intersectsSegment dt ball s1 = Vertical
        | intersectsSegment dt ball s2 = Horizontal
        | intersectsSegment dt ball s3 = Vertical
        | intersectsSegment dt ball s4 = Horizontal
        | otherwise = Null

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
    return (GameState (Ball, ballCoords) (win:level))
    where
        win = makeWindow (fromIntegral 800) (fromIntegral 600) (-maxX, -maxY)
        level = makeLevel 1

gameWindow :: Display
gameWindow = InWindow "breakout" (weight, height) (0, 0)

main :: IO ()
main = do
    initial <- initialState
    simulate gameWindow black fps initial draw (\_ -> update)