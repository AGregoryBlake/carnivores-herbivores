module World where
        
import Linear
import Linear.Affine
import Foreign.C.Types
import Neural
import Data.Word
import System.Random
       
type Location = Point V2 CInt
type Height = Word8

width = 500 :: CInt
height = 500 :: CInt

maxBeasts = 1000000 -- arbitrary cap
grassGrowPerTick = 0.05 :: Double -- arbitrary grass growth rate.
grassBaseNutrition = 1.0 -- arbitrary unit of nutrition from grass.

data Grass = Grass Location Height
-- data Beast = Location NeuralNetwork HitPoints FoodStores AttackPower Carnivorousness MutationRate Flightiness
data World = World [Grass] -- [Beast]

grassLocations :: [Location]
grassLocations = [(P $ V2 (CInt (fromIntegral x)) (CInt (fromIntegral y))) | x <- [0..width], y <- [0..height]]

initGrass :: [Location] -> [Height] -> [Grass]
initGrass locations heights = zipWith (\location height -> Grass location height) locations heights

initGrassStatic :: [Grass]
initGrassStatic = initGrass grassLocations heights
    where heights = take (length grassLocations) $ repeat (100 :: Word8)

initGrassRandom :: IO [Grass]
initGrassRandom = do
    g <- newStdGen
    let randomHeights = take (length grassLocations) $ randomRs (0,255) g
    return $ initGrass grassLocations randomHeights

initWorld :: [Grass] -> World
initWorld grasses = World grasses

initWorldStatic :: World
initWorldStatic = initWorld initGrassStatic

initWorldRandom :: IO World
initWorldRandom = do
    grasses <- initGrassRandom
    return $ initWorld grasses

updateWorld :: World -> IO World
updateWorld (World grass) = do
    grass' <- grassGrows grass
    return $ World grass'

grassGrows :: [Grass] -> IO [Grass]
grassGrows grass = do
    g <- newStdGen
    let grassGrowChance = randomRs (0.0,0.1) g
    let grassGrowList = zip grass grassGrowChance
    return $ map (\(grass@(Grass location height),growChance) -> if growChance <= grassGrowPerTick then Grass location (height + 1) else grass) grassGrowList
