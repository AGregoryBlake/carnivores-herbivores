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
plantGrowPerTick = 0.05 :: Double -- arbitrary grass growth rate.
plantBaseNutrition = 1.0 -- arbitrary unit of nutrition from grass.

data Plant = Plant Location Height
-- data Beast = Location NeuralNetwork HitPoints FoodStores AttackPower Carnivorousness MutationRate Flightiness
data World = World [Plant] -- [Beast]

plantLocations :: [Location]
plantLocations = [(P $ V2 (CInt (fromIntegral x)) (CInt (fromIntegral y))) | x <- [0,10..width], y <- [0,10..height]]

initPlant :: [Location] -> [Height] -> [Plant]
initPlant locations heights = zipWith (\location height -> Plant location height) locations heights

initPlantsStatic :: [Plant]
initPlantsStatic = initPlant plantLocations heights
    where heights = take (length plantLocations) $ repeat (100 :: Word8)

initPlantsRandom :: IO [Plant]
initPlantsRandom = do
    g <- newStdGen
    let randomHeights = take (length plantLocations) $ randomRs (0,255) g
    return $ initPlant plantLocations randomHeights

initWorld :: [Plant] -> World
initWorld plants = World plants

initWorldStatic :: World
initWorldStatic = initWorld initPlantsStatic

initWorldRandom :: IO World
initWorldRandom = do
    plants <- initPlantsRandom
    return $ initWorld plants

updateWorld :: World -> IO World
updateWorld (World plants) = do
    plants' <- plantsGrow plants
    return $ World plants'

plantsGrow :: [Plant] -> IO [Plant]
plantsGrow plants = do
    g <- newStdGen
    let plantGrowChance = randomRs (0.0,0.1) g
    let plantGrowList = zip plants plantGrowChance
    let growConditions = growChance <= plantGrowPerTick && height < 256
    return $ map (\(plant@(Plant location height),growChance) -> if growConditions then Plant location (height + 1) else plant) plantGrowList

