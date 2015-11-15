module Demo where
import Animation

svgHeight :: Double
svgHeight = 1000

svgWidth :: Double
svgWidth = 1000

--individual planet config
--format: size, speed, orbit radius, skew
sunConfig :: ( Double, Double, Double )
sunConfig = (25.0, 0.0, 0.0)

earthConfig :: ( Double, Double, Double )
earthConfig = (12.5, 0.2, 50)

allPlanetConfig :: [(Double, Double, Double)]
allPlanetConfig = [sunConfig, earthConfig]

	

pic :: Animation
pic = allPlanets

allPlanets :: Animation
allPlanets = joinShapes [ getPlanetConfig n | n <- allPlanetConfig ]

getPlanetConfig :: ( Double, Double, Double ) -> Animation
getPlanetConfig (size, speed, orbitradius) = translate (getTranslationConfig orbitradius speed) (getCircleShape size)

getTranslationConfig :: Double -> Double-> Varying (Double, Double)
getTranslationConfig orbitradius speed
	| orbitradius == 0 = always (xCenter, yCenter)
	| otherwise = cycleSmooth speed (getCircleCoordinates orbitradius)

getCircleCoordinates :: Double -> [ (Double, Double) ]
getCircleCoordinates radius = [ (getCoordinateOnCircumference angle radius xCenter cos, getCoordinateOnCircumference angle radius yCenter sin) | angle <- [0..365] ]

getCoordinateOnCircumference :: Double -> Double -> Double -> (Double -> Double) -> Double
getCoordinateOnCircumference angle radius midPoint trigFunc = (midPoint + radius *(trigFunc angle) )

getCircleShape :: Double -> Animation
getCircleShape radius = circle (always radius)

--need to refactor this
joinShapes :: [Animation] -> Animation
joinShapes shapeList
	| (length shapeList) > 1 = (head shapeList) `plus` joinShapes (tail shapeList)
	| (length shapeList) == 1 = head shapeList	

xCenter :: Double
xCenter = svgWidth/2

yCenter :: Double
yCenter = svgHeight/2

make :: IO ()
make = writeFile "test.svg" (svg svgHeight svgWidth pic)
