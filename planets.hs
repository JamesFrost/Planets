module MyAnimation where
import Animation

--config start
svgHeight :: Double
svgHeight = 600

svgWidth :: Double
svgWidth = 800

--size, colour
planetShapeConfig :: [(Double, Colour)]
planetShapeConfig = 
	[
		(25, yellow), 
		(7.5, green), 
		(11, maroon), 
		(12.5, blue),
		(10, red),
		(15, gray)
	]

--speed, direction, radius
planetMovementConfig :: [(Double, OrbitDirection, Double)]
planetMovementConfig = 
	[
		(0, CounterClockwise, 0), 
		(0.5, CounterClockwise, 50), 
		(0.35, Clockwise, 90), 
		(0.3, CounterClockwise, 135),
		(0.25, CounterClockwise, 185),
		(0.20, Clockwise, 245)
	]

--size, colour, radius, orbit direction
moonConfig :: [ [(Double, Colour, Double, OrbitDirection)] ]
moonConfig = 
	[
		[], 
		[], 
		[], 
		[(5, black, 20, CounterClockwise)],
		[],
		[(7.5, purple, 30, Clockwise), (4, aqua, 50, CounterClockwise)]
	]

--config end

picture :: Animation
picture = allPlanets

--Builds all planets outlined in the config above
allPlanets :: Animation
allPlanets = combine (zipWith3 buildPlanet planetShapeConfig planetMovementConfig moonConfig)

--Builds a single planet outlined in the config above
buildPlanet :: (Double, Colour) -> (Double, OrbitDirection, Double) -> [(Double, Colour, Double, OrbitDirection)]-> Animation
buildPlanet shapeConfig movementConfig moonConfig = 
	getOrbitOutline movementConfig
		`plus` 
			getPlanetAnimation shapeConfig movementConfig 
				`plus`
					getAllMoonAnimations moonConfig movementConfig 

--Builds all moon animations for a planet
getAllMoonAnimations :: [(Double, Colour, Double, OrbitDirection)] -> (Double, OrbitDirection, Double) -> Animation
getAllMoonAnimations moonConfig movementConfig = 
	combine ( map moonAnimation moonConfig )
		where
			moonAnimation = getMoonAnimation movementConfig

--Builds a single moon animation
getMoonAnimation :: (Double, OrbitDirection, Double) -> (Double, Colour, Double, OrbitDirection) -> Animation
getMoonAnimation movementConfig (size, colour, radius, orbitDirection) = 
	translate ( getMoonTranslation movementConfig (size, colour, radius, orbitDirection) ) $
	getPlanetShape (size, colour)

--Generates the translation config for moon animations
getMoonTranslation :: (Double, OrbitDirection, Double) -> (Double, Colour, Double, OrbitDirection) -> Varying (Double, Double)
getMoonTranslation (speed, orbitDirection, radius) (size, colour, moonRadius, moonOrbitDirection) = 
	cycleSmooth speed (getMoonOrbitCoordinates moonRadius size 0 moonOrbitDirection orbitingPlanetCoordinates)
		where
			orbitingPlanetCoordinates = getPlanetOrbitCoordinates radius orbitDirection

--Recursively generates the coordinates of a moons orbit
--Steps through the coordinates of the planet the moon is orbiting
getMoonOrbitCoordinates :: Double -> Double -> Double -> OrbitDirection-> [(Double, Double)] -> [(Double, Double)]
getMoonOrbitCoordinates moonRadius moonSize angle moonOrbitDirection [] = []
getMoonOrbitCoordinates moonRadius moonSize angle moonOrbitDirection ( (x,y):xs ) =	
	(getCoordinateOnCircumference angle moonRadius x cos, getCoordinateOnCircumference angle moonRadius y sin) : 
		getMoonOrbitCoordinates moonRadius moonSize nextAngle moonOrbitDirection xs
			where
				nextAngle = getNextMoonOrbitAngle angle moonOrbitDirection

--Returns the next angle in a moons orbit
getNextMoonOrbitAngle :: Double -> OrbitDirection -> Double
getNextMoonOrbitAngle angle orbitDirection
	| orbitDirection == Clockwise = angle + 0.25
	| otherwise = angle - 0.25

--Background outline of a planets orbit
getOrbitOutline :: (Double, OrbitDirection, Double) -> Animation
getOrbitOutline (speed, orbitDirection, radius) =
	translate (always (xCenter, yCenter)) $
	withBorder (always grey) (always 2.5) $
	withoutPaint (circle (always radius))

--Builds a single planet animation
getPlanetAnimation :: (Double, Colour) -> (Double, OrbitDirection, Double) -> Animation
getPlanetAnimation shapeConfig movementConfig =
	translate (getPlanetTranslation movementConfig) $
	getPlanetShape shapeConfig

--Get the translation config for a single planet
getPlanetTranslation :: (Double, OrbitDirection, Double) -> Varying (Double, Double)
getPlanetTranslation (speed, orbitDirection, radius) =
	cycleSmooth speed (getPlanetOrbitCoordinates radius orbitDirection)

--Generates the coordinates of a planets orbit
getPlanetOrbitCoordinates :: Double -> OrbitDirection -> [(Double, Double)]
getPlanetOrbitCoordinates radius orbitDirection
	| orbitDirection == Clockwise = orbitCoordinates
	| otherwise = reverse orbitCoordinates
	where
		orbitCoordinates =
			--scale using the orbit radius, then apply x and y center offsets to coordinates on the orbits circumference 
			map (\(x, y) -> ( (radius*x)+xCenter, (radius*y)+yCenter) ) circleCircumferenceCoordinates

--List of (x,y) coordinates on the circumference of a circle with radius 1
--Circle is centered around (0,0)
circleCircumferenceCoordinates :: [(Double, Double)]
circleCircumferenceCoordinates = 
	map getCoordinatePairOnCircumference [0.0,0.1..359]

--Retuns (x,y) coordinates on a circles circumference at a specified angle
getCoordinatePairOnCircumference :: Double -> (Double, Double)
getCoordinatePairOnCircumference angle =
	(getCoordinateOnCircumference angle 1 0 cos, getCoordinateOnCircumference angle 1 0 sin)

--Gets a single coordinate (either x or y) on a circles circumference
getCoordinateOnCircumference :: Double -> Double -> Double -> (Double -> Double) -> Double
getCoordinateOnCircumference angle radius circleMidPoint trigFunc = 
	circleMidPoint + radius * (trigFunc angle) 

getPlanetShape :: (Double, Colour) -> Animation
getPlanetShape (size, colour) =
	withPaint (always colour) $
	circle (always size)

xCenter :: Double
xCenter = svgWidth/2

yCenter :: Double
yCenter = svgHeight/2

data OrbitDirection = Clockwise | CounterClockwise deriving (Enum, Eq)

make :: IO ()
make = writeFile "planets.svg" (svg svgWidth svgHeight picture)
