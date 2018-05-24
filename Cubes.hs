{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import          Control.Arrow
import          Control.Concurrent
import          Control.Monad.Random
import          Control.Monad.Reader
import          Data.Colour.RGBSpace
import          Data.Colour.RGBSpace.HSV
import          Data.Foldable           (for_)
import          Data.List               (nub)
import          Data.Semigroup          ((<>))
import          Data.Time.Clock.POSIX
import          Graphics.Rendering.Cairo
import          Linear.V2
import          Linear.Vector
import qualified Numeric.Noise.Perlin    as P
import          Text.Printf

data World = World
  { worldWidth  :: Int
  , worldHeight :: Int
  , worldSeed   :: Int
  , worldScale  :: Double
  }

-- a---b
-- /   /
-- c---d
data Quad = Quad
  { quadA :: V2 Double
  , quadB :: V2 Double
  , quadC :: V2 Double
  , quadD :: V2 Double
  } deriving (Eq, Ord)

--       a
--     /   \
--    b-----c
data Tri = Tri
  { triA :: V2 Double
  , triB :: V2 Double
  , triC :: V2 Double
  } deriving (Eq, Ord)

--a   c   e
-- \ / \ / \
--  b   d   f
data Zigzag = Zigzag
  { zigA :: V2 Double
  , zigB :: V2 Double
  , zigC :: V2 Double
  , zigD :: V2 Double
  , zigE :: V2 Double
  , zigF :: V2 Double
  } deriving(Eq, Ord)

type Generate a = RandT StdGen (ReaderT World Render) a

-- / Lift a Cairo action into a Generate action
cairo :: Render a -> Generate a
cairo = lift . lift

main :: IO()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  let
    stdGen = mkStdGen seed
    width = 60
    height = 60
    scaleAmount = 20

    scaledWidth = round $ fromIntegral width * scaleAmount
    scaledHeight = round $ fromIntegral height * scaleAmount
  surface <- createImageSurface FormatARGB32 scaledWidth scaledHeight
  -- The "world" thinks the width and height are the initial values, not scaled
  let world = World width height seed scaleAmount

  void
    . renderWith surface
    . flip runReaderT world
    . flip runRandT stdGen
    $ do
      cairo $ scale scaleAmount scaleAmount
      renderSketch
  putStrLn "Generating art..."
  surfaceWriteToPNG surface
    $ "images/example_sketch/"
    <> show seed <> "-" <> show (round scaleAmount :: Int) <> ".png"
  surfaceWriteToPNG surface "images/example_sketch/latest.png"

getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (worldWidth &&& worldHeight)
  pure (fromIntegral w, fromIntegral h)

fillScreen :: (Double -> Render a) -> Double -> Generate()
fillScreen color opacity = do
  (w, h) <- getSize @Double
  cairo $ do
    rectangle 0 0 w h
    color opacity *> fill

hsva :: Double -> Double -> Double -> Double -> Render()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
  where RGB{..} = hsv h s v

eggshell :: Double -> Render ()
eggshell = hsva 71 0.13 0.96

teaGreen :: Double ->Render ()
teaGreen = hsva 81 0.25 0.94

lime :: Double  -> Render ()
lime = hsva 101 0.57 0.83

vividTangerine :: Double -> Render ()
vividTangerine = hsva 11 0.40 0.92

aquamarine :: Double -> Render ()
aquamarine = hsva 191 97.6 65.9

englishVermillion :: Double -> Render ()
englishVermillion = hsva 355 0.68 0.84

renderSketch :: Generate ()
renderSketch = do
  fillScreen eggshell 1

  cairo $ setLineWidth 0.15

  quads <- genQuadGrid
  tris <- genTriGrid
  zigzags <- genZigzag
  noisyQuads <- traverse quadAddNoise quads
  noisyTris <- traverse triAddNoise tris
  -- / Original values: fill 0.4, stroke 0.6
  for_ noisyQuads $ \quad -> do
    strokeOrFill <- weighted [(fill, 0.8), (stroke, 0.2)]
    color <- uniform
      [ lime
      , vividTangerine
      , englishVermillion
      , darkGunmetal
      ]
    cairo $  do
      renderQuad quad
      color 1 *> strokeOrFill

  {--for_ noisyTris $ \tri -> do
    strokeOrFill <- weighted [(fill, 0.7), (stroke, 0.3)]
    color <- uniform
      [ lime
      , teaGreen
      , vividTangerine
      , eggshell
      ]
    cairo $  do
      renderTri tri
      color 1 *> strokeOrFill
--}
  {--for_ zigzags $ \zig -> do
    strokeOrFill <- weighted [(fill, 0.1), (stroke, 0.9)]
    color <- uniform
      [ lime
      , teaGreen
      , vividTangerine
      , aquamarine
      ]
    cairo $ do
      renderZigzag zig
      color 1 *> strokeOrFill

--}



fromIntegralVector :: V2 Int -> V2 Double
fromIntegralVector (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

genQuadGrid :: Generate [Quad]
genQuadGrid = do
  (w, h) <- getSize @Int
  vectors <- replicateM 800 $ do
    v <- V2 <$> getRandomR (3, w `div` 2 - 3) <*> getRandomR (3, h `div` 2 - 3)
    pure $ v ^* 2
  pure . nub . flip map vectors $ \v ->
    let v' = fromIntegralVector v
    --originally 0 1.5 1.5 1.5 1.5 0
    in Quad v' (v' ^+^ V2 1 1.5) (v' ^+^ V2 1.5 1.5) (v' ^+^ V2 1.5 7)

genZigzag :: Generate[Zigzag]
genZigzag = do
  (w, h) <- getSize @Int
  vectors <- replicateM 800 $ do
    v <- V2 <$> getRandomR(3, w `div` 2 - 3) <*> getRandomR (3, h `div` 2 - 3)
    pure $ v ^* 2
  pure . nub . flip map vectors $ \v ->
    let v' = fromIntegralVector v
    in Zigzag v' (v' ^+^ V2 1 1.5) (v' ^+^ V2 1.5 3) (v' ^+^ V2 3 4.5) (v' ^+^ V2 4.5 3) (v' ^+^ V2 3 1)


genTriGrid :: Generate [Tri]
genTriGrid = do
  (w, h) <- getSize @Int
  vectors <- replicateM 800 $ do
    v <- V2 <$> getRandomR (3, w `div` 2 - 3) <*> getRandomR (3, h `div` 2 - 3)
    pure $ v ^* 2
  pure . nub . flip map vectors $ \v ->
    let v' = fromIntegralVector v
    -- Originally 0 1.5 1.5 0
    in Tri v' (v' ^+^ V2 0 2) (v' ^+^ V2 2 0)


renderClosedPath :: [V2 Double] -> Render()
renderClosedPath (V2 x y:vs) = do
  newPath
  moveTo x y
  for_ vs $ \v -> let V2 x' y' = v in lineTo x' y'
  closePath
renderClosedPath [] = pure ()

renderQuad :: Quad -> Render ()
renderQuad Quad{..} = renderClosedPath [quadA, quadB, quadC, quadD]

renderTri :: Tri -> Render ()
renderTri Tri{..} = renderClosedPath [triA, triB, triC]

renderZigzag :: Zigzag -> Render ()
renderZigzag Zigzag{..} = renderClosedPath [zigA, zigB, zigC, zigD, zigE, zigF]

darkGunmetal :: Double -> Render()
darkGunmetal = hsva 170 0.30 0.16

--original values: noise / 5, noise / 8, - 0.5
quadAddNoise :: Quad -> Generate Quad
quadAddNoise Quad{..} = do
  perlinSeed <- fromIntegral <$> asks worldSeed

  let
    perlinOctaves = 5
    perlinScale = 0.1
    perlinPersistance = 0.5
    perlinNoise
      = P.perlin (round perlinSeed) perlinOctaves perlinScale perlinPersistance
    perlin2d (V2 x y)
      = P.noiseValue perlinNoise (x + perlinSeed, y + perlinSeed, perlinSeed) - 0.5
    addNoise v = let noise = perlin2d v in v ^+^ V2 (noise / 5) (noise / 8)

  pure $ Quad
    (addNoise quadA)
    (addNoise quadB)
    (addNoise quadC)
    (addNoise quadD)

triAddNoise :: Tri -> Generate Tri
triAddNoise Tri{..} = do
    perlinSeed <- fromIntegral <$> asks worldSeed

    let
      perlinOctaves = 5
      perlinScale = 0.1
      perlinPersistance = 0.5
      perlinNoise
        = P.perlin (round perlinSeed) perlinOctaves perlinScale perlinPersistance
      perlin2d (V2 x y)
        = P.noiseValue perlinNoise (x + perlinSeed, y + perlinSeed, perlinSeed) - 0.5
      addNoise v = let noise = perlin2d v in v ^+^ V2 (noise / 5) (noise / 8)

    pure $ Tri
      (addNoise triA)
      (addNoise triB)
      (addNoise triC)
