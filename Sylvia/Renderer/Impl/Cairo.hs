{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Sylvia.Renderer.Impl.Cairo
-- Copyright   : GPLv3
--
-- Maintainer  : chrisyco@gmail.com
-- Portability : non-portable (requires FFI)
--
-- Renderer using the Cairo graphics library.

module Sylvia.Renderer.Impl.Cairo
    (
    ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Graphics.Rendering.Cairo

import Sylvia.Renderer.Core
import Sylvia.Renderer.Impl
import Sylvia.Renderer.Pair

type Cairo = ReaderT Context Render

data Context = C
    { ctxGridSize :: PInt
    , ctxOffset   :: PInt
    }

-- | Map a relative coordinate to an absolute one, scaling and shifting
-- it in the process.
getAbsolute :: PInt -> Cairo PDouble
getAbsolute pair = flip fmap ask $
    \(C gridSize offset) -> fromIntegralP $ (pair |+| offset) |*| gridSize

instance RenderImpl Cairo where
    drawLine src dest = do
        x1 :| y1 <- (|+| (0.5 :| 0.5)) <$> getAbsolute src
        x2 :| y2 <- (|+| (0.5 :| 0.5)) <$> getAbsolute dest
        lift $ do
            newPath
            moveTo x1 y1
            lineTo x2 y2
            setLineWidth 1
            stroke

    drawDot center = do
        cx :| cy <- getAbsolute center
        radius <- asks (fromIntegral . (`div` 2) . sndP . ctxGridSize)
        lift $ do
            newPath
            arc cx cy radius 0 (2 * pi)
            setSourceRGB 0 0 0
            fill

    relativeTo delta = local $
        \ctx@C{ ctxOffset = offset } -> ctx{ ctxOffset = offset |+| delta }

dumpPNG :: Int -> Int -> Cairo a -> IO a
dumpPNG w h action = withImageSurface FormatRGB24 w h $ \surface -> do
    -- Fill the background with white
    renderWith surface $ setSourceRGB 1 1 1 >> paint
    -- Render ALL the things
    result <- renderWith surface $ runReaderT action (C (20 :| 10) (0 :| 0))
    -- Save the image
    surfaceWriteToPNG surface "result.png"
    return result

testRhyme :: IO ()
testRhyme = dumpPNG 260 100 $ do
    relativeTo (1 :| 8) $ renderRhyme [5,4,3,2,1]
    relativeTo (3 :| 8) $ renderRhyme [4,3,2,1,0]
    relativeTo (5 :| 8) $ renderRhyme [0,1,2,3,4]
    relativeTo (7 :| 8) $ renderRhyme [0,0]
    relativeTo (9 :| 8) $ renderRhyme [1,0]
    relativeTo (11 :| 8) $ renderRhyme [1]

testRhythm :: IO ()
testRhythm = dumpPNG 500 100 $ do
    relativeTo (1 :| 8) $ renderRhythm (replicate 5 0) [0,1,2,3]

testAll :: IO ()
testAll = dumpPNG 500 100 $ do
    -- random numbers
    relativeTo (1 :| 8) $ renderAll $ RS
                            { rhyme = [0,1,2,3,4]
                            , rhythm = [0,3,1,2]
                            }
    -- mockingbird
    relativeTo (8 :| 8) $ renderAll $ RS
                            { rhyme = [0,0]
                            , rhythm = [0]
                            }
    -- bluebird
    relativeTo (12 :| 8) $ renderAll $ RS
                            { rhyme = [2,1,0]
                            , rhythm = [1,0]
                            }
    -- warbler
    relativeTo (17 :| 8) $ renderAll $ RS
                            { rhyme = [1,0,0]
                            , rhythm = [0,1]
                            }
