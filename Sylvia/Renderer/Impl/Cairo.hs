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
import Data.Monoid
import Graphics.Rendering.Cairo

import Sylvia.Renderer.Impl
import Sylvia.Renderer.Pair

newtype Image = I { unI :: ImageM () }

type ImageM = ReaderT Context Render

runImage :: Image -> Context -> Render ()
runImage (I action) = runReaderT action

data Context = C
    { ctxGridSize :: PInt
    , ctxOffset   :: PInt
    }

-- | Map a relative coordinate to an absolute one, scaling and shifting
-- it in the process.
getAbsolute :: PInt -> ImageM PDouble
getAbsolute pair = flip fmap ask $
    \(C gridSize offset) -> fromIntegralP $ (pair |+| offset) |*| gridSize

instance Monoid Image where
    mempty = I $ return ()
    I a `mappend` I b = I $ a >> b

instance RenderImpl Image where
    drawLine src dest = I $ do
        x1 :| y1 <- (|+| (0.5 :| 0.5)) <$> getAbsolute src
        x2 :| y2 <- (|+| (0.5 :| 0.5)) <$> getAbsolute dest
        lift $ do
            newPath
            moveTo x1 y1
            lineTo x2 y2
            setLineWidth 1
            stroke

    relativeTo delta = I . local addOffset . unI
      where
        addOffset ctx@C{ ctxOffset = offset }
          = ctx{ ctxOffset = offset |+| delta }

dumpPNG :: Int -> Int -> Image -> IO ()
dumpPNG w h action = withImageSurface FormatRGB24 w h $ \surface -> do
    -- Fill the background with white
    renderWith surface $ setSourceRGB 1 1 1 >> paint
    -- Render ALL the things
    renderWith surface $ runImage action (C (20 :| 10) (0 :| 0))
    -- Save the image
    surfaceWriteToPNG surface "result.png"

testRhyme :: IO ()
testRhyme = dumpPNG 260 100 $ mconcat
    [ relativeTo (1 :| 8) $ renderRhyme' [5,4,3,2,1]
    , relativeTo (3 :| 8) $ renderRhyme' [4,3,2,1,0]
    , relativeTo (5 :| 8) $ renderRhyme' [0,1,2,3,4]
    , relativeTo (7 :| 8) $ renderRhyme' [0,0]
    , relativeTo (9 :| 8) $ renderRhyme' [1,0]
    , relativeTo (11 :| 8) $ renderRhyme' [1]
    ]
