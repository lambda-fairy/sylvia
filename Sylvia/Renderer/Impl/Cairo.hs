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
      testRender
    ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Monoid
import Graphics.Rendering.Cairo

import Sylvia.Model
import Sylvia.Renderer.Impl
import Sylvia.Renderer.Pair

newtype Image = I { unI :: ImageM () }

type ImageM = ReaderT Context Render

runImage :: Image -> Context -> Render ()
runImage (I action) = runReaderT action

-- | Lift a rendering action into the 'ImageM' monad, wrapping it in
-- calls to 'save' and 'restore' to stop its internal state from leaking
-- out.
cairo :: Render a -> ImageM a
cairo action = lift $ do
    save
    result <- action
    restore
    return result

data Context = C
    { ctxGridSize :: PInt
    , ctxOffset   :: PInt
    }

-- | Map a relative coordinate to an absolute one, scaling and shifting
-- it in the process.
getAbsolute :: PInt -> ImageM PDouble
getAbsolute pair = getRelative =<< (pair |+|) <$> asks ctxOffset

-- | Scale a relative coordinate.
getRelative :: PInt -> ImageM PDouble
getRelative pair = fromIntegralP . (pair |*|) <$> asks ctxGridSize

-- | Add a half-pixel offset. This can make lines noticeably sharper, by
-- aligning points to the pixel grid.
addHalf :: PDouble -> PDouble
addHalf = (|+| (0.5 :| 0.5))

instance Monoid Image where
    mempty = I $ return ()
    I a `mappend` I b = I $ a >> b

instance RenderImpl Image where
    drawDottedRectangle corner size = I $ do
        x  :| y  <- addHalf <$> getAbsolute corner
        dx :| dy <- getRelative size
        cairo $ do
            newPath
            rectangle x y dx dy
            setDash [1, 1] 0.5
            setLineWidth 1
            stroke

    drawLine src dest = I $ do
        x1 :| y1 <- addHalf <$> getAbsolute src
        x2 :| y2 <- addHalf <$> getAbsolute dest
        cairo $ do
            newPath
            moveTo x1 y1
            lineTo x2 y2
            setLineWidth 1
            stroke

    drawCircleSegment center start end = I $ do
        cx :| cy <- getAbsolute center
        -- A dot's diameter is approximately equal to one vertical grid unit
        radius <- asks (fromIntegral . (`div` 2) . sndP . ctxGridSize)
        cairo $ do
            newPath
            arc cx cy radius start end
            setSourceRGB 0 0 0
            fill

    relativeTo delta = I . local addOffset . unI
      where
        addOffset ctx@C{ ctxOffset = offset }
          = ctx{ ctxOffset = offset |+| delta }

dumpPNG :: PInt -> Image -> IO ()
dumpPNG size action = withImageSurface FormatRGB24 w h $ \surface -> do
    -- Fill the background with white
    renderWith surface $ setSourceRGB 1 1 1 >> paint
    -- Render ALL the things
    let action' = relativeTo (1 :| 1) action
    renderWith surface $ runImage action' (C defaultGridSize (0 :| 0))
    -- Save the image
    surfaceWriteToPNG surface "result.png"
  where
    defaultGridSize = 20 :| 10
    w :| h = defaultGridSize |*| (size |+| (2 :| 2)) -- padding

testRender :: IO ()
testRender = uncurry dumpPNG $ foldl step (0 :| 0, mempty) es
  where
    step ((w :| h), image) e = ((w + w' + 1) :| (max h h'), image <> relativeTo ((w + w') :| h') image')
      where Result image' (w' :| h') _ _ = renderRhythm e
    es  = (Ref 2 .$. Ref 0) .$. (Ref 1 .$. Ref 0) .$. (Ref 3 .$. Ref 0)
        : Ref 0 .$. (Lam $ Lam (App (Ref (S O)) (Ref (S (S 0))))) .$. Ref 0
        : []
    (.$.) = App
