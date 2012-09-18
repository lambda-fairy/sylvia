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
    -- * Types
      Image
    , runImage
    , runImageWithPadding
    , Context(..)

    -- * Testing
    , testRender

    -- * Re-exports
    , module Sylvia.Renderer.Impl
    ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Default
import Data.Monoid ( Monoid(..), (<>) )
import Graphics.Rendering.Cairo

import Sylvia.Renderer.Impl
import Sylvia.Renderer.Pair
import Sylvia.Text.Parser

newtype Image = I { unI :: ImageM () }

type ImageM = ReaderT Context Render

-- | Render an image.
--
-- The resulting image's throat will be at (0, 0); in practice, this will
-- mean that if the result is run directly, most of the image would be
-- off the page. To fix this, use the 'translate' function or call
-- 'runImageWithPadding' instead.
runImage :: Context -> Image -> Render ()
runImage ctx = flip runReaderT ctx . unI

-- | Render an image, translating it so its top-left corner is at (0, 0).
--
-- If you only want to render the thing, this is the function to use.
runImageWithPadding
    :: Context
    -> (Image, PInt)     -- The image, along with its size in grid units
    -> (Render (), PInt) -- Rendering action, with its size in pixels
runImageWithPadding ctx (image, innerSize) = (action, realSize)
  where
    action = runImage ctx $ relativeTo (innerSize |+| (1 :| 1)) image
    realSize = ctxGridSize ctx |*| (innerSize |+| (2 :| 2))

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

instance Default Context where
    def = C { ctxGridSize = (20 :| 10), ctxOffset = (0 :| 0) }

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

    drawZigzag src dest = I $ do
        x1 :| y1 <- addHalf <$> getAbsolute src
        x2 :| y2 <- addHalf <$> getAbsolute dest
        let xmid = (x1 + x2) / 2
        cairo $ do
            newPath
            moveTo x1 y1
            lineTo xmid y1
            lineTo xmid y2
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
    renderWith surface $ runImage def action'
    -- Save the image
    surfaceWriteToPNG surface "result.png"
  where
    w :| h = ctxGridSize def |*| (size |+| (2 :| 2)) -- padding

testRender :: IO ()
testRender = uncurry dumpPNG $ foldl step (0 :| 0, mempty) es
  where
    step ((w :| h), image) e = ((w + w' + 1) :| (max h h'), image <> relativeTo ((w + w') :| h') image')
      where (image', (w' :| h')) = render e
    es = map (fromRight . parseExp) $
        [ "L 0"
        , "LL 1"
        , "LL 0"
        , "LLL 2 0 (1 0)"
        , "(L 0 0) (L 0 0)"
        , "L (L 1 (0 0)) (L 1 (0 0))"
        ]

fromRight :: Show e => Either e a -> a
fromRight e = case e of
    Left sinister -> error $ "Unexpected Left: " ++ show sinister
    Right dextrous -> dextrous
