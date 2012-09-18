{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- |
-- Module      : Sylvia.UI.GTK
-- Copyright   : GPLv3
--
-- Maintainer  : chrisyco@gmail.com
-- Portability : portable
--
-- Graphical interface based on Cairo and GTK.

module Sylvia.UI.GTK ( showInWindow ) where

import Data.Default
import Data.Void
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import System.IO ( hPutStr, hPutStrLn, stderr )

import Sylvia.Model
import Sylvia.Renderer.Pair
import Sylvia.Renderer.Impl.Cairo

title :: String
title = "Sylvia"

showInWindow :: Exp Void -> IO ()
showInWindow e = do
    "Initializing GTK" -:- do
        initGUI

    window <- "Creating window" -:- do
        window <- windowNew
        set window
            [ windowTitle := title
            , widgetAppPaintable := True
            ]
        onDestroy window mainQuit
        return window

    "Creating canvas" -:- do
        canvas <- drawingAreaNew
        let (_, (w :| h)) = renderCairo e
        widgetSetSizeRequest canvas w h
        set window [ containerChild := canvas ]
        canvas `on` exposeEvent $ updateCanvas e

    "Show ALL the things!" -:- do
        widgetShowAll window
        mainGUI

updateCanvas :: Exp Void -> EventM EExpose Bool
updateCanvas e = do
    win <- eventWindow
    liftIO $ do
        let (action, _) = renderCairo e
        renderWithDrawable win action
    return True

renderCairo :: Exp Void -> (Render (), PInt)
renderCairo = runImageWithPadding def . render

infixr 1 -:-
(-:-) :: String -> IO a -> IO a
msg -:- action = do
    hPutStr stderr msg
    result <- action
    hPutStrLn stderr " ... done"
    return result
