{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies
  #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import Graphics.QML
import Graphics.QML.Canvas
import Graphics.Rendering.OpenGL.GL

import Paths_hsqml_demo_samples

data GLInfo = GLInfo {
    vendorText :: Text,
    rendererText :: Text,
    versionText :: Text
}

main :: IO ()
main = do
    info <- newIORef $ GLInfo (T.pack "-") (T.pack "-") (T.pack "-")
    skey <- newSignalKey
    let defInfoProp name field = defPropertySigRO' name skey (\_ ->
            fmap field $ readIORef info)
    clazz <- newClass [
        defPropertyConst' "glDelegate" (\this -> newOpenGLDelegate
            (\paint -> do
                vend <- get vendor
                rend <- get renderer
                ver <- get glVersion
                writeIORef info $
                    GLInfo (T.pack vend) (T.pack rend) (T.pack ver)
                fireSignal skey this)
            (\(_ :: OpenGLPaint' ()) -> return ())
            (\_ -> return ())),
        defInfoProp "vendor" vendorText,
        defInfoProp "renderer" rendererText,
        defInfoProp "version" versionText]
    ctx <- newObject clazz ()
    doc <- getDataFileName "opengl2.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument doc,
        contextObject = Just $ anyObjRef ctx}
