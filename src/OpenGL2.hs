module Main where

import Graphics.QML

import Paths_hsqml_demo_samples

main :: IO ()
main = do
    clazz <- newClass []
    ctx <- newObject clazz ()
    doc <- getDataFileName "opengl2.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument doc,
        contextObject = Just $ anyObjRef ctx}
