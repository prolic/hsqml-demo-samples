module Main where

import Graphics.QML

import Paths_hsqml_demo_samples

main :: IO ()
main = do
    doc <- getDataFileName "model1.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument doc}
