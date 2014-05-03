module Main where

import Graphics.QML
import Data.Text (Text)
import qualified Data.Text as T

import Paths_hsqml_demo_samples

main :: IO ()
main = do
    clazz <- newClass [
        defMethod' "factorial" (\_ txt ->
            let n = read $ T.unpack txt :: Integer
            in return . T.pack . show $ product [1..n] :: IO Text)]
    ctx <- newObject clazz ()
    doc <- getDataFileName "factorial1.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument doc,
        contextObject = Just $ anyObjRef ctx}
