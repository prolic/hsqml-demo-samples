module Main where

import Graphics.QML
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

import Paths_hsqml_demo_samples

main :: IO ()
main = do
    clazz <- newClass [
        defMethod' "factorial" (\_ txt -> do
            let maybeN = readMaybe (T.unpack txt) :: Maybe Integer
            case maybeN of
                Nothing -> do
                    return (T.pack "0")
                Just n -> do
                    let result = product [1..n]
                    return (T.pack (show result)) :: IO Text)]
    ctx <- newObject clazz ()
    doc <- getDataFileName "factorial1.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument doc,
        contextObject = Just $ anyObjRef ctx}
