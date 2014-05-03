module Main where

import Graphics.QML
import Control.Concurrent
import Control.Exception
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T

import Paths_hsqml_demo_samples

main :: IO ()
main = do
    state <- newIORef $ T.pack ""
    skey <- newSignalKey
    clazz <- newClass [
        defPropertySigRO' "result" skey (\_ ->
            readIORef state),
        defMethod' "factorial" (\obj txt -> do
            let n = read $ T.unpack txt :: Integer
            writeIORef state $ T.pack "Working..."
            fireSignal skey obj
            forkIO $ do
                let out = T.take 1000 . T.pack . show $ product [1..n]
                evaluate out
                writeIORef state out
                fireSignal skey obj
            return ())]
    ctx <- newObject clazz ()
    doc <- getDataFileName "factorial2.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument doc,
        contextObject = Just $ anyObjRef ctx}
