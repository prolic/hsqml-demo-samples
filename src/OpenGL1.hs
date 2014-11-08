module Main where

import Graphics.QML
import Graphics.QML.Canvas
import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GLU.Errors
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Paths_hsqml_demo_samples

shaderHead :: OpenGLType -> [String]
shaderHead OpenGLDesktop = ["#version 120", "#define highp"]
shaderHead OpenGLES = ["#version 100"]

vertShaderText :: OpenGLType -> Text
vertShaderText t = T.pack $ unlines $ shaderHead t ++ [
    "attribute highp vec4 position;",
    "attribute highp vec4 color;",
    "varying highp vec4 vColor;",
    "void main() {",
    "    gl_Position = position;",
    "    vColor = color;",
    "}"]

fragShaderText :: OpenGLType -> Text
fragShaderText t = T.pack $ unlines $ shaderHead t ++ [
    "varying highp vec4 vColor;",
    "void main() {",
    "    gl_FragColor = vColor;",
    "}"]

dataArray :: [CFloat]
dataArray = [
  0.0, 1.0, 0.0, 1.0,
  -1.0, -1.0, 0.0, 1.0,
  1.0, -1.0, 0.0, 1.0,
  1.0, 0.0, 0.0, 1.0,
  0.0, 1.0, 0.0, 1.0,
  0.0, 0.0, 1.0, 1.0]

data GLData = GLData
    Program AttribLocation AttribLocation BufferObject

initGL :: OpenGLSetup -> IO GLData
initGL setup = do
    let ctype = openGLType setup
    vertShader <- createShader VertexShader
    shaderSourceBS vertShader $= (T.encodeUtf8 $ vertShaderText ctype)
    compileShader vertShader
    -- vsl <- get $ shaderInfoLog vertShader
    -- putStrLn vsl
    fragShader <- createShader FragmentShader
    shaderSourceBS fragShader $= (T.encodeUtf8 $ fragShaderText ctype)
    compileShader fragShader
    -- fsl <- get $ shaderInfoLog fragShader
    -- putStrLn fsl
    program <- createProgram
    attachShader program vertShader
    attachShader program fragShader
    linkProgram program
    -- pl <- get $ programInfoLog program
    -- putStrLn pl
    posLoc <- get $ attribLocation program "position"
    colLoc <- get $ attribLocation program "color"
    buf <- genObjectName
    bindBuffer ArrayBuffer $= Just buf
    withArrayLen dataArray $ \len ptr ->
        bufferData ArrayBuffer $=
            (fromIntegral $ len * sizeOf (head dataArray),
             ptr, StaticDraw)
    bindBuffer ArrayBuffer $= Nothing
    return $ GLData program posLoc colLoc buf

paintGL :: OpenGLPaint GLData Double -> IO ()
paintGL paint = do
    let (GLData program posLoc colLoc buf) = setupData paint
    let num = realToFrac $ modelData paint
    viewport $= (Position 0 0, Size 250 250)
    clearColor $= Color4 num (1-num) (1-num) 1
    clear [ColorBuffer, DepthBuffer]
    currentProgram $= Just program
    bindBuffer ArrayBuffer $= Just buf
    vertexAttribArray posLoc $= Enabled
    vertexAttribPointer posLoc $=
        (ToFloat, VertexArrayDescriptor 4 Float 0 nullPtr)
    vertexAttribArray colLoc $= Enabled
    vertexAttribPointer colLoc $=
        (ToFloat, VertexArrayDescriptor 4 Float 0 $ plusPtr nullPtr $
            length dataArray * sizeOf (head dataArray) `quot` 2)
    drawArrays Triangles 0 3
    vertexAttribArray posLoc $= Disabled
    vertexAttribArray colLoc $= Disabled
    bindBuffer ArrayBuffer $= Nothing
    currentProgram $= Nothing

deinitGL :: GLData -> IO ()
deinitGL (GLData program _ _ buf) = do
    deleteObjectName buf
    deleteObjectName program

main :: IO ()
main = do
    clazz <- newClass [
        defPropertyConst' "myDelegate" (\_ ->
            newOpenGLDelegate initGL paintGL deinitGL)]
    ctx <- newObject clazz ()
    doc <- getDataFileName "opengl1.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument doc,
        contextObject = Just $ anyObjRef ctx}
