module Main where

import Graphics.GL.ARB.ShaderObjects (glUniformMatrix4fvARB)
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
    "uniform highp mat4 matrix;",
    "uniform highp float model;",
    "varying highp vec4 vColor;",
    "void main() {",
    "    gl_Position = matrix * position;",
    "    vColor = mix(color, vec4(1,1,1,2) - color, model);",
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
    Program Shader Shader UniformLocation UniformLocation
    AttribLocation AttribLocation BufferObject

checkErrors :: String -> IO ()
checkErrors title = do
    errs <- get errors
    if null errs then return () else putStrLn $ title ++ ": " ++ show errs

setupGL :: OpenGLSetup -> IO GLData
setupGL setup = do
    putStrLn $ (showString "Context is " .
        shows (openGLType setup) . showString " " .
        shows (openGLMajor setup) . showString "." .
        shows (openGLMinor setup)) ""
    let ctype = openGLType setup
    vertShader <- createShader VertexShader
    shaderSourceBS vertShader $= (T.encodeUtf8 $ vertShaderText ctype)
    compileShader vertShader
    -- vsl <- get $ shaderInfoLog vertShader
    -- putStrLn $ show vsl
    fragShader <- createShader FragmentShader
    shaderSourceBS fragShader $= (T.encodeUtf8 $ fragShaderText ctype)
    compileShader fragShader
    -- fsl <- get $ shaderInfoLog fragShader
    -- putStrLn $ show fsl
    program <- createProgram
    attachShader program vertShader
    attachShader program fragShader
    linkProgram program
    -- pl <- get $ programInfoLog program
    -- putStrLn $ show pl
    matLoc <- get $ uniformLocation program "matrix"
    modelLoc <- get $ uniformLocation program "model"
    posLoc <- get $ attribLocation program "position"
    colLoc <- get $ attribLocation program "color"
    buf <- genObjectName
    bindBuffer ArrayBuffer $= Just buf
    withArrayLen dataArray $ \len ptr ->
        bufferData ArrayBuffer $=
            (fromIntegral $ len * sizeOf (head dataArray),
             ptr, StaticDraw)
    bindBuffer ArrayBuffer $= Nothing
    checkErrors "Setup"
    return $ GLData
        program vertShader fragShader matLoc modelLoc posLoc colLoc buf

paintGL :: OpenGLPaint GLData Double -> IO ()
paintGL paint = do
    let (GLData program _ _ matLoc modelLoc posLoc colLoc buf) = setupData paint
    let num = realToFrac $ modelData paint :: GLfloat
    currentProgram $= Just program
    let (UniformLocation matLocId) = matLoc
    glUniformMatrix4fvARB matLocId 1 0 (
        castPtr $ matrixPtr paint :: Ptr GLfloat)
    uniform modelLoc $= Index1 num
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
    checkErrors "Paint"

cleanupGL :: GLData -> IO ()
cleanupGL (GLData program vertShader fragShader _ _ _ _ buf) = do
    deleteObjectName buf
    deleteObjectName program
    deleteObjectName vertShader
    deleteObjectName fragShader
    checkErrors "Cleanup"

main :: IO ()
main = do
    clazz <- newClass [
        defPropertyConst' "myDelegate" (\_ ->
            newOpenGLDelegate setupGL paintGL cleanupGL)]
    ctx <- newObject clazz ()
    doc <- getDataFileName "opengl1.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument doc,
        contextObject = Just $ anyObjRef ctx}
