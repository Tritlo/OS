import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT

import           System.Random
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.ForeignPtr

import           Data.List
import           Data.Ord
import           Data.Word
import           Data.Char

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import Codec.Picture


genLineCirc ::  GLfloat -> [(GLfloat,GLfloat,GLfloat)]
genLineCirc s = map (\k -> (s*sin(2*pi*k/24),s*cos(2*pi*k/24),0.0)) [1..24]

circ :: [(GLfloat,GLfloat,GLfloat)]
circ = genLineCirc 1

main :: IO()
main = do
    (progname, _) <- getArgsAndInitialize
    createWindow "hello, world"
    displayCallback $= display
    mainLoop
    
renderCirc :: PrimitiveMode -> [(GLfloat,GLfloat,GLfloat)] -> IO()
renderCirc mode circ =  renderPrimitive mode $ mapM_ (\(x, y, z)-> vertex $ Vertex3 x y z) circ
renderCircLine circ = renderCirc  LineLoop circ

splitNum :: StdGen -> Float -> (Float, Float, StdGen)
splitNum gen num =
  let (value, newgen) = random gen :: (Float,StdGen)
  in  (value *num, (1-value) * num, newgen)

maxAndInd :: (Ord a) => [a] -> (a,Int)
maxAndInd xs = maximumBy (comparing fst) (zip xs [0..])

splitAtMax :: (Ord a) => [a] -> ([a],[a])
splitAtMax xs = splitAt (snd $ maxAndInd xs) xs

splitMaxNum :: StdGen -> [Float] -> ([Float], StdGen)
splitMaxNum gen xs =
  (js ++ [a,b] ++ ks, newgen)
  where (js,k:ks) = splitAtMax xs
        (a,b,newgen) = splitNum gen k


genProbSpace :: StdGen -> Int -> ([Float], StdGen)
genProbSpace gen n = genProbSpace' gen n [1]

genProbSpace' :: StdGen -> Int -> [Float] -> ([Float],StdGen)
genProbSpace' gen 0 xs = (xs,gen)
genProbSpace' gen n xs =
  genProbSpace' newgen (n-1) ys
  where (ys,newgen) = splitMaxNum gen xs

partialSumList :: (Num a) => [a] -> [a]
partialSumList (x:xs) = partialSumList' xs [x]

partialSumList' :: (Num a) => [a] -> [a] -> [a]
partialSumList' [] ys = ys
partialSumList' (x:xs) (y:ys) = partialSumList' xs ((x+y):y:ys)

hexToFloat :: [Char] -> GLfloat
hexToFloat (a:b:[]) = (realToFrac (16 * (digitToInt a) + (digitToInt b)) )/255

hexStringToFloat :: String -> (GLfloat,GLfloat,GLfloat)
hexStringToFloat (x:xs) =
  (hexToFloat a , hexToFloat b, hexToFloat c)
  where (a,as) = splitAt 2 xs
        (b, c) = splitAt 2 as

hexStrToColor :: String -> Color3 GLfloat
hexStrToColor xs =
  Color3 a b c
  where (a,b,c) = hexStringToFloat xs

format :: Ptr Word8 -> PixelData Word8
format ptr = PixelData RGBA UnsignedByte ptr

{-
 - Skilar Base64 encodeaðri mynd
 - af því sem er í buffernum núna
 - @TODO: Finna út hvernig 
 - hægt er að sleppa því að prenta á skjáinn
 -}
printPixels :: Position -> Size -> IO (BS.ByteString)
-- @ merkið þýðir að við bindum siz, en við deconstructum það í leiðinni
printPixels pos siz@(Size vw vh) = do 
  let sib = fromIntegral (vw*vh*4)
      fi q = fromIntegral q
  ptr <- mallocBytes sib
  readPixels pos siz (format ptr)
  fpn <- newForeignPtr_ ptr
  let img = (imageFromUnsafePtr (fi vw) (fi vh) fpn) :: (Image PixelRGBA8)
  --writePng "ok.png" img
  free ptr
  return $ B64.encode $ BSL.toStrict $  encodePng img

display :: IO() 
display = do
  clear [ColorBuffer]
  color $ hexStrToColor "#deb887"
  renderCirc Polygon (genLineCirc 1.0) 
  color $ hexStrToColor "#8b7355"
  genSeed <- randomIO :: IO Int
  mapM_ (\s -> renderCircLine (genLineCirc (realToFrac s))) $ partialSumList $ fst $ genProbSpace (mkStdGen genSeed) 10
  bs <- printPixels (Position 0 0) (Size 200 200)
  BS.putStrLn bs
  flush
