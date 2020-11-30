{-# LANGUAGE ForeignFunctionInterface #-}

module FFI
  ( getTime
  , sin
  , add
  , getPi
  , getNegPi
  , negAdd
  )
where

import           Foreign.C
import           Foreign.Ptr                    ( Ptr
                                                , nullPtr
                                                )
import           Prelude                 hiding ( sin )

-- |A pure stdlib foreign function
foreign import ccall "sin" c_sin :: CDouble -> CDouble
sin :: Double -> Double
sin d = realToFrac (c_sin (realToFrac d))

-- |An impure stdlib function
foreign import ccall "time" c_time :: Ptr a -> IO CTime
getTime :: IO Int
getTime = c_time nullPtr >>= (return . fromEnum)

-- |A pure custom function
foreign import ccall "add" c_add :: CInt -> CInt -> CInt
add :: Int -> Int -> Int
add x y = fromIntegral $ c_add (fromIntegral x) (fromIntegral y)

-- |Another pure custom function
foreign import ccall "get_pi" c_get_pi :: CDouble
getPi :: Double
getPi = realToFrac c_get_pi

-- |Another pure custom function demonstrating the usage of multiple C sources
foreign import ccall "get_neg_pi" c_get_neg_pi :: CDouble
getNegPi :: Double
getNegPi = realToFrac c_get_neg_pi

-- |Another pure custom function demonstrating the usage of multiple C sources
foreign import ccall "neg_add" c_neg_add :: CInt -> CInt -> CInt
negAdd :: Int -> Int -> Int
negAdd x y = fromIntegral $ c_neg_add (toEnum x) (toEnum y)
