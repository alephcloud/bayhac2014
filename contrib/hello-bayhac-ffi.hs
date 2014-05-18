module Main
( main
) where

import Haste.Prim
import Haste.Foreign

foreign import ccall "alert" alert2 :: JSString -> IO ()

alert3 :: JSString -> IO Int
alert3 = ffi "(function (x) { alert(x); return 0; })"

foreign import ccall "setTimeout" timeout :: JSFun (IO ()) -> Int -> IO ()

main :: IO ()
main = do
    alert2 $ toJSStr "hello bayhac 2"
    alert3 $ toJSStr "hello bayhac 3"
    flip timeout 2000 . mkCallback . alert2 . toJSStr $ "hello bayhac 4"



