module Main
( main
) where

import Haste.Prim
import Haste.Foreign

foreign import ccall "alert" alert2 :: JSString -> IO ()

alert3 :: JSString -> IO ()
alert3 = ffi "(function (x) { alert(x); })"

main :: IO ()
main = do
    alert2 $ toJSStr "hello bayhac 2"
    alert3 $ toJSStr "hello bayhac 3"
