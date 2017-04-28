{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DataKinds            #-}

import Graphics.Identicon
import qualified Graphics.Identicon.Styles as Styles
import Crypto.Hash.MD5
import Codec.Picture
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (ByteString)
import Data.Proxy
import Control.Exception.Base (assert)

genIcon
  :: Int               -- ^ Desired width
  -> Int               -- ^ Desired height
  -> ByteString        -- ^ Input (some sort of hash)
  -> Maybe (Image PixelRGB8) -- ^ Resulting image
genIcon = renderIdenticon (Proxy :: Proxy (Styles.Squares 2))
        $ Styles.squares (Proxy :: Proxy 2)


main :: IO ()
main = do
  let h = hash "identicon"
  let Just img = genIcon 200 200 h
  let ih = hash $ toStrict $ encodeBitmap img
  assert (ih == "\251\173\EM\135\209sH\136\192\234=\DC4\214\255e\195") $ return ()

