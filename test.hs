{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DataKinds            #-}

import Graphics.Identicon
import Graphics.Identicon.Styles
import Crypto.Hash.MD5
import Codec.Picture
import Data.ByteString (ByteString)
import Data.Proxy

genIcon
  :: Int               -- ^ Desired width
  -> Int               -- ^ Desired height
  -> ByteString        -- ^ Input (some sort of hash)
  -> Maybe (Image PixelRGB8) -- ^ Resulting image
genIcon = renderIdenticon (Proxy :: Proxy (GeneralizedGithub 2))
        $ generalizedGithubStyle (Proxy :: Proxy 2)


main :: IO ()
main = do
  let h = hash "identicon"
  let Just img = genIcon 200 200 h
  putStrLn "salvando..."
  writePng "image.png" img

