{-# LANGUAGE OverloadedStrings #-}

import Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Base16 as B16

input :: B.ByteString
input = "yzbqklnj"

check :: Int -> Bool
check = (=="000000") . B.take 6 . B16.encode . hash . (input<>) . B.pack . show

main = print . head . filter check $ [1..]
