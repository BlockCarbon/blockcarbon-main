
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils
  ( 
    integerToBS,
  )
where

import PlutusTx.Prelude



minusAsciiCode :: Integer
minusAsciiCode = 45

zeroAsciiCode :: Integer
zeroAsciiCode = 48


{-# INLINEABLE integerToBS #-}
integerToBS :: Integer -> BuiltinByteString
integerToBS x
  | x < 0 = consByteString minusAsciiCode $ integerToBS (negate x)
  -- x is single-digit
  | x `quotient` 10 == 0 = digitToBS x
  | otherwise = integerToBS (x `quotient` 10) <> digitToBS (x `remainder` 10)
  where
    digitToBS :: Integer -> BuiltinByteString
    digitToBS d = consByteString (d + zeroAsciiCode) emptyByteString