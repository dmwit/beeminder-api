{-

Copyright (c) 2012 Bryan O'Sullivan, (c) 2011 MailRank, Inc.

MODIFIED by Brent Yorgey, February 2014.

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. Neither the name of the author nor the names of his contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

-}

{-# LANGUAGE CPP, BangPatterns, OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aeson.Encode.Shim
-- Copyright   :  (c) 2012 Bryan O'Sullivan
--                (c) 2011 MailRank, Inc.
--                (c) 2014 Brent Yorgey
-- License     :  BSD3
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
--
-- Beeminder's JSON parser does not accept floating-point values in
-- scientific format (even though that format is allowed by the JSON
-- standard).  To work around this, we use a slightly modified variant
-- of aeson's encoding which never uses scientific notation for
-- encoded floating-point values (the modification is marked by !!!!).
--
-----------------------------------------------------------------------------

module Data.Aeson.Encode.Shim (encode) where

import Data.Aeson.Types (Value(..))
import Data.Monoid (mappend)
import Data.Scientific (Scientific, coefficient, base10Exponent, FPFormat(Fixed))
import Data.Text.Lazy.Builder.Scientific (formatScientificBuilder)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Numeric (showHex)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

#if MIN_VERSION_bytestring(0,10,4)
import Data.Aeson.Encode (encode, encodeToTextBuilder)
#else
import Data.Aeson.Types (ToJSON(toJSON))
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text.Lazy.Builder  as TLB
import qualified Data.Text.Lazy.Encoding as TLE

-- | Encode a JSON 'Value' as a UTF-8 encoded 'BL.ByteString'.
encode :: ToJSON a => a -> BL.ByteString
encode = TLE.encodeUtf8 . TLB.toLazyText . encodeToTextBuilder . toJSON
#endif

-- | Encode a JSON 'Value' to a 'Builder', which can be embedded efficiently
-- in a text-based protocol.
encodeToTextBuilder :: Value -> Builder
encodeToTextBuilder =
    go
  where
    go Null       = {-# SCC "go/Null" #-} "null"
    go (Bool b)   = {-# SCC "go/Bool" #-} if b then "true" else "false"
    go (Number s) = {-# SCC "go/Number" #-} fromScientific s
    go (String s) = {-# SCC "go/String" #-} string s
    go (Array v)
        | V.null v = {-# SCC "go/Array" #-} "[]"
        | otherwise = {-# SCC "go/Array" #-}
                      singleton '[' <>
                      go (V.unsafeHead v) <>
                      V.foldr f (singleton ']') (V.unsafeTail v)
      where f a z = singleton ',' <> go a <> z
    go (Object m) = {-# SCC "go/Object" #-}
        case H.toList m of
          (x:xs) -> singleton '{' <> one x <> foldr f (singleton '}') xs
          _      -> "{}"
      where f a z     = singleton ',' <> one a <> z
            one (k,v) = string k <> singleton ':' <> go v

string :: T.Text -> Builder
string s = {-# SCC "string" #-} singleton '"' <> quote s <> singleton '"'
  where
    quote q = case T.uncons t of
                Nothing      -> fromText h
                Just (!c,t') -> fromText h <> escape c <> quote t'
        where (h,t) = {-# SCC "break" #-} T.break isEscape q
    isEscape c = c == '\"' ||
                 c == '\\' ||
                 c == '<'  ||
                 c == '>'  ||
                 c < '\x20'
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"

    -- The following prevents untrusted JSON strings containing </script> or -->
    -- from causing an XSS vulnerability:
    escape '<'  = "\\u003c"
    escape '>'  = "\\u003e"

    escape c
        | c < '\x20' = fromString $ "\\u" ++ replicate (4 - length h) '0' ++ h
        | otherwise  = singleton c
        where h = showHex (fromEnum c) ""

fromScientific :: Scientific -> Builder
fromScientific s
    | e < 0     = formatScientificBuilder Fixed Nothing s   -- !!!!
    | otherwise = decimal (coefficient s * 10 ^ e)
  where
    e = base10Exponent s

(<>) :: Builder -> Builder -> Builder
(<>) = mappend
{-# INLINE (<>) #-}
infixr 6 <>
