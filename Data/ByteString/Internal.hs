{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.ByteString.Internal
-- Copyright   : (c) Don Stewart 2006-2008
--               (c) Duncan Coutts 2006-2012
-- License     : BSD-style
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : unstable
-- Portability : non-portable
--
-- A module containing semi-public 'ByteString' internals. This exposes the
-- 'ByteString' representation and low level construction functions. As such
-- all the functions in this module are unsafe. The API is also not stable.
--
-- Where possible application should instead use the functions from the normal
-- public interface modules, such as "Data.ByteString.Unsafe". Packages that
-- extend the ByteString system at a low level will need to use this module.
--
module Data.ByteString.Internal (

        -- * The @ByteString@ type and representation
        ByteString
        ( BS
        , PS -- backwards compatibility shim
        ),

        StrictByteString,

        -- * Internal indexing
        findIndexOrLength,

        -- * Conversion with lists: packing and unpacking
        packBytes, packUptoLenBytes, unsafePackLenBytes,
        packChars, packUptoLenChars, unsafePackLenChars,
        unpackBytes, unpackAppendBytesLazy, unpackAppendBytesStrict,
        unpackChars, unpackAppendCharsLazy, unpackAppendCharsStrict,
        unsafePackAddress, unsafePackLenAddress,
        unsafePackLiteral, unsafePackLenLiteral,

        -- * Low level imperative construction
        empty,
        create,
        createUptoN,
        createUptoN',
        createAndTrim,
        createAndTrim',
        unsafeCreate,
        unsafeCreateUptoN,
        unsafeCreateUptoN',
        mallocByteString,

        -- * Conversion to and from ForeignPtrs
        mkDeferredByteString,
        fromForeignPtr,
        toForeignPtr,
        fromForeignPtr0,
        toForeignPtr0,

        -- * Utilities
        nullForeignPtr,
        deferForeignPtrAvailability,
        SizeOverflowException,
        overflowError,
        checkedAdd,
        checkedMultiply,

        -- * Standard C Functions
        c_strlen,
        c_free_finalizer,

        memchr,
        memcmp,
        memcpy,
        memset,

        -- * cbits functions
        c_reverse,
        c_intersperse,
        c_maximum,
        c_minimum,
        c_count,
        c_sort,

        -- * Chars
        w2c, c2w, isSpaceWord8, isSpaceChar8,

        -- * Deprecated and unmentionable
        accursedUnutterablePerformIO,

        -- * Exported compatibility shim
        plusForeignPtr,
        unsafeWithForeignPtr
  ) where

import Data.ByteString.Internal.Type
