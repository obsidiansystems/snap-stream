{-|
Description:
  Handler for HTTP range requests

HTTP range requests include a header specifying the range of bytes expected in
the response. For example:

> Range: bytes=0-1023

See this <https://developer.mozilla.org/en-US/docs/Web/HTTP/Range_requests
documentation> for more details on range requests.
-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Contains web handlers to stream files
module Snap.Util.FileServe.Stream where

------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Attoparsec.ByteString.Char8 hiding (char8)
import Data.ByteString.Builder
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe (fromMaybe, isNothing)
import Data.Word (Word64)
import Prelude
import Snap.Core
import Snap.Internal.Parsing (fullyParse, parseNum)
import System.IO.Streams (OutputStream)


------------------------------------------------------------------------------
-- | Serves a file, with support for range requests and explicit mime type
-- specification.
--
-- This function can be used in the presence or absence of @Range@ headers: it
-- can be used to serve partial files and whole files.
--
-- Similar to 'serveFile'.
serveStreamAs :: MonadSnap m
              => ByteString
              -- ^ MIME type
              -> Word64
              -- ^ The size of the file being streamed
              -> (Word64 -> Word64 -> OutputStream Builder -> IO ())
              -- ^ If a partial range is requested, this function is used to send that range.
              -> (OutputStream Builder -> IO ())
              -- ^ If a partial range is not requested, this function is used to send the whole file.
              -> m ()
serveStreamAs mime sz stream streamAll = do
    reqOrig <- getRequest

    -- If-Range header must be ignored if there is no Range: header in the
    -- request (RFC 2616 section 14.27)
    let req = if isNothing $ getHeader "range" reqOrig
                then deleteHeader "if-range" reqOrig
                else reqOrig

    -- ok, at this point we know the last-modified time and the
    -- content-type. set those.
    modifyResponse $ setHeader "Accept-Ranges" "bytes"
                   . setContentType mime

    -- checkRangeReq checks for a Range: header in the request and sends a
    -- partial response if it matches.
    wasRange <- liftSnap $ checkRangeReq req stream sz

    -- if we didn't have a range request, we just do normal sendfile
    unless wasRange $ do
      modifyResponse $ setResponseCode 200
                     . setContentLength sz
      addToOutput $ \str -> liftIO (streamAll str) >> return str

------------------------------------------------------------------------------
data RangeReq = RangeReq !Word64 !(Maybe Word64)
              | SuffixRangeReq !Word64


------------------------------------------------------------------------------
rangeParser :: Parser RangeReq
rangeParser = string "bytes=" *>
              (byteRangeSpec <|> suffixByteRangeSpec) <*
              endOfInput
  where
    byteRangeSpec = do
        start <- fromIntegral <$> parseNum
        void $! char '-'
        end   <- option Nothing $ liftM Just parseNum

        return $! RangeReq start (fromIntegral <$> end)

    suffixByteRangeSpec =
        liftM (SuffixRangeReq . fromIntegral) $ char '-' *> parseNum


------------------------------------------------------------------------------
checkRangeReq :: (MonadSnap m)
              => Request
              -> (Word64 -> Word64 -> OutputStream Builder -> IO ())
              -> Word64
              -> m Bool
checkRangeReq req stream sz = do
    -- TODO/FIXME: multiple ranges
    maybe (return False)
          (\s -> either (const $ return False)
                        withRange
                        (fullyParse s rangeParser))
          (getHeader "range" req)

  where
    withRange (RangeReq start mend) = do
        let end = fromMaybe (sz-1) mend

        if start < 0 || end < start || start >= sz || end >= sz
           then send416
           else send206 start end

    withRange (SuffixRangeReq nbytes) = do
        let end   = sz-1
        let start = sz - nbytes

        if start < 0 || end < start || start >= sz || end >= sz
           then send416
           else send206 start end

    -- note: start and end INCLUSIVE here
    send206 start end = do
        let !len = end-start+1
        let crng = S.concat . L.toChunks $
                   toLazyByteString $
                   mconcat [ byteString "bytes "
                           , fromShow start
                           , char8 '-'
                           , fromShow end
                           , char8 '/'
                           , fromShow sz ]

        modifyResponse $ setResponseCode 206
                       . setHeader "Content-Range" crng
                       . setContentLength len

        -- end here was inclusive, sendFilePartial is exclusive
        addToOutput $ \str -> liftIO (stream start (end+1) str) >> return str
        return True


    send416 = do
        -- if there's an "If-Range" header in the request, then we just send
        -- back 200
        if getHeader "If-Range" req /= Nothing
           then return False
           else do
               let crng = S.concat . L.toChunks $
                          toLazyByteString $
                          mconcat [ byteString "bytes */"
                                  , fromShow sz ]

               modifyResponse $ setResponseCode 416
                              . setHeader "Content-Range" crng
                              . setContentLength 0
                              . deleteHeader "Content-Type"
                              . deleteHeader "Content-Encoding"
                              . deleteHeader "Transfer-Encoding"
                              . setResponseBody (return . id)

               return True

------------------------------------------------------------------------------
fromShow :: Show a => a -> Builder
fromShow = stringUtf8 . show
