# snap-stream

[Snap](https://hackage.haskell.org/package/snap) handlers for HTTP [range requests](https://developer.mozilla.org/en-US/docs/Web/HTTP/Range_requests).

This package provides a way to respond to an HTTP request for a particular range of bytes. Range requests include a header that specifies which bytes the client is requesting (e.g., `Range: bytes=0-1023`). See the documentation for `serveStreamAs`.
