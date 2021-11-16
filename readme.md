# snap-stream

[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/snap-stream.svg)](https://hackage.haskell.org/package/snap-stream) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/snap-stream/badge)](https://matrix.hackage.haskell.org/#/package/snap-stream)  [![Github CI](https://github.com/obsidiansystems/snap-stream/workflows/github-action/badge.svg)](https://github.com/obsidiansystems/snap-stream/actions) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/obsidiansystems/snap-stream/blob/master/LICENSE)

[Snap](https://hackage.haskell.org/package/snap) handlers for HTTP [range requests](https://developer.mozilla.org/en-US/docs/Web/HTTP/Range_requests).

This package provides a way to respond to an HTTP request for a particular range of bytes. Range requests include a header that specifies which bytes the client is requesting (e.g., `Range: bytes=0-1023`). See the documentation for `serveStreamAs`.
