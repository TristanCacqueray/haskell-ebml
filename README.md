# EBML parser

[![Hackage](https://img.shields.io/hackage/v/ebml.svg?logo=haskell&label=ebml)](https://hackage.haskell.org/package/ebml)

A pure decoder for [rfc-8794][rfc-8794].
Use this library to parse Extensible Binary Meta Language [ebml][ebml], [webm][webm] and [mkv][mkv] file format.

## Overview

The main goal of this project is to enable serving a webm stream by splitting the initialization segments from the media segments.

Implemented features:

- [x] Basic data types (binary, text, int)
- [x] Unknown-sized element
- [x] Pretty printer
- [x] Incremental stream parser
- [ ] WebM track informations
- [ ] Float and date types
- [ ] Schemas validation
- [ ] Encoder
- [ ] Property tests

## Usage

Check the Codec.EBML module for documentation. Or try the executable demo:

```ShellSession
$ cabal run exe:haskell-ebml -- ./data/firefox-mrec-opus.webm
[pretty print the EBML elements]
```

Without a parameter, the command process the stdin as a webm stream, for example:

```ShellSession
$ gst-launch-1.0 pulsesrc ! audioconvert ! opusenc ! webmmux ! fdsink fd=2 2>&1 >/dev/null | cabal run
[print the stream cluster]
```

## Contribute

Contributions and bug reports are welcome!
Run the `./bin/run-tests` script to validate a commit.

[rfc-8794]: https://datatracker.ietf.org/doc/rfc8794/
[ebml]: https://github.com/ietf-wg-cellar/ebml-specification/blob/master/specification.markdown
[webm]: https://www.webmproject.org/docs/container/
[mkv]: https://www.matroska.org/index.html
