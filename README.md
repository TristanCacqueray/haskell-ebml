# EBML parser

A pure parser for [rfc-8794][rfc-8794].
Use this library to decode [ebml][ebml], [webm][webm] and [mkv][mkv] file format.

## Overview

Implemented features:

- [x] Basic data types (binary, text, int)
- [x] Unknown-sized element
- [x] Pretty printer
- [x] Incremental stream parser
- [ ] WebM track informations
- [ ] Float and date types
- [ ] Schemas validation
- [ ] Property tests

## Contribute

Contributions and bug reports are welcome!
Run the `./bin/run-tests` script to validate a commit.

[rfc-8794]: https://datatracker.ietf.org/doc/rfc8794/
[ebml]: https://github.com/ietf-wg-cellar/ebml-specification/blob/master/specification.markdown
[webm]: https://www.webmproject.org/docs/container/
[mkv]: https://www.matroska.org/index.html
