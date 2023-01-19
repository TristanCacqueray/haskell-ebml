# EBML parser

A pure parser for [rfc-8794][rfc-8794].
Use this library to decode [ebml][ebml], [webm][webm] and [mkv][mkv] file format.

## Overview

Work in progress, only binary elements are currently supported.

- [ ] Decode data type (e.g. int, text, ...)
- [ ] Schemas definition
- [ ] Handle edge case (e.g. streaming with unknown size)
- [ ] Pretty printer
- [ ] Incremental parser
- [ ] Property tests
- [ ] Schemas validation

[rfc-8794]: https://datatracker.ietf.org/doc/rfc8794/
[ebml]: https://github.com/ietf-wg-cellar/ebml-specification/blob/master/specification.markdown
[webm]: https://www.webmproject.org/docs/container/
[mkv]: https://www.matroska.org/index.html
