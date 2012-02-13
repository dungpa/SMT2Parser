SMT2Parser is a minimal SMT-LIB v2 parser written in F# using the parser combinator library FParsec.

Current status:
- Support basic commands and scripts.
- Parsed successfully around 40000 benchmarks from smtlib.org.
- Need to manually increase stack space limit to avoid stackoverflow.

- Currently not support theory declaration.
- Currently not support logic declaration.
- Currently not support command responses.

