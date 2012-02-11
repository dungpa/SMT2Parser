2012.01.30:
- First prototype
- Currently use custom expression, should change to operators later.
- Need a lot of test cases to test parsing.
- AST should be revised.
- A lot of duplication should be removed.
- Handling numbers is not elegant.

2012.01.31:
- Have some trouble of parsing Sexpressions based on whitespaces.
- Need to understand how to use attempt

2012.02.01:
- Problems with trailing whitespaces.
- Need to write test cases.
- Remove unnecessary backtracking.
- Some problems with + (at least once) and * (at least zero) occurrence.
- May find shorter representation of enum toString.

2012.02.02:
- Need a common function for parsing string without whitespace.
- The official smt2 standard doesn't support declare-const, echo, display, simplify, help, get-model, declare-datatypes, eval.
- Currently does not support comments (out of line and in line).
- Need to find out how to treat (push) and (pop) convention of Z3.
- Could mix between built-in flags and custom flags.
- Currently does not support multiline commands.

2012.02.03:
- Replaced sepEndBy by sepEndBy1 in appropriate cases.

2012.02.05:
- Should review the role of spaces1.
- Need careful testing the structure of ASTs.
- Should run the parser on real-world examples.
- Reference other big projects to get new ideas.

2012.02.07:
- Investigate operator precedences and chances to apply them.
- Think of changing int32 to bigint to avoid stackoverflow.
- May add labels to have more indicative error messages.
- Handling whitespaces is complete.
- Need to investigate corner cases.
- Should add facade functions.

2012.02.11:
- Divide into 2 projects.
- Using enums for easy deriving toString(), it may lead to problem when using ASTs.