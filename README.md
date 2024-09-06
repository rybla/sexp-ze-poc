# sexp-ze-poc

To run the webapp locally:

```sh
pnpm install
pnpm run build
pnpm run serve
```

## Notes

There are three kinds of cursors:
- __point__: a position between two subexpressions.
- __span__: a contiguous span between two points that contains equal number of
  open and close parentheses
- __zipper__: a one-hole-context formed by two spans surrounding a third span,
  where the two outer spans contain equal numbers of open and close parentheses.

Importantly, a span does not need to be a valid subexpression. So, for example,
the following is a valid span: `) (`.

Though, spans and zippers have an associated number that is half the number of
unclosed parentheses they contain. So `) (` has an unclosed number `1`, and `))
(((a b)` has an unclosed number `2`.

Usually we just have a cursor as pointing into a expression. But sometimes, for
example in the clipboard, we need to encode a standalone cursor.

A standalone span is encoded by:
- an expression
  - this is a valid expression, but is surrounded by `n` pairs of extra pairs of
    parentheses, where `n` is the unclosed number
- an unclosed number

A standalone zipper is encoded by:
- an expression
  - (in same way as standalone span), this is a valid expression, but is
    surrounded by `n` pairs of extra pairs of parentheses, where `n` is the
    unclosed number 
- an unclosed number
- a point into the expression (which is where the hole of the one-hole context
  is)