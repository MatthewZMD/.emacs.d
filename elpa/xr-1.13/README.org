#+TITLE: xr.el

XR converts Emacs regular expressions to the structured rx form, thus
being an inverse of ~rx~. It can also find mistakes and questionable
constructs inside regexp strings.

It can be useful for:
- Migrating existing code to rx form
- Understanding what a regexp string really means
- Finding errors in regexp strings

It can also parse and find mistakes in skip-sets, the regexp-like
arguments to ~skip-chars-forward~ and ~skip-chars-backward~.

The xr package can be used interactively or by other code as a library.

* Example

: (xr-pp "\\`\\(?:[^^]\\|\\^\\(?: \\*\\|\\[\\)\\)")

outputs

: (seq bos 
:      (or (not (any "^"))
:          (seq "^"
:               (or " *" "["))))

* Installation

From [[https://elpa.gnu.org/packages/xr.html][GNU ELPA]]:

: M-x package-install RET xr RET

* Interface

Functions parsing regexp strings:

| ~xr~      | convert regexp to rx                  |
| ~xr-pp~   | convert regexp to rx and pretty-print |
| ~xr-lint~ | find mistakes in regexp               |

Functions parsing skip sets:

| ~xr-skip-set~      | convert skip-set to rx                  |
| ~xr-skip-set-pp~   | convert skip-set to rx and pretty-print |
| ~xr-skip-set-lint~ | find mistakes in skip-set               |

Utility:

| ~xr-pp-rx-to-str~ | pretty-print rx expression to string |

* See also

The [[https://elpa.gnu.org/packages/relint.html][relint]] package uses xr to find regexp mistakes in elisp code.

The [[https://elpa.gnu.org/packages/lex.html][lex]] package, a lexical analyser generator, provides the
~lex-parse-re~ function which translates regexps to rx, but does not
attempt to handle all the edge cases of Elisp's regexp syntax or
pretty-print the result.

The [[https://github.com/joddie/pcre2el][pcre2el]] package, a regexp syntax converter and interactive regexp
explainer, can also be used for translating regexps to rx. However, xr
is more accurate for this purpose.
