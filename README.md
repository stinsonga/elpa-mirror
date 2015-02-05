# Context Coloring [![Build Status](https://travis-ci.org/jacksonrayhamilton/context-coloring.png?branch=develop)](https://travis-ci.org/jacksonrayhamilton/context-coloring)

<p align="center">
  <img alt="Screenshot of JavaScript code highlighted by context." src="screenshot.png" title="Screenshot">
</p>

Highlights code according to function context.

- Code in the global scope is one color. Code in functions within the global
  scope is a different color, and code within such functions is another color,
  and so on.
- Identifiers retain the color of the scope in which they are declared.

Lexical scope information at-a-glance can assist a programmer in understanding
the overall structure of a program. It can help to curb nasty bugs like name
shadowing. A rainbow can indicate excessive complexity. State change within a
closure is easily monitored.

By default, Context Coloring still highlights comments and strings
syntactically. It is still easy to differentiate code from non-code, and strings
cannot be confused for variables.

This coloring strategy is probably more useful than conventional syntax
highlighting. Highlighting keywords can help one to detect spelling errors, but
a [linter][] could also spot those errors, and if integrated with [flycheck][],
an extra spot opens up in your editing toolbelt.

Give context coloring a try; you may find that it *changes the way you write
code*.

## Features

- Supported languages: JavaScript
- Light and dark (customizable) color schemes.
- Very fast for files under 1000 lines.

## Installation

Requires Emacs 24+.

JavaScript language support requires either [js2-mode][], or
[Node.js 0.10+][node] and the [scopifier][] executable.

### ELPA

- `M-x package-refresh-contents RET`
- `M-x package-install RET context-coloring RET`

### Git

- Clone this repository.

```bash
cd ~/.emacs.d/
git clone https://github.com/jacksonrayhamilton/context-coloring.git
```

- Byte-compile the package for improved speed.

```bash
cd context-coloring/
make compile
```

- Add the following to your `~/.emacs` file:

```lisp
(add-to-list 'load-path "~/.emacs.d/context-coloring")
(require 'context-coloring)
```

### scopifier (for non-js2-mode users)

```bash
npm install -g scopifier
```

## Usage

Add the following to your `~/.emacs` file:

```lisp
;; non-js2-mode users:
(add-hook 'js-mode-hook 'context-coloring-mode)

;; js2-mode users:
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'context-coloring-mode)
```

## Customizing

You can enable different color schemes via `context-coloring-load-theme`. (The
screenshot above pairs the [zenburn][] color theme with the similarly-named
context-coloring theme.)

Built-in available themes are: `monokai`, `solarized`, `tango` and
`zenburn`. Contributions are welcome.

```lisp
(context-coloring-load-theme 'zenburn)
```

You can define your own themes, too:

```lisp
(context-coloring-define-theme
 'zenburn
 :colors '("#DCDCCC"
           "#93E0E3"
           "#BFEBBF"
           "#F0DFAF"
           "#DFAF8F"
           "#CC9393"
           "#DC8CC3"
           "#94BFF3"
           "#9FC59F"
           "#D0BF8F"
           "#DCA3A3"))
```

## Extending

To add support for a new language, write a "scopifier" for it, and define a new
coloring dispatch strategy with `context-coloring-define-dispatch`. Then the
plugin should handle the rest.

A "scopifier" is a CLI program that reads a buffer's contents from stdin and
writes a JSON array of numbers to stdout. Every three numbers in the array
represent a range of color. For instance, if I fed the following string of
JavaScript code to a scopifier,

```js
var a = function () {};
```

then the scopifier would produce the following array:

```js
[1,24,0,9,23,1]
```

Where, for every three numbers, the first number is a 1-indexed start [point][],
the second number is an exclusive end point, and the third number is a scope
level. The result of applying level 0 coloring to the range &#91;1, 24) and then
applying level 1 coloring to the range &#91;9, 23) would result in the following
coloring:

<p align="center">
  <img alt="Screenshot of ranges &#91;1, 24) and &#91;9, 23)." src="scopifier.png" title="Screenshot">
</p>

If there is an abstract syntax tree generator for your language, you can walk
the syntax tree, find variables and scopes, and build their positions and levels
into an array like the one above.

For example, a Ruby scopifier might be defined and implemented like this:

```lisp
(context-coloring-define-dispatch
 'ruby
 :modes '(ruby-mode)
 :executable "ruby"
 :command "/home/username/scopifier")
```

```ruby
#!/usr/bin/env ruby
def scopifier(code)
    # Parse code.
    # Return an array.
end
print scopifier ARGF.read
```

When a `--version` argument is passed, a scopifier should print its version
number and exit. For installable scopifiers, this allows context-coloring to
check for updates as needed.

[linter]: http://jshint.com/about/
[flycheck]: http://www.flycheck.org/
[zenburn]: http://github.com/bbatsov/zenburn-emacs
[point]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Point.html
[js2-mode]: https://github.com/mooz/js2-mode
[node]: http://nodejs.org/download/
[scopifier]: https://github.com/jacksonrayhamilton/scopifier
[load path]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Libraries.html
