Just like occur, except that changes in the *All* buffer is
propagated to the original buffer.

I also added highlighting of the matches.

You can no longer use mouse-2 to find a match in the original file,
since the default definition of mouse to is useful.  
However, `C-c C-c' still works.

Line numbers are not listed in the *All* buffer.

Ok, it is _not_ just like occur.

Some limitations:

- Undo in the *All* buffer is an ordinary change in the original.
- Changes to the original buffer is not reflected in the *All* buffer.
- A single change in the *All* buffer must be limited to a single match.

Requires GNU Emacs 19.23 or later.
