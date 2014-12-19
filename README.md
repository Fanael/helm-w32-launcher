[![MELPA](http://melpa.org/packages/helm-w32-launcher-badge.svg)](http://melpa.org/#/helm-w32-launcher)
[![MELPA Stable](http://stable.melpa.org/packages/helm-w32-launcher-badge.svg)](http://stable.melpa.org/#/helm-w32-launcher)

# Helm W32 launcher

Launch programs from Start Menu from Emacs, using Helm.

Why would you want to do that?
  * It actually makes sense if you agree that Helm is superior to the Start
    Menu/Screen search feature.
  * Besides, it's Emacs, so why not?

## Installation

Make sure [MELPA](http://melpa.org/) or [MELPA Stable](http://stable.melpa.org/)
is in your `package-archives`, then issue

    M-x package-install RET helm-w32-launcher RET

Other installation methods are unsupported. If you can't or don't want to use
MELPA, you'll have to remember to put `helper-src` in the same directory as the
Emacs Lisp code and to set up autoloads and/or requires as necessary.

The first time one of the provided commands is called, the package tries to
compile the C# helper. If you have .NET 2.0 or newer installed, this should just
work, so any errors messages from `csc.exe` should be reported as bugs.

## Usage

To run a program normally:

    M-x helm-w32-launcher

To run a program with elevated privileges:

    M-x helm-w32-launcher-elevated

To open a directory a shortcut is in:

    M-x helm-w32-launcher-open-shortcut-directory

To open the property page of a shortcut:

    M-x helm-w32-launcher-open-shortcut-properties

If `helm-w32-launcher-use-cache` is non-nil (the default), the Start Menu
entries are cached to avoid process spawns and disk I/O. To flush the cache to
see the current Start Menu contents, use

    M-x helm-w32-launcher-flush-cache

## Implementation

Due to Emacs having no FFI to speak of, the code retrieving Start Menu
directories is implemented in C#. Listing the entries is done there too, because
`directory-files-recursively` requires Emacs 25, and
`System.IO.Directory.GetFiles` is much nicer than manually implementing
recursive search with `directory-files`.

The Emacs side does relatively little: it calls the C# code, reads the output
using `read`, caches the entries, passes them to Helm and calls the C# code to
act on the selection again.

The C# code actually executes the shortcut because even though
`w32-shell-execute` exists, Emacs 24.3 and earlier use legacy codepage-based
APIs, so they're unable to correctly start a shortcut from a path containing
characters outside the current code page. .NET uses Unicode APIs, so it just
works there.
