# Simple hackernews emacs client

It's simple because it doesn't actually interact with hackernews. It
uses a non official http [api](http://api.ihackernews.com/page) to get
the data.

This version is able to list posts present in the main page, as well as
their points and comment count. You can also click (or `RET`) in the
post titles to open them in a browser.

The next versions will implement the upvote command and the possibility
to interact with comments.

## How it looks like

![screenshot](https://raw.github.com/clarete/hackernews.el/master/Screenshot.png)

## Installing

### Emacs package

If you like this package repo stuff, you just need add the marmalade
repo to your emacs config with the following code:

```lisp
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)
```

And then, run `M-x package-install`, the package name is `hackernews`.

### Downloading the file

Copy the hackernews.el to your elisp directory and add the following
code to your .emacs file:

```lisp
(require 'hackernews)
```

## Using it

Just run `M-x hackernews`

## License

Copyright (C) 2012  Lincoln de Sousa <lincoln@comum.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
