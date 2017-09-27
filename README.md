# Simple Hacker News Emacs Client

It's simple because it doesn't actually interact with Hacker News. It
uses a HTTP [API](https://hacker-news.firebaseio.com/v0) to get the
data.


## Navigation

This version is able to list posts present in the main page, as well as
their point and comment counts. You can also click (or `RET`) in the
post titles to open them in a browser.

Or you can press 't' to open the article in text only mode in emacs itself.
It may not work for some articles due to the page structure.

The next versions will implement the upvote command and the possibility
to interact with comments.

### Keymap

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>RET</kbd>     | Open post in default browser
<kbd>t</kbd>     | Open post in text-mode (may not work for all articles)
<kbd>n</kbd>       | Navigate to next post
<kbd>p</kbd>       | Navigate to previous post
<kbd>TAB</kbd>     | Navigate to next comment
<kbd>Shift-TAB</kbd>| Navigate to previous comment
<kbd>m</kbd>       | Load more posts
<kbd>g</kbd>       | Refresh posts
<kbd>q</kbd>       | Quit

## Screenshot

![screenshot](https://raw.github.com/clarete/hackernews.el/master/Screenshot.png)

## Installation

### Using built-in package manager

Those who like the built-in package manager `package.el` need only
point it to the Marmalade repository, which can be achieved by adding
the following code to your `user-init-file`:

```el
(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(package-initialize)
```

Then you can run `M-x package-install RET hackernews RET`.

### Manual download

Place the `hackernews.el` file into a directory on your `load-path`
and add the following code to your `user-init-file`:

```el
(autoload 'hackernews "hackernews" nil t)
```

Alternatively, if you always want the package loaded at startup
(this slows down startup):

```el
(require 'hackernews)
```

## Usage

Just run `M-x hackernews`.

## License

Copyright (C) 2012-2017 Lincoln de Sousa <lincoln@comum.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

[![instanc.es Badge](https://instanc.es/bin/clarete/hackernews.el.png)](http://instanc.es)
[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/clarete/hackernews.el/trend.png)](https://bitdeli.com/free "Bitdeli Badge")
