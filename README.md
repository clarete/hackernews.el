# Simple Hacker News Emacs Client

It's simple because it doesn't actually interact with [Hacker
News](https://news.ycombinator.com/). It uses a HTTP
[API](https://hacker-news.firebaseio.com/v0) to get the data.

## Interface

Version 0.4.0 of the `hackernews` package is able to fetch stories
from six different Hacker News feeds, namely top, new, best, ask, show
and job stories. The default feed is top stories, which corresponds to
the Hacker News homepage.

The score, title, and comments count of each story is presented on a
line of its own (see screenshot below), though this format is
customizable. Both the title and comments count strings are
hyperlinked to the Hacker News page for the item (the one with the
comments), unless the story links to an external page, in which case
the title is hyperlinked to that instead.

Clicking or typing <kbd>RET</kbd> on a link opens it with the command
[`browse-url`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Browse_002dURL.html),
which selects a browser based on the user option
`browse-url-browser-function`. This defaults to the system's default
browser.

Typing <kbd>t</kbd> on a link first tries to open it in
[`eww`](https://www.gnu.org/software/emacs/manual/html_node/eww/index.html),
if available, and otherwise passes it to the command
`browse-url-text-emacs`, which consults the user option
`browse-url-text-browser`. This defaults to running `lynx` within
Emacs. Keep in mind that some websites do not render well in text
mode.

A future `hackernews` version may support upvoting and interacting
with comments.

### Keymap

Keybinding       | Description
-----------------|-------------------------------------------------------
<kbd>RET</kbd>   | Open link in default (external) browser
<kbd>t</kbd>     | Open link in text-mode browser within Emacs
<kbd>n</kbd>     | Move to next title link
<kbd>p</kbd>     | Move to previous title link
<kbd>TAB</kbd>   | Move to next comments count link
<kbd>S-TAB</kbd> | Move to previous comments count link
<kbd>m</kbd>     | Load more stories
<kbd>g</kbd>     | Reload stories
<kbd>f</kbd>     | Prompt user for a feed to switch to
<kbd>q</kbd>     | Quit

All feed re/loading commands accept an optional [numeric prefix
argument](https://www.gnu.org/software/emacs/manual/html_node/emacs/Arguments.html)
denoting how many stories to act on. For example,
<kbd>M-5</kbd><kbd>0</kbd><kbd>g</kbd> refreshes the feed of the
current `hackernews` buffer and fetches its top 50 stories. With no
prefix argument, the value of the user option
`hackernews-items-per-page` is used instead.

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

Then you can run <kbd>M-x</kbd>`package-install`<kbd>RET</kbd>`hackernews`<kbd>RET</kbd>.

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

Just run <kbd>M-x</kbd>`hackernews`<kbd>RET</kbd>. This reads the feed
specified by the user option `hackernews-default-feed`, which defaults
to top stories, i.e. the Hacker News homepage. A direct command for
each supported feed is also supported, e.g.
<kbd>M-x</kbd>`hackernews-top-stories`<kbd>RET</kbd> or
<kbd>M-x</kbd>`hackernews-ask-stories`<kbd>RET</kbd>. These direct
commands are not autoloaded, however, so to use them before
`hackernews` has been loaded you should autoload them yourself, e.g.
by adding the following to your `user-init-file`:

```el
(autoload 'hackernews-ask-stories "hackernews" nil t)
```

### Customization

You can list and modify all custom faces and variables by typing
<kbd>M-x</kbd>`customize-group`<kbd>RET</kbd>`hackernews`<kbd>RET</kbd>.

All `hackernews` buffers are displayed using the `pop-to-buffer`
function for increased compatibility and customizability in how
windows and frames are re/used. This function displays buffers in a
new window by default. The simplest way to instead reuse the current
window for `hackernews` buffers is to customize one of the user
options `same-window-buffer-names`, `same-window-regexp` or in Emacs
24 and subsequent versions, `display-buffer-alist` via
<kbd>M-x</kbd>`customize-group`<kbd>RET</kbd>`windows`<kbd>RET</kbd>.

If you prefer to roll out your own Elisp, you could add to your
`user-init-file` something as simple as:

```el
(add-to-list 'same-window-regexps "\\`\\*hackernews .*\\*\\'")
```

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
