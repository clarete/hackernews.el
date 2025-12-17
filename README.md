# Simple Hacker News Emacs Client

[![Build](https://github.com/clarete/hackernews.el/actions/workflows/build.yml/badge.svg)](https://github.com/clarete/hackernews.el/actions/workflows/build.yml)
[![MELPA](https://melpa.org/packages/hackernews-badge.svg)](https://melpa.org/#/hackernews)
[![MELPA Stable](https://stable.melpa.org/packages/hackernews-badge.svg)](https://stable.melpa.org/#/hackernews)

It's simple because it doesn't actually interact with [Hacker
News](https://news.ycombinator.com/).  It uses a HTTP
[API](https://hacker-news.firebaseio.com/v0) to get the data.

## Interface

Version 0.8.0 of the `hackernews` package is able to fetch stories
from six different Hacker News feeds, namely top, new, best, ask, show
and job stories.  The default feed is top stories, which corresponds
to the Hacker News homepage.

The interface features a modern, widget-based design.  Each story is
displayed with a clickable title widget,
followed by metadata (score, comments count, and author) in styled
text with color coding.  Interactive buttons for accessing comments
and external links are provided for each story, and stories are
separated by horizontal dividers for easy reading.

The header includes clickable navigation buttons for switching between
feeds (Top, New, Best, Ask, Show) and refreshing the current feed.
Content is centered and formatted to a configurable width (default 80
characters) for optimal readability.  If the
[`visual-fill-column`](https://github.com/joostkremers/visual-fill-column)
package is installed, it will be used to center the content
automatically.

Clicking or typing <kbd>RET</kbd> on a widget opens it with the
command
[`browse-url`](https://gnu.org/software/emacs/manual/html_node/emacs/Browse_002dURL.html),
which selects a browser based on the user option
`browse-url-browser-function`.  This defaults to the system's default
browser.  Comment buttons use the user option
`hackernews-internal-browser-function`, which defaults to
[`eww`](https://gnu.org/software/emacs/manual/html_node/eww/index.html)
for in-Emacs browsing.

A future `hackernews` version may support upvoting and interacting
with comments.

### Keymap

| Key              | Description                                  |
|------------------|----------------------------------------------|
| <kbd>RET</kbd>   | Activate widget at point (open link/button)  |
| <kbd>t</kbd>     | Open link in text-based browser within Emacs |
| <kbd>r</kbd>     | Mark link as visited                         |
| <kbd>R</kbd>     | Mark link as unvisited                       |
| <kbd>n</kbd>     | Move to next story                           |
| <kbd>p</kbd>     | Move to previous story                       |
| <kbd>TAB</kbd>   | Move to next widget (buttons, links, etc.)   |
| <kbd>S-TAB</kbd> | Move to previous widget                      |
| <kbd>m</kbd>     | Load more stories                            |
| <kbd>g</kbd>     | Reload stories                               |
| <kbd>f</kbd>     | Prompt user for a feed to switch to          |
| <kbd>q</kbd>     | Quit                                         |

All feed re/loading commands accept an optional [numeric prefix
argument](https://gnu.org/software/emacs/manual/html_node/emacs/Arguments.html)
denoting how many stories to act on.  For example,
<kbd>M-5</kbd><kbd>0</kbd><kbd>g</kbd> refreshes the feed of the
current `hackernews` buffer and fetches its top 50 stories.  With no
prefix argument, the value of the user option
`hackernews-items-per-page` is used instead.

## Screenshot

![screenshot](https://raw.github.com/clarete/hackernews.el/master/Screenshot.png)

## Installation

### Using the built-in package manager

Those who like the built-in package manager `package.el` need only
point it to a [MELPA](https://melpa.org) repository, which can be
achieved by adding the following code to your `user-init-file`:

```el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
```

Note that this will follow the bleeding edge of `hackernews`
development.  Though `hackernews` contributors make every effort to
keep the latest snapshot usable and bug-free, to err is human.  If
this thought scares you, a more stable experience can be achieved by
replacing:

```el
'("melpa" . "https://melpa.org/packages/")
```

in the example above with:

```el
'("melpa-stable" . "https://stable.melpa.org/packages/")
```

or equivalent.  See https://melpa.org/#/getting-started/ for more on
this.

Once `package.el` is configured, you can run
<kbd>M-x</kbd>`package-install`<kbd>RET</kbd>`hackernews`<kbd>RET</kbd>.

### Manual download

Place the `hackernews.el` file into a directory on your `load-path`
and add the following code to your `user-init-file`:

```el
(autoload 'hackernews "hackernews" nil t)
```

Alternatively, if you always want the package loaded at startup (this
slows down startup):

```el
(require 'hackernews)
```

## Configuration Examples

### Classic Mode (Default)

The classic mode provides a minimal, text-based interface with no additional configuration needed:

```el
(use-package hackernews
  :ensure t)
```

### Modern Mode with Visual Enhancements

The modern mode offers an enhanced interface with widgets, colors, and centered content:

```el
;; Install visual-fill-column for centered display
(use-package visual-fill-column
  :ensure t)

;; Configure hackernews with modern UI
(use-package hackernews
  :ensure t
  :config
  ;; Use modern UI with enhanced visual elements
  (setq hackernews-ui-style 'modern)
  ;; Enable emoji icons in the interface
  (setq hackernews-enable-emojis t)
  ;; Optional: customize display width (default 80)
  ;; (setq hackernews-display-width 100)
  )
```

## Usage

Just run <kbd>M-x</kbd>`hackernews`<kbd>RET</kbd>.  This reads the
feed specified by the user option `hackernews-default-feed`, which
defaults to top stories, i.e. the Hacker News homepage.  A direct
command for each supported feed is also supported, e.g.
<kbd>M-x</kbd>`hackernews-top-stories`<kbd>RET</kbd> or
<kbd>M-x</kbd>`hackernews-ask-stories`<kbd>RET</kbd>.  These direct
commands are not autoloaded, however, so to use them before
`hackernews` has been loaded, you should autoload them yourself, e.g.
by adding the following to your `user-init-file`:

```el
(autoload 'hackernews-ask-stories "hackernews" nil t)
```

### Customization

You can list and modify all custom faces and variables by typing
<kbd>M-x</kbd>`customize-group`<kbd>RET</kbd>`hackernews`<kbd>RET</kbd>.

Key customization options:

- `hackernews-ui-style` (default `'classic`): Choose between
  `'classic` (minimal text-based interface) or `'modern` (enhanced
  widget-based interface with colors and visual separators).

- `hackernews-display-width` (default 80): Maximum width for
  displaying content in modern mode.  Content is automatically
  centered when
  [`visual-fill-column`](https://github.com/joostkremers/visual-fill-column)
  is installed.

- `hackernews-enable-emojis` (default nil): Whether to display emojis
  in the modern interface.  When non-nil, feed navigation buttons
  (Top, New, Best, Ask, Show) and comment counts will include emoji
  icons for visual enhancement.

In modern mode, buffers are displayed using `display-buffer-same-window`
for a full-screen experience.  Classic mode uses the traditional display
behavior.

### Troubleshooting

In general, errors and misbehavior pertaining to network retrieval and
JSON parsing are probably due to bugs in older Emacsen.  The minimum
recommended Emacs version for `hackernews` is 25.  Emacs 24 should
work, but suffers from network security vulnerabilities that were
fixed in version 25.  Emacs 23 is no longer officially supported as of
[2018-06-08](https://github.com/clarete/hackernews.el/issues/46),
i.e. since `hackernews` version 0.5.0.

In any case, please report any problems on the project's [issue
tracker](https://github.com/clarete/hackernews.el/issues), so that the
possibility for mitigation can be investigated.

## License

Copyright (C) 2012-2025 The Hackernews.el Authors

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
