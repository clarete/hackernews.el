;;; hackernews.el --- Hacker News Client -*- lexical-binding: t -*-

;; Copyright (C) 2012-2025 The Hackernews.el Authors

;; Author: Lincoln de Sousa <lincoln@clarete.li>
;; Maintainer: Basil L. Contovounesios <basil@contovou.net>
;; Keywords: comm hypermedia news
;; Version: 0.8.0
;; Package-Requires: ((emacs "24.3") (visual-fill-column "2.2"))
;; URL: https://github.com/clarete/hackernews.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Read Hacker News from Emacs.
;;
;; Enjoy!

;;; Code:

(require 'browse-url)
(require 'cus-edit)
(require 'format-spec)
(require 'url)
(require 'widget)
(require 'wid-edit)
(require 'cl-lib)

(eval-when-compile
  ;; - 24.3 started complaining about unknown `declare' props.
  ;; - 28 introduced `modes'.
  (and (boundp 'defun-declarations-alist)
       (null (assq 'modes defun-declarations-alist))
       (push (list 'modes #'ignore) defun-declarations-alist)))

(defgroup hackernews nil
  "Simple Hacker News client."
  :group 'external
  :prefix "hackernews-")

;;;; Faces

(define-obsolete-face-alias 'hackernews-link-face
  'hackernews-link "0.4.0")

(defface hackernews-link
  '((t :inherit link :underline nil))
  "Face used for links to stories."
  :package-version '(hackernews . "0.4.0"))

(defface hackernews-link-visited
  '((t :inherit link-visited :underline nil))
  "Face used for visited links to stories."
  :package-version '(hackernews . "0.5.0"))

(define-obsolete-face-alias 'hackernews-comment-count-face
  'hackernews-comment-count "0.4.0")

(defface hackernews-comment-count
  '((t :inherit hackernews-link))
  "Face used for comment counts."
  :package-version '(hackernews . "0.4.0"))

(defface hackernews-comment-count-visited
  '((t :inherit hackernews-link-visited))
  "Face used for visited comment counts."
  :package-version '(hackernews . "0.5.0"))

(define-obsolete-face-alias 'hackernews-score-face
  'hackernews-score "0.4.0")

(defface hackernews-score
  '((t :inherit default))
  "Face used for the score of a story."
  :package-version '(hackernews . "0.4.0"))

;; Faces for modern UI style

(defface hackernews-logo
  '((t :foreground "#ff6600" :height 1.5))
  "Face used for the \"Y\" in the Hacker News logo.
Only used when `hackernews-ui-style' is \\='modern."
  :package-version '(hackernews . "0.8.0"))

(defface hackernews-title-text
  '((t :foreground "#ff6600" :height 1.3))
  "Face used for the \"Hacker News\" title text.
Only used when `hackernews-ui-style' is \\='modern."
  :package-version '(hackernews . "0.8.0"))

(defface hackernews-separator
  '((t :foreground "#666666"))
  "Face used for horizontal separator lines.
Only used when `hackernews-ui-style' is \\='modern."
  :package-version '(hackernews . "0.8.0"))

(defface hackernews-score-modern
  '((t :foreground "#ff6600"))
  "Face used for story scores in modern UI.
Only used when `hackernews-ui-style' is \\='modern."
  :package-version '(hackernews . "0.8.0"))

(defface hackernews-author
  '((t :foreground "#0066cc"))
  "Face used for author names.
Only used when `hackernews-ui-style' is \\='modern."
  :package-version '(hackernews . "0.8.0"))

(defface hackernews-feed-indicator
  '((t :foreground "#ff6600"))
  "Face used for the current feed indicator.
Only used when `hackernews-ui-style' is \\='modern."
  :package-version '(hackernews . "0.8.0"))

;;;; User options

(define-obsolete-variable-alias 'hackernews-top-story-limit
  'hackernews-items-per-page "0.4.0")

(defcustom hackernews-items-per-page 20
  "Default number of stories to retrieve in one go."
  :package-version '(hackernews . "0.4.0")
  :type 'integer)

(defvar hackernews-feed-names
  '(("top"  . "top stories")
    ("new"  . "new stories")
    ("best" . "best stories")
    ("ask"  . "ask stories")
    ("show" . "show stories")
    ("job"  . "job stories"))
  ;; TODO: Should the keys all be symbols?
  "Map feed types as strings to their display names.")
;; As per Info node `(elisp) Basic Completion'
(put 'hackernews-feed-names 'risky-local-variable t)

(defcustom hackernews-default-feed "top"
  "Default story feed to load.
See `hackernews-feed-names' for supported feed types."
  :package-version '(hackernews . "0.4.0")
  :type (cons 'choice (mapcar (lambda (feed)
                                (list 'const :tag (cdr feed) (car feed)))
                              hackernews-feed-names)))

;; TODO: Allow the following `*-format' options to take on function values?

(defcustom hackernews-item-format "%-7s%t %c\n"
  "Format specification for items in hackernews buffers.
The result is obtained by passing this string and the following
arguments to `format-spec':

%s - Item score;    see `hackernews-score-format'.
%t - Item title;    see `hackernews-title-format'.
%c - Item comments; see `hackernews-comments-format'."
  :package-version '(hackernews . "0.4.0")
  :type 'string)

(defcustom hackernews-score-format "[%s]"
  "Format specification for displaying the score of an item.
The result is obtained by passing this string and the score count
to `format'."
  :package-version '(hackernews . "0.4.0")
  :type 'string)

(defcustom hackernews-title-format "%s"
  "Format specification for displaying the title of an item.
The result is obtained by passing this string and the title to
`format'."
  :package-version '(hackernews . "0.4.0")
  :type 'string)

(defcustom hackernews-comments-format "(%s comments)"
  "Format specification for displaying the comments of an item.
The result is obtained by passing this string and the comments
count to `format'."
  :package-version '(hackernews . "0.4.0")
  :type 'string)

(defcustom hackernews-preserve-point t
  "Whether to preserve point when loading more stories.
When nil, point is placed on first new item retrieved."
  :package-version '(hackernews . "0.4.0")
  :type 'boolean)

(defcustom hackernews-before-render-hook ()
  "Hook called before rendering any new items."
  :package-version '(hackernews . "0.4.0")
  :type 'hook)

(defcustom hackernews-after-render-hook ()
  "Hook called after rendering any new items.
The position of point will not have been affected by the render."
  :package-version '(hackernews . "0.4.0")
  :type 'hook)

(defcustom hackernews-finalize-hook ()
  "Hook called as final step of loading any new items.
The position of point may have been adjusted after the render,
buffer-local feed state will have been updated and the hackernews
buffer will be current and displayed in the selected window."
  :package-version '(hackernews . "0.4.0")
  :type 'hook)

(defcustom hackernews-suppress-url-status t
  "Whether to suppress messages controlled by `url-show-status'.
When nil, `url-show-status' determines whether certain status
messages are displayed when retrieving online data.  This is
suppressed by default so that the hackernews progress reporter is
not interrupted."
  :package-version '(hackernews . "0.4.0")
  :type 'boolean)

(defcustom hackernews-internal-browser-function
  (if (functionp 'eww-browse-url)
      #'eww-browse-url
    #'browse-url-text-emacs)
  "Function to load a given URL within Emacs.
See `browse-url-browser-function' for some possible options."
  :package-version '(hackernews . "0.4.0")
  :type (cons 'radio (butlast (cdr (custom-variable-type
                                    'browse-url-browser-function)))))

(defcustom hackernews-show-visited-links t
  "Whether to visually distinguish links that have been visited.
For example, when a link with the `hackernews-link' face is
visited and the value of this variable is non-nil, that link's
face is changed to `hackernews-link-visited'."
  :package-version '(hackernews . "0.5.0")
  :type 'boolean)

(defcustom hackernews-visited-links-file
  (locate-user-emacs-file "hackernews/visited-links.el")
  "Name of file used to remember which links have been visited.
When nil, visited links are not persisted across sessions."
  :package-version '(hackernews . "0.5.0")
  :type '(choice file (const :tag "None" nil)))

(defcustom hackernews-ui-style 'classic
  "Display style for the Hacker News interface.
\\='classic - Minimal text-based interface using format strings.
            This is the traditional interface, backward compatible
            with all existing configurations.
\\='modern  - Enhanced interface with interactive widgets, colors,
            and visual separators for improved readability."
  :package-version '(hackernews . "0.8.0")
  :type '(choice (const :tag "Classic minimal interface" classic)
                 (const :tag "Modern enhanced interface" modern)))

(defcustom hackernews-display-width 80
  "Maximum width for displaying hackernews content.
Only used when `hackernews-ui-style' is \\='modern."
  :package-version '(hackernews . "0.8.0")
  :type 'integer)

(defcustom hackernews-enable-emojis nil
  "Whether to display emojis in the modern interface.
When non-nil and `hackernews-ui-style' is \\='modern, feed navigation
buttons and comment counts will include emoji icons for visual
enhancement."
  :package-version '(hackernews . "0.8.0")
  :type 'boolean)

;;;; Internal definitions

(defconst hackernews-api-version "v0"
  "Currently supported version of the Hacker News API.")

(defconst hackernews-api-format
  (format "https://hacker-news.firebaseio.com/%s/%%s.json"
          hackernews-api-version)
  "Format of targeted Hacker News API URLs.")

(defconst hackernews-site-item-format "https://news.ycombinator.com/item?id=%s"
  "Format of Hacker News website item URLs.")

(defvar hackernews--feed-state ()
  "Plist capturing state of current buffer's Hacker News feed.
:feed     - Type of endpoint feed; see `hackernews-feed-names'.
:items    - Vector holding items being or last fetched.
:register - Cons of number of items currently displayed and
            vector of item IDs last read from this feed.
            The `car' is thus an offset into the `cdr'.")
(make-variable-buffer-local 'hackernews--feed-state)

(defvar hackernews-feed-history ()
  "Completion history of hackernews feeds switched to.")

(define-obsolete-variable-alias 'hackernews-map
  'hackernews-mode-map "0.4.0")

(defvar hackernews-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f"             #'hackernews-switch-feed)
    (define-key map "g"             #'hackernews-reload)
    (define-key map "m"             #'hackernews-load-more-stories)
    (define-key map "n"             #'hackernews-next-item)
    (define-key map "p"             #'hackernews-previous-item)
    (define-key map "\t"            #'hackernews-next-comment)
    (define-key map [backtab]       #'hackernews-previous-comment)
    (define-key map [S-iso-lefttab] #'hackernews-previous-comment)
    (define-key map [S-tab]         #'hackernews-previous-comment)
    map)
  "Keymap used in hackernews buffer.")

(defvar hackernews-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map "R" #'hackernews-button-mark-as-unvisited)
    (define-key map "r" #'hackernews-button-mark-as-visited)
    (define-key map "t" #'hackernews-button-browse-internal)
    map)
  "Keymap used on hackernews links.")

(define-button-type 'hackernews-link
  'action                  #'hackernews-browse-url-action
  'follow-link             t
  'hackernews-face         'hackernews-link
  'hackernews-visited-face 'hackernews-link-visited
  'keymap                  hackernews-button-map)

(define-button-type 'hackernews-comment-count
  'hackernews-face         'hackernews-comment-count
  'hackernews-visited-face 'hackernews-comment-count-visited
  'supertype               'hackernews-link)

;; Use `font-lock-face' on creation instead.
(button-type-put 'hackernews-link          'face nil)
(button-type-put 'hackernews-comment-count 'face nil)

;; Remove `hackernews-link' as `supertype' so that
;; `hackernews--forward-button' can distinguish between
;; `hackernews-link' and `hackernews-comment-count'.
(button-type-put 'hackernews-comment-count 'supertype 'button)

(defvar hackernews--visited-ids
  (mapcar #'list '(hackernews-link hackernews-comment-count))
  "Map link button types to their visited ID sets.
Values are initially nil and later replaced with a hash table.")

;; Emulate `define-error' for Emacs < 24.4.
(put 'hackernews-error 'error-conditions '(hackernews-error error))
(put 'hackernews-error 'error-message    "Hackernews error")

;;;; Utils

(defun hackernews--get (prop)
  "Extract value of PROP from `hackernews--feed-state'."
  (plist-get hackernews--feed-state prop))

(defun hackernews--put (prop val)
  "Change value in `hackernews--feed-state' of PROP to VAL."
  (setq hackernews--feed-state (plist-put hackernews--feed-state prop val)))

(defun hackernews--comments-url (id)
  "Return Hacker News website URL for item with ID."
  (format hackernews-site-item-format id))

(defun hackernews--format-api-url (fmt &rest args)
  "Construct a Hacker News API URL.
The result of passing FMT and ARGS to `format' is substituted in
`hackernews-api-format'."
  (format hackernews-api-format (apply #'format fmt args)))

(defun hackernews--item-url (id)
  "Return Hacker News API URL for item with ID."
  (hackernews--format-api-url "item/%s" id))

(defun hackernews--feed-url (feed)
  "Return Hacker News API URL for FEED.
See `hackernews-feed-names' for supported values of FEED."
  (hackernews--format-api-url "%sstories" feed))

(defun hackernews--feed-name (feed)
  "Lookup FEED in `hackernews-feed-names'."
  (cdr (assoc-string feed hackernews-feed-names)))

(defun hackernews--feed-annotation (feed)
  "Annotate FEED during completion.
This is intended as an :annotation-function in
`completion-extra-properties'."
  (let ((name (hackernews--feed-name feed)))
    (and name (concat " - " name))))


;;;; UI Helpers for modern style

(defconst hackernews--separator-char ?-
  "Character used for horizontal separators in modern UI.")

(defun hackernews--string-separator ()
  "Return a string with the separator character."
  (make-string hackernews-display-width hackernews--separator-char))

(defun hackernews--insert-separator ()
  "Insert a horizontal separator line using modern UI style."
  (insert "\n")
  (insert (propertize (hackernews--string-separator)
                      'face 'hackernews-separator))
  (insert "\n\n"))

(defun hackernews--insert-logo ()
  "Insert the Hacker News logo/header in modern UI style."
  (insert "\n")
  (insert (propertize "Y " 'face 'hackernews-logo))
  (insert (propertize "Hacker News" 'face 'hackernews-title-text))
  (insert "\n\n"))


(defun hackernews--insert-header (feed-name)
  "Insert the header for FEED-NAME in modern UI style."
  (hackernews--insert-logo)

  ;; Feed navigation buttons
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (hackernews-top-stories))
                 :help-echo "View top stories"
                 (format " %sTop " (if hackernews-enable-emojis "🔥 " "")))

  (insert " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (hackernews-new-stories))
                 :help-echo "View new stories"
                 (format " %sNew " (if hackernews-enable-emojis "🆕 " "")))

  (insert " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (hackernews-best-stories))
                 :help-echo "View best stories"
                 (format " %sBest " (if hackernews-enable-emojis "⭐ " "")))

  (insert " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (hackernews-ask-stories))
                 :help-echo "View ask stories"
                 (format " %sAsk " (if hackernews-enable-emojis "❓ " "")))

  (insert " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (hackernews-show-stories))
                 :help-echo "View show stories"
                 (format " %sShow " (if hackernews-enable-emojis "📺 " "")))

  (insert " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (hackernews-reload))
                 :help-echo "Refresh current feed"
                 " ↻ Refresh ")

  ;; Current feed indicator
  (insert "\n\n")
  (insert (propertize (format "Showing: %s\n" feed-name)
                      'face 'hackernews-feed-indicator))

  ;; Keyboard shortcuts help
  (insert "Keyboard: (n) Next | (p) Previous | (g) Refresh | (q) Quit\n")

  (hackernews--insert-separator))




;;;; Motion

(defun hackernews--forward-button (n type)
  "Move to Nth next button of TYPE (previous if N is negative)."
  (let ((pos  (point))
        (sign (cond ((> n 0)  1)
                    ((< n 0) -1)
                    (t        0)))
        msg)
    (while (let ((button (ignore-errors (forward-button sign))))
             (when button
               (when (button-has-type-p button type)
                 (setq pos (button-start button))
                 (setq msg (button-get button 'help-echo))
                 (setq n   (- n sign)))
               (/= n 0))))
    (goto-char pos)
    (when msg (message "%s" msg))))

(defun hackernews-next-item (&optional n)
  "Move to Nth next story (previous if N is negative).
N defaults to 1."
  (declare (modes hackernews-mode))
  (interactive "p")
  (if (eq hackernews-ui-style 'modern)
      ;; Modern UI: search for separator lines
      (let ((count (or n 1)))
        (if (< count 0)
            (hackernews-previous-item (- count))
          (dotimes (_ count)
            (let ((separator-regex (concat "^" (regexp-quote (hackernews--string-separator)) "$")))
              (if (search-forward-regexp separator-regex nil t)
                  (progn
                    (forward-line 2)  ; Skip blank line to reach title
                    (beginning-of-line)
                    (recenter))  ; Center cursor vertically
                (message "No more stories"))))))
    ;; Classic UI: use original button navigation
    (hackernews--forward-button (or n 1) 'hackernews-link)))

(defun hackernews-previous-item (&optional n)
  "Move to Nth previous story (next if N is negative).
N defaults to 1."
  (declare (modes hackernews-mode))
  (interactive "p")
  (if (eq hackernews-ui-style 'modern)
      ;; Modern UI: search for separator lines
      (let ((count (or n 1)))
        (if (< count 0)
            (hackernews-next-item (- count))
          (dotimes (_ count)
            (let ((separator-regex (concat "^" (regexp-quote (hackernews--string-separator)) "$")))
              (search-backward-regexp separator-regex nil t)
              (unless (search-backward-regexp separator-regex nil t)
                (goto-char (point-min)))
              (forward-line 2)  ; Skip blank line to reach title
              (beginning-of-line)
              (recenter)))))  ; Center cursor vertically
    ;; Classic UI: use original button navigation (reverse direction)
    (hackernews-next-item (- (or n 1)))))

(defun hackernews-first-item ()
  "Move point to first story link in hackernews buffer."
  (declare (modes hackernews-mode))
  (interactive)
  (goto-char (point-min))
  (hackernews-next-item))

(defun hackernews-next-comment (&optional n)
  "Move to Nth next comments link (previous if N is negative).
N defaults to 1."
  (declare (modes hackernews-mode))
  (interactive "p")
  (hackernews--forward-button (or n 1) 'hackernews-comment-count))

(defun hackernews-previous-comment (&optional n)
  "Move to Nth previous comments link (next if N is negative).
N defaults to 1."
  (declare (modes hackernews-mode))
  (interactive "p")
  (hackernews-next-comment (- (or n 1))))

;;;; UI

(defun hackernews--read-visited-links ()
  "Read and return contents of `hackernews-visited-links-file'.
On error, display a warning for the user and return nil."
  (when (and hackernews-visited-links-file
             (file-exists-p hackernews-visited-links-file))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents hackernews-visited-links-file)
          (unless (eobp)
            (read (current-buffer))))
      (error
       (ignore
        (lwarn 'hackernews :error
               "Could not read `hackernews-visited-links-file':\n      %s%s"
               (error-message-string err)
               (substitute-command-keys "
N.B.  Any valid data in the file will be overwritten next time
      Emacs is killed.  To avoid data loss, type
      \\[hackernews-load-visited-links] after fixing the error
      above.
      Alternatively, you can set `hackernews-visited-links-file'
      to nil: the file will not be overwritten, but any links
      visited in the current Emacs session will not be saved.")))))))

(defun hackernews-load-visited-links ()
  "Merge visited links on file with those in memory.
This command tries to reread `hackernews-visited-links-file',
which may be useful when, for example, the contents of the file
change and you want to update the hackernews display without
restarting Emacs, or the file could not be read initially and
risks being overwritten next time Emacs is killed."
  (interactive)
  ;; Ensure `hackernews--visited-ids' is initialized
  (dolist (entry hackernews--visited-ids)
    (unless (cdr entry)
      (setcdr entry (make-hash-table))))
  ;; Merge with `hackernews-visited-links-file'
  (dolist (entry (hackernews--read-visited-links))
    (let ((table (cdr (assq (car entry) hackernews--visited-ids))))
      (maphash (lambda (k newv)
                 (let ((oldv (gethash k table)))
                   (when (or (not oldv)
                             (time-less-p (plist-get oldv :last-visited)
                                          (plist-get newv :last-visited)))
                     (puthash k newv table))))
               (cdr entry)))))

(defalias 'hackernews--prin1
  (if (condition-case nil
          (with-no-warnings (prin1 t #'ignore ()))
        (wrong-number-of-arguments))
      #'prin1
    (lambda (object &optional printcharfun _overrides)
      (let ((print-length nil)
            (print-level nil))
        (prin1 object printcharfun))))
  "Compatibility shim for default `prin1' overrides in Emacs < 29.
\n(fn OBJECT &optional PRINTCHARFUN OVERRIDES)")

(defun hackernews-save-visited-links ()
  "Write visited links to `hackernews-visited-links-file'."
  (when hackernews-visited-links-file
    (condition-case err
        (with-temp-file hackernews-visited-links-file
          (let ((dir (file-name-directory hackernews-visited-links-file)))
            ;; Ensure any parent directories exist
            (when dir (make-directory dir t)))
          (hackernews-load-visited-links)
          (hackernews--prin1 hackernews--visited-ids (current-buffer) t))
      (error (lwarn 'hackernews :error
                    "Could not write `hackernews-visited-links-file': %s"
                    (error-message-string err))))))

(defun hackernews--init-visited-links ()
  "Set up tracking of visited links.
Do nothing if `hackernews--visited-ids' is already initialized."
  (unless (cdar hackernews--visited-ids)
    (hackernews-load-visited-links)
    (add-hook 'kill-emacs-hook #'hackernews-save-visited-links)))

(defun hackernews--visit (button fn &optional unvisit)
  "Visit URL of BUTTON by passing it to FN.
If UNVISIT is non-nil, mark BUTTON as unvisited."
  (let* ((id    (button-get button 'id))
         (type  (button-type button))
         (face  (cond (unvisit 'hackernews-face)
                      (hackernews-show-visited-links
                       'hackernews-visited-face)))
         (table (cdr (assq type hackernews--visited-ids)))
         (val   (gethash id table))
         (val   (plist-put val :visited      (not unvisit)))
         (val   (plist-put val :last-visited (current-time)))
         (inhibit-read-only t))
    (puthash id val table)
    (when face
      (button-put button 'face (button-type-get type face))))
  (funcall fn (button-get button 'shr-url)))

(defun hackernews-browse-url-action (button)
  "Pass URL of BUTTON to `browse-url'."
  (hackernews--visit button #'browse-url))

(defun hackernews-button-browse-internal ()
  "Open URL of button under point within Emacs.
The URL is passed to `hackernews-internal-browser-function',
which see."
  (declare (modes hackernews-mode))
  (interactive)
  (hackernews--visit (point) hackernews-internal-browser-function))

(defun hackernews-button-mark-as-visited ()
  "Mark button under point as visited."
  (declare (modes hackernews-mode))
  (interactive)
  (hackernews--visit (point) #'ignore))

(defun hackernews-button-mark-as-unvisited ()
  "Mark button under point as unvisited."
  (declare (modes hackernews-mode))
  (interactive)
  (hackernews--visit (point) #'ignore t))

(defalias 'hackernews--text-button
  ;; Emacs 24.4 was the first to return BEG when it's a string, so
  ;; earlier versions can't return the result of `make-text-button'.
  ;; Emacs 28.1 started modifying a copy of BEG when it's a string, so
  ;; subsequent versions must return the result of `make-text-button'.
  (if (version<= "24.4" emacs-version)
      #'make-text-button
    (lambda (beg end &rest properties)
      (apply #'make-text-button beg end properties)
      beg))
  "Like `make-text-button', but always return BEG.
This is for compatibility with various Emacs versions.
\n(fn BEG END &rest PROPERTIES)")

(defun hackernews--button-string (type label url id)
  "Make LABEL a text button of TYPE for item ID and URL."
  (let* ((props (and hackernews-show-visited-links
                     (gethash id (cdr (assq type hackernews--visited-ids)))))
         (face  (button-type-get type (if (plist-get props :visited)
                                          'hackernews-visited-face
                                        'hackernews-face))))
    (hackernews--text-button label nil
                             'type type 'face face
                             'id id 'help-echo url 'shr-url url)))

(autoload 'xml-substitute-special "xml")

(defun hackernews--render-item-classic (item)
  "Render Hacker News ITEM in current buffer using classic format.
The user options `hackernews-score-format',
`hackernews-title-format' and `hackernews-comments-format'
control how each of the ITEM's score, title and comments count
are formatted, respectively.  These components are then combined
according to `hackernews-item-format'.  The title and comments
counts are rendered as text buttons which are hyperlinked to
their respective URLs."
  (let* ((id           (cdr (assq 'id          item)))
         (title        (cdr (assq 'title       item)))
         (score        (cdr (assq 'score       item)))
         (item-url     (cdr (assq 'url         item)))
         (descendants  (cdr (assq 'descendants item)))
         (comments-url (hackernews--comments-url id))
         (item-start   (point)))
    (setq title (xml-substitute-special title))
    (insert
     (format-spec hackernews-item-format
                  `((?s . ,(propertize (format hackernews-score-format score)
                                       'face 'hackernews-score))
                    (?t . ,(hackernews--button-string
                            'hackernews-link
                            (format hackernews-title-format title)
                            (or item-url comments-url)
                            id))
                    (?c . ,(hackernews--button-string
                            'hackernews-comment-count
                            (format hackernews-comments-format
                                    (or descendants 0))
                            comments-url
                            id)))))
    ;; Add text property for unified navigation
    (put-text-property item-start (point) 'hackernews-item-id id)))

(defun hackernews--render-item-modern (item)
  "Render Hacker News ITEM in current buffer using modern UI.
The item is displayed with interactive widgets, styled text with
faces, and visual separators for improved readability."
  (let* ((id           (cdr (assq 'id          item)))
         (title        (cdr (assq 'title       item)))
         (score        (cdr (assq 'score       item)))
         (by           (cdr (assq 'by          item)))
         (item-url     (cdr (assq 'url         item)))
         (descendants  (cdr (assq 'descendants item)))
         (comments-url (hackernews--comments-url id))
         (item-start   (point)))
    (setq title (xml-substitute-special title))

    ;; Title (make it a clickable widget)
    (widget-create 'push-button
                   :notify (lambda (&rest _)
                             (browse-url (or item-url comments-url)))
                   :help-echo (if item-url
                                  (format "Open: %s" item-url)
                                "No URL")
                   :format "%[%v%]"
                   title)

    (insert "\n")

    ;; Score, comments button, and author info
    (insert (propertize "  " 'face 'default))
    (insert (propertize (format "↑%d" (or score 0))
                        'face 'hackernews-score-modern))
    (insert " | ")

    ;; Comments as clickable button
    (widget-create 'push-button
                   :notify (lambda (&rest _)
                             (browse-url comments-url))
                   :help-echo (format "View comments: %s" comments-url)
                   :format "%[%v%]"
                   (format "%s%d comment%s"
                           (if hackernews-enable-emojis "💬 " "")
                           (or descendants 0)
                           (if (= (or descendants 0) 1) "" "s")))

    ;; Author
    (when by
      (insert " | by ")
      (insert (propertize by 'face 'hackernews-author)))

    (insert "\n")
    (hackernews--insert-separator)

    ;; Mark the entire item range with a text property for navigation
    (put-text-property item-start (point) 'hackernews-item-id id)))

(defun hackernews--render-item (item)
  "Render Hacker News ITEM in current buffer.
The rendering style is determined by `hackernews-ui-style'."
  (pcase hackernews-ui-style
    ('classic (hackernews--render-item-classic item))
    ('modern  (hackernews--render-item-modern item))
    (_        (hackernews--render-item-classic item))))




(defun hackernews--display-items ()
  "Render items associated with, and pop to, the current buffer."
  (let* ((reg          (hackernews--get :register))
         (items        (hackernews--get :items))
         (nitem        (length items))
         (feed         (hackernews--get :feed))
         (feed-name    (hackernews--feed-name feed))
         (is-first-load (= (buffer-size) 0))
         (is-modern    (eq hackernews-ui-style 'modern))
         (inhibit-read-only t))

    ;; Insert header for modern UI on first load
    (when (and is-first-load is-modern)
      (hackernews--insert-header feed-name))

    ;; Render items (filter out null, deleted, and dead items)
    (run-hooks 'hackernews-before-render-hook)
    (save-excursion
      (goto-char (point-max))
      (mapc #'hackernews--render-item
            (cl-remove-if (lambda (item)
                            (or (eq item :null)
                                (cdr (assq 'deleted item))
                                (cdr (assq 'dead item))))
                          items)))
    (run-hooks 'hackernews-after-render-hook)

    ;; Setup widgets for modern UI
    (when is-modern
      ;; Create a composed keymap that combines widget functionality with mode bindings
      ;; Priority: widget-keymap (for widget navigation) > hackernews-mode-map > special-mode-map
      (use-local-map (make-composed-keymap (list widget-keymap hackernews-mode-map)
                                            special-mode-map))
      (widget-setup))

    ;; Enable visual-fill-column for modern UI
    (when is-modern
      (when (and (require 'visual-fill-column nil t)
                 (boundp 'visual-fill-column-width))
        (setq-local visual-fill-column-width hackernews-display-width)
        (setq-local visual-fill-column-center-text t)
        (visual-fill-column-mode 1)))

    ;; Disable line numbers
    (when (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode 0))

    ;; Adjust point
    (cond
     ;; First load with modern UI: jump to first widget
     ((and is-first-load is-modern)
      (goto-char (point-min))
      (widget-forward 1))
     ;; First load with classic UI: jump to first item
     (is-first-load
      (hackernews-first-item))
     ;; Loading more items: move to first new item unless preserving point
     ((not (or (<= nitem 0) hackernews-preserve-point))
      (goto-char (point-max))
      (hackernews-previous-item nitem)))

    ;; Persist new offset
    (setcar reg (+ (car reg) nitem)))

  ;; Enable read-only mode after all modifications
  (when (eq hackernews-ui-style 'modern)
    (read-only-mode 1))

  ;; Display buffer with appropriate action based on UI style
  (if (eq hackernews-ui-style 'modern)
      ;; Modern UI: occupy full window
      (pop-to-buffer (current-buffer) '((display-buffer-same-window)))
    ;; Classic UI: default behavior (partial window)
    (pop-to-buffer (current-buffer) '(() (category . hackernews))))
  (run-hooks 'hackernews-finalize-hook))


;; TODO: Derive from `tabulated-list-mode'?
(define-derived-mode hackernews-mode special-mode "HN"
  "Mode for browsing Hacker News.

Summary of key bindings:

Key		Binding
---		-------
\\<hackernews-button-map>
\\[push-button]\
		Open link at point in default (external) browser.
\\[hackernews-button-browse-internal]\
		Open link at point in text-based browser within Emacs.
\\<hackernews-mode-map>
\\[hackernews-next-item]\
		Move to next title link.
\\[hackernews-previous-item]\
		Move to previous title link.
\\[hackernews-next-comment]\
		Move to next comments count link.
\\[hackernews-previous-comment]\
		Move to previous comments count link.
\\[hackernews-load-more-stories]\
		Load more stories.
\\[hackernews-reload]\
		Reload stories.
\\[hackernews-switch-feed]\
		Prompt user for a feed to switch to.
\\<special-mode-map>\\[quit-window]\
		Quit.

Official major mode key bindings:
\\{hackernews-mode-map}"
  :interactive nil
  (setq hackernews--feed-state ())
  (setq truncate-lines t)
  (buffer-disable-undo))


(defun hackernews--ensure-major-mode ()
  "Barf if current buffer is not derived from `hackernews-mode'."
  (unless (derived-mode-p #'hackernews-mode)
    (signal 'hackernews-error '("Not a hackernews buffer"))))

;;;; Retrieval

;; At top level for Emacs < 24.4.
(defvar json-array-type)
(defvar json-object-type)
(declare-function json-read "json" ())

(defalias 'hackernews--parse-json
  (if (fboundp 'json-parse-buffer)
      (lambda ()
        (json-parse-buffer :object-type 'alist))
    (require 'json)
    (lambda ()
      (let ((json-array-type  'vector)
            (json-object-type 'alist))
        (json-read))))
  "Read JSON object from current buffer starting at point.
Objects are decoded as alists and arrays as vectors.")

(defun hackernews--read-contents (url)
  "Retrieve and read URL contents with `hackernews--parse-json'."
  (with-temp-buffer
    (let ((url-show-status (unless hackernews-suppress-url-status
                             url-show-status)))
      (url-insert-file-contents url)
      (hackernews--parse-json))))

(defun hackernews--retrieve-items ()
  "Retrieve items associated with current buffer."
  (let* ((items  (hackernews--get :items))
         (reg    (hackernews--get :register))
         (nitem  (length items))
         (offset (car reg))
         (ids    (cdr reg)))
    (dotimes-with-progress-reporter (i nitem)
        (format "Retrieving %d %s..."
                nitem (hackernews--feed-name (hackernews--get :feed)))
      (aset items i (hackernews--read-contents
                     (hackernews--item-url (aref ids (+ offset i))))))))

(defun hackernews--load-stories (feed n &optional append)
  "Retrieve and render at most N items from FEED.
Create and setup corresponding hackernews buffer if necessary.

If APPEND is nil, refresh the list of items from FEED and render
at most N of its top items.  Any previous hackernews buffer
contents are overwritten.

Otherwise, APPEND should be a cons cell (OFFSET . IDS), where IDS
is the vector of item IDs corresponding to FEED and OFFSET
indicates where in IDS the previous retrieval and render left
off.  At most N of FEED's items starting at OFFSET are then
rendered at the end of the hackernews buffer."
  ;; TODO: * Allow negative N?
  ;;       * Make asynchronous?
  (hackernews--init-visited-links)
  (let* ((name   (hackernews--feed-name feed))
         (offset (or (car append) 0))
         (ids    (if append
                     (cdr append)
                   ;; Display initial progress message before blocking
                   ;; to retrieve ID vector
                   (message "Retrieving %s..." name)
                   (hackernews--read-contents (hackernews--feed-url feed)))))

    (with-current-buffer (get-buffer-create (format "*hackernews %s*" name))
      (unless append
        ;; Clear buffer
        (let ((inhibit-read-only t))
          (erase-buffer))
        (remove-overlays)

        ;; Activate hackernews-mode (which calls kill-all-local-variables)
        (hackernews-mode))

      (hackernews--put :feed     feed)
      (hackernews--put :register (cons offset ids))
      (hackernews--put :items    (make-vector
                                  (max 0 (min (- (length ids) offset)
                                              (if n
                                                  (prefix-numeric-value n)
                                                hackernews-items-per-page)))
                                  ()))

      (hackernews--retrieve-items)
      (hackernews--display-items))))

;;;; Feeds

;;;###autoload
(defun hackernews (&optional n)
  "Read top N Hacker News stories.
The Hacker News feed is determined by `hackernews-default-feed'
and N defaults to `hackernews-items-per-page'."
  (interactive "P")
  (hackernews--load-stories hackernews-default-feed n))

(defun hackernews-reload (&optional n)
  "Reload top N Hacker News stories from current feed.
N defaults to `hackernews-items-per-page'."
  (declare (modes hackernews-mode))
  (interactive "P")
  (hackernews--ensure-major-mode)
  (hackernews--load-stories
   (or (hackernews--get :feed)
       (signal 'hackernews-error '("Buffer unassociated with feed")))
   n))

(defun hackernews-load-more-stories (&optional n)
  "Load N more stories into hackernews buffer.
N defaults to `hackernews-items-per-page'."
  (declare (modes hackernews-mode))
  (interactive "P")
  (hackernews--ensure-major-mode)
  (let ((feed (hackernews--get :feed))
        (reg  (hackernews--get :register)))
    (unless (and feed reg)
      (signal 'hackernews-error '("Buffer in invalid state")))
    (if (>= (car reg) (length (cdr reg)))
        (message "%s" (substitute-command-keys "\
End of feed; type \\[hackernews-reload] to load new items."))
      (hackernews--load-stories feed n reg))))

(defalias 'hackernews--prompt
  (if (fboundp 'format-prompt)
      #'format-prompt
    (lambda (prompt default)
      (format "%s (default %s): " prompt default)))
  "Compatibility shim for `format-prompt' in Emacs < 28.
\n(fn PROMPT DEFAULT)")

(defun hackernews-switch-feed (&optional n)
  "Read top N Hacker News stories from a different feed.
The Hacker News feed is determined by the user with completion
and N defaults to `hackernews-items-per-page'."
  (interactive "P")
  (hackernews--load-stories
   (let ((completion-extra-properties
          (list :annotation-function #'hackernews--feed-annotation)))
     (completing-read
      (hackernews--prompt "Hacker News feed" hackernews-default-feed)
      hackernews-feed-names nil t nil 'hackernews-feed-history
      hackernews-default-feed))
   n))

(defun hackernews-top-stories (&optional n)
  "Read top N Hacker News Top Stories.
N defaults to `hackernews-items-per-page'."
  (interactive "P")
  (hackernews--load-stories "top" n))

(defun hackernews-new-stories (&optional n)
  "Read top N Hacker News New Stories.
N defaults to `hackernews-items-per-page'."
  (interactive "P")
  (hackernews--load-stories "new" n))

(defun hackernews-best-stories (&optional n)
  "Read top N Hacker News Best Stories.
N defaults to `hackernews-items-per-page'."
  (interactive "P")
  (hackernews--load-stories "best" n))

(defun hackernews-ask-stories (&optional n)
  "Read top N Hacker News Ask Stories.
N defaults to `hackernews-items-per-page'."
  (interactive "P")
  (hackernews--load-stories "ask" n))

(defun hackernews-show-stories (&optional n)
  "Read top N Hacker News Show Stories.
N defaults to `hackernews-items-per-page'."
  (interactive "P")
  (hackernews--load-stories "show" n))

(defun hackernews-job-stories (&optional n)
  "Read top N Hacker News Job Stories.
N defaults to `hackernews-items-per-page'."
  (interactive "P")
  (hackernews--load-stories "job" n))

(provide 'hackernews)

;;; hackernews.el ends here
