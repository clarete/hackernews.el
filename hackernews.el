;;; hackernews.el --- Hacker News Client for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2012-2018 Lincoln de Sousa <lincoln@comum.org>

;; Author: Lincoln de Sousa <lincoln@comum.org>
;;         Basil L. Contovounesios <contovob@tcd.ie>
;; Keywords: comm hypermedia news
;; Version: 0.4.0
;; Homepage: https://github.com/clarete/hackernews.el

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
(require 'json)
(require 'url)

(defgroup hackernews nil
  "Simple Hacker News client."
  :group 'external
  :prefix "hackernews-")

;;; Faces

(define-obsolete-face-alias 'hackernews-link-face
  'hackernews-link "0.4.0")

(defface hackernews-link
  '((t :inherit link :underline nil))
  "Face used for links to stories."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews)

(defface hackernews-link-visited
  '((t :inherit link-visited :underline nil))
  "Face used for links to stories."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews)

(define-obsolete-face-alias 'hackernews-comment-count-face
  'hackernews-comment-count "0.4.0")

(defface hackernews-comment-count
  '((t :inherit hackernews-link))
  "Face used for comment counts."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews)

(defface hackernews-comment-count-visited
  '((t :inherit hackernews-link))
  "Face used for comment counts."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews)

(define-obsolete-face-alias 'hackernews-score-face
  'hackernews-score "0.4.0")

(defface hackernews-score
  '((t :inherit default))
  "Face used for the score of a story."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews)

;;; User options

(define-obsolete-variable-alias 'hackernews-top-story-limit
  'hackernews-items-per-page "0.4.0")

(defcustom hackernews-items-per-page 20
  "Default number of stories to retrieve in one go."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews
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
(put 'hackernews-feed-names 'risky-local-variable t)

(defcustom hackernews-default-feed "top"
  "Default story feed to load.
See `hackernews-feed-names' for supported feed types."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews
  :type (cons 'radio (mapcar (lambda (feed)
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
  :group 'hackernews
  :type 'string)

(defcustom hackernews-score-format "[%s]"
  "Format specification for displaying the score of an item.
The result is obtained by passing this string and the score count
to `format'."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews
  :type 'string)

(defcustom hackernews-title-format "%s"
  "Format specification for displaying the title of an item.
The result is obtained by passing this string and the title to
`format'."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews
  :type 'string)

(defcustom hackernews-comments-format "(%s comments)"
  "Format specification for displaying the comments of an item.
The result is obtained by passing this string and the comments
count to `format'."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews
  :type 'string)

(defcustom hackernews-preserve-point t
  "Whether to preserve point when loading more stories.
When nil, point is placed on first new item retrieved."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews
  :type 'boolean)

(defcustom hackernews-before-render-hook ()
  "Hook called before rendering any new items."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews
  :type 'hook)

(defcustom hackernews-after-render-hook ()
  "Hook called after rendering any new items.
The position of point will not have been affected by the render."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews
  :type 'hook)

(defcustom hackernews-finalize-hook ()
  "Hook called as final step of loading any new items.
The position of point may have been adjusted after the render,
buffer-local feed state will have been updated and the hackernews
buffer will be current and displayed in the selected window."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews
  :type 'hook)

(defcustom hackernews-suppress-url-status t
  "Whether to suppress messages controlled by `url-show-status'.
When nil, `url-show-status' determines whether certain status
messages are displayed when retrieving online data.  This is
suppressed by default so that the hackernews progress reporter is
not interrupted."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews
  :type 'boolean)

(defcustom hackernews-internal-browser-function
  (if (functionp 'eww-browse-url)
      #'eww-browse-url
    #'browse-url-text-emacs)
  "Function to load a given URL within Emacs.
See `browse-url-browser-function' for some possible options."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews
  :type (cons 'radio (butlast (cdr (custom-variable-type
                                    'browse-url-browser-function)))))

;;; Internal definitions

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
    (define-key map "t" #'hackernews-button-browse-internal)
    map)
  "Keymap used on hackernews links.")

(define-button-type 'hackernews-link
  'action                    #'hackernews-browse-url-action
  'face                      'hackernews-link
  'follow-link               t
  'hackernews-visited-ids    '()
  'hackernews-visited-button 'hackernews-link-visited
  'keymap                    hackernews-button-map)

(define-button-type 'hackernews-link-visited
  'face      'hackernews-link-visited
  'supertype 'hackernews-link)

(define-button-type 'hackernews-comment-count
  'face                      'hackernews-comment-count
  'hackernews-visited-ids    '()
  'hackernews-visited-button 'hackernews-comment-count-visited
  'supertype                 'hackernews-link)

(define-button-type 'hackernews-comment-count-visited
  'face      'hackernews-comment-count-visited
  'supertype 'hackernews-comment-count)

;; Emulate `define-error'
(put 'hackernews-error 'error-conditions '(hackernews-error error))
(put 'hackernews-error 'error-message    "Hackernews error")

;;; Utils

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

(defalias 'hackernews--signum
  (if (and (require 'cl-lib nil t)
           (fboundp 'cl-signum))
      #'cl-signum
    (lambda (x)
      (cond ((> x 0)  1)
            ((< x 0) -1)
            (t        0))))
  "Compatibility shim for `cl-signum'.")


;;; Motion

(defun hackernews--forward-button (n type)
  "Move to Nth next button of TYPE (previous if N is negative)."
  (let ((pos  (point))
        (sign (hackernews--signum n))
        msg)
    (while (let ((button (ignore-errors (forward-button sign))))
             (when button
               (when (eq (button-type button) type)
                 (setq pos (button-start button))
                 (setq msg (button-get button 'help-echo))
                 (setq n   (- n sign)))
               (/= n 0))))
    (goto-char pos)
    (when msg (message "%s" msg))))

(defun hackernews-next-item (&optional n)
  "Move to Nth next story link (previous if N is negative).
N defaults to 1."
  (interactive "p")
  ;; N is kept optional for backward compatibility
  (hackernews--forward-button (or n 1) 'hackernews-link))

(defun hackernews-previous-item (&optional n)
  "Move to Nth previous story link (next if N is negative).
N defaults to 1."
  (interactive "p")
  (hackernews-next-item (- (or n 1))))

(defun hackernews-next-comment (&optional n)
  "Move to Nth next comments link (previous if N is negative).
N defaults to 1."
  (interactive "p")
  (hackernews--forward-button (or n 1) 'hackernews-comment-count))

(defun hackernews-previous-comment (&optional n)
  "Move to Nth previous comments link (next if N is negative).
N defaults to 1."
  (interactive "p")
  (hackernews-next-comment (- (or n 1))))

(defun hackernews-first-item ()
  "Move point to first story link in hackernews buffer."
  (interactive)
  (goto-char (point-min))
  (hackernews-next-item))

;;; UI

(defun hackernews-browse-url-action (button)
  "Pass URL of BUTTON to `browse-url'."
  (flet ((bget (prop) (button-get button prop)))
    (let ((id (bget 'id))
	  (url (bget 'shr-url))
	  (button-type (bget 'type)))
      (button-type-put button-type 'hackernews-visited-ids (cons id (button-type-get button-type 'hackernews-visited-ids)))
      (browse-url url))))

(defun hackernews-button-browse-internal (button)
  "Open URL of button under point within Emacs.
The URL is passed to `hackernews-internal-browser-function',
which see."
  (interactive)
  (flet ((bget (prop) (button-get button prop)))
    (let ((id (bget 'id))
	  (url (bget 'shr-url))
	  (button-type (bget 'type)))
      (button-type-put button-type 'hackernews-visited-ids (cons id (button-type-get button-type 'hackernews-visited-ids)))
      (funcall hackernews-internal-browser-function url))))

(defun hackernews--button-string (type label url id)
  "Return button string of TYPE pointing to URL with LABEL."
  (make-text-button label nil 'type type 'help-echo url 'shr-url url 'id id)
  label)

(defun hackernews--render-item (item)
  "Render Hacker News ITEM in current buffer.
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
         (comments-url (hackernews--comments-url id)))
    (insert
     (format-spec hackernews-item-format
                  (format-spec-make
                   ?s (propertize (format hackernews-score-format score)
                                  'face 'hackernews-score)
                   ?t (hackernews--button-string
                       (if (member id (button-type-get 'hackernews-link 'hackernews-visited-ids))
			   'hackernews-link-visited
			 'hackernews-link)
                       (format hackernews-title-format title)
                       (or item-url comments-url)
		       id)
                   ?c (hackernews--button-string
		       (if (member id (button-type-get 'hackernews-comment-count 'hackernews-visited-ids))
			   'hackernews-comment-count-visited
			 'hackernews-comment-count)
                       (format hackernews-comments-format (or descendants 0))
                       comments-url
		       id))))))

(defun hackernews--display-items ()
  "Render items associated with, and pop to, the current buffer."
  (let* ((reg   (hackernews--get :register))
         (items (hackernews--get :items))
         (nitem (length items))
         (inhibit-read-only t))

    ;; Render items
    (run-hooks 'hackernews-before-render-hook)
    (save-excursion
      (goto-char (point-max))
      (mapc #'hackernews--render-item items))
    (run-hooks 'hackernews-after-render-hook)

    ;; Adjust point
    (unless (or (<= nitem 0) hackernews-preserve-point)
      (goto-char (point-max))
      (hackernews-previous-item nitem))

    ;; Persist new offset
    (setcar reg (+ (car reg) nitem)))

  (pop-to-buffer (current-buffer))
  (run-hooks 'hackernews-finalize-hook))

;; TODO: Derive from `tabulated-list-mode'?
(define-derived-mode hackernews-mode special-mode "HN"
  "Mode for browsing Hacker News.

Summary of key bindings:

key		binding
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
\\[quit-window]\
		Quit.

Official major mode key bindings:

\\{hackernews-mode-map}"
  :group 'hackernews
  (setq hackernews--feed-state ())
  (setq truncate-lines t)
  (buffer-disable-undo))

(defun hackernews--ensure-major-mode ()
  "Barf if current buffer is not derived from `hackernews-mode'."
  (unless (derived-mode-p #'hackernews-mode)
    (signal 'hackernews-error '("Not a hackernews buffer"))))

;;; Retrieval

(defalias 'hackernews--parse-json
  (if (fboundp 'json-parse-buffer)
      (lambda ()
        (json-parse-buffer :object-type 'alist))
    (lambda ()
      (let ((json-object-type 'alist)
            (json-array-type  'vector))
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
        (let ((inhibit-read-only t))
          (erase-buffer))
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

;;; Feeds

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
  (interactive "P")
  (hackernews--ensure-major-mode)
  (let ((feed (hackernews--get :feed)))
    (if feed
        (hackernews--load-stories feed n)
      (signal 'hackernews-error '("Buffer unassociated with feed")))))

(defun hackernews-load-more-stories (&optional n)
  "Load N more stories into hackernews buffer.
N defaults to `hackernews-items-per-page'."
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

(defun hackernews-switch-feed (&optional n)
  "Read top N Hacker News stories from a different feed.
The Hacker News feed is determined by the user with completion
and N defaults to `hackernews-items-per-page'."
  (interactive "P")
  (hackernews--load-stories
   (let ((completion-extra-properties
          (list :annotation-function #'hackernews--feed-annotation)))
     (completing-read
      (format "Hacker News feed (default %s): " hackernews-default-feed)
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
