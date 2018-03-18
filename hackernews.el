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

;; FIXME: Require only when necessary
(require 'browse-url)
(require 'cus-edit)
(require 'format-spec)
(require 'json)
(require 'mm-url)
(require 'url)
(require 'url-handlers)
(require 'url-queue nil t)              ; Added in Emacs 24.1

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
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews)

(define-obsolete-face-alias 'hackernews-comment-count-face
  'hackernews-comment-count "0.4.0")

(defface hackernews-comment-count
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

;;;; User options

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
  "Map feed types as strings to their display names.")
(put 'hackernews-feed-names 'risky-local-variable t)

(defcustom hackernews-default-feed "top"
  "Default story feed to load.
See `hackernews-feed-names' for supported feed types."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews
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

(defvar hackernews-backends
  '((url-queue
     :ids   hackernews-url-queue-ids
     :items hackernews-url-queue-items
     :doc   "\
Asynchronous retrieval using the `url-queue' library.")
    (url-chain
     :ids   hackernews-url-chain-ids
     :items hackernews-url-chain-items
     :doc   "\
Asynchronous retrieval using the `url' library.
Emulates `url-queue' by chaining `url-retrieve' callbacks.")
    (url-sync
     :ids   hackernews-url-sync-ids
     :items hackernews-url-sync-items
     :doc   "\
Synchronous retrieval using the `url' library.")
    (mm-async
     :ids   hackernews-mm-async-ids
     :items hackernews-mm-async-items
     :doc   "\
Asynchronous retrieval using the `mm-url' library.
Like `url-chain', but with sentinel, not callback, chains.")
    (mm-asyncs
     :ids   hackernews-mm-async-ids
     :items hackernews-mm-asyncs-items
     :doc   "\
Asynchronous retrieval using the `mm-url' library.
This backend is significantly faster than `mm-async' by passing
multiple URLs to each subprocess, thus spawning fewer
overall (the trailing 's' in `mm-asyncs' alludes to both
plurality and speed).  It is not enabled by default because it
renders the progress reporter far less, if at all, effective, and
some `mm-url-predefined-programs', such as `lynx', do not accept
multiple URL arguments.")
    (mm-sync
     :ids   hackernews-mm-sync-ids
     :items hackernews-mm-sync-items
     :doc   "\
Synchronous retrieval using the `mm-url' library.")
    (mm-syncs
     :ids   hackernews-mm-sync-ids
     :items hackernews-mm-syncs-items
     :doc   "\
Synchronous retrieval using the `mm-url' library.
This backend is to `mm-sync' what `mm-asyncs' (which see) is to
`mm-async'."))
  "Map retrieval backends to their property lists.
The following properties are currently understood:
:ids   - Function for retrieving the IDs of a given buffer.
:items - Function for retrieving the items of a given buffer.
:doc   - Backend description.")

(defcustom hackernews-backend (if (featurep 'url-queue)
                                  'url-queue
                                'mm-async)
  "Online retrieval backend.
Default to using the `url-queue' library when available, i.e. on
Emacs 24.1 and newer.  Otherwise, use an external grabber, such
as `wget', by way of the `mm-url' library.  Both of these
defaults are asynchronous and their libraries customizable.

The older your version of Emacs, the more you are discouraged
from relying on any of the `url-*' backends.  Emacs 23 and its
version of the `url' library, in particular, are notorious for
barely working at all with the Hacker News API.

You should typically only need to set `hackernews-backend' once
for all retrievals, but using a different value for each
hackernews invocation is also supported.

See `hackernews-backends' for the backends available by default
and how to register custom backends.  See also
`hackernews-async-processes' for controlling the parallelism of
asynchronous backends."
  :group 'hackernews
  :link '(custom-group-link mm-url)
  :link '(custom-group-link url)
  :type (cons 'radio (mapcar (lambda (backend)
                               (list 'const :doc (plist-get (cdr backend) :doc)
                                     (car backend)))
                             hackernews-backends)))

(defcustom hackernews-async-processes
  (or (bound-and-true-p url-queue-parallel-processes) 6)
  "Maximum number of asynchronous processes to use for retrieval.
This user option applies to all asynchronous backends provided by
default in `hackernews-backends' except `url-queue', which has
its own setting for this, namely `url-queue-parallel-processes'."
  :group 'hackernews
  :type 'integer)

(defcustom hackernews-suppress-url-status t
  "Whether to suppress `url' library progress messages.
When `hackernews-backend' is set to one of the default `url-*'
backends, this user option determines whether to suppress
messages controlled by `url-show-status' and also acts as the
SILENT argument to `url' retrieval functions such as
`url-retrieve'.  The corresponding messages are suppressed by
default so that the hackernews progress reporter is not
interrupted, as well as for general noise reduction in the echo
area."
  :package-version '(hackernews . "0.4.0")
  :group 'hackernews
  :type 'boolean)

(defcustom hackernews-report-progress t
  "Whether to display a progress reporter during retrieval.
This is enabled by default so as to provide more feedback to the
user.  You may want to disable this when `hackernews-backend' is
asynchronous, so as to free up the echo area for other purposes."
  :group 'hackernews
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

(defvar hackernews-state ()
  "Plist capturing state of a Hacker News feed.
:feed    - Type of endpoint feed; see `hackernews-feed-names'.
:buffer  - Buffer displaying items and holding state.
:backend - Retrieval backend plist; see `hackernews-backends'.
:journo  - Progress reporter.
:ids     - Vector of item IDs last read from this feed.
:offset  - Number of items currently displayed.
           This is an index into :ids.
:items   - Vector holding items being or last fetched.
:nitem   - Number of items to be or already fetched.
           Counts down to zero, and is used to update :journo,
           during retrieval.")
(make-variable-buffer-local 'hackernews-state)

(defvar hackernews-feed-history ()
  "Completion history of hackernews feeds switched to.")

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
  "Keymap used in hackernews buffers.")

(define-obsolete-variable-alias 'hackernews-map
  'hackernews-mode-map "0.4.0")

(defvar hackernews-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map "t" #'hackernews-button-browse-internal)
    map)
  "Keymap used on hackernews links.")

(define-button-type 'hackernews-link
  'action      #'hackernews-browse-url-action
  'face        'hackernews-link
  'follow-link t
  'keymap      hackernews-button-map)

(define-button-type 'hackernews-comment-count
  'face      'hackernews-comment-count
  'supertype 'hackernews-link)

;; Emulate `define-error'
(put 'hackernews-error 'error-conditions '(hackernews-error error))
(put 'hackernews-error 'error-message    "Hackernews error")

;;;; Utils

(defun hackernews--error (format &rest args)
  "Like `error', but signal error symbol `hackernews-error'."
  (signal 'hackernews-error
          (list (apply (if (functionp 'format-message)
                           #'format-message
                         #'format)
                       format args))))

(defun hackernews--comments-url (id)
  "Return Hacker News website URL for item with ID."
  (format hackernews-site-item-format id))

(defun hackernews--format-api-url (format &rest args)
  "Construct a Hacker News API URL.
The result of passing FORMAT and ARGS to `format' is substituted
in `hackernews-api-format'."
  (format hackernews-api-format (apply #'format format args)))

(defun hackernews--item-url (id)
  "Return Hacker News API URL for item with ID."
  (hackernews--format-api-url "item/%s" id))

(defun hackernews--feed-url (feed)
  "Return Hacker News API URL for FEED.
See `hackernews-feed-names' for possible values of FEED."
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

(defun hackernews--reads (writer &rest args)
  ""
  (with-temp-buffer
    (apply writer args)
    (goto-char (point-min))
    (let (objects)
      (while (progn (push (hackernews--parse-json) objects)
                    (not (eobp))))
      (nreverse objects))))

;; TODO: Remove?
(defun hackernews--read (writer &rest args)
  "Like `hackernews--reads', but return the first object read."
  (car (apply #'hackernews--reads writer args)))

(defalias 'hackernews--signum
  (if (and (require 'cl-lib nil t)
           (fboundp 'cl-signum))
      #'cl-signum
    (lambda (x)
      (cond ((> x 0)  1)
            ((< x 0) -1)
            (t        0))))
  "Compatibility shim for `cl-signum'.")

;;;; Motion

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
  "Move point to first story link in current buffer."
  (interactive)
  (goto-char (point-min))
  (hackernews-next-item))

;;;; UI

(defun hackernews-browse-url-action (button)
  "Pass URL of BUTTON to `browse-url'."
  (browse-url (button-get button 'shr-url)))

(defun hackernews-button-browse-internal ()
  "Open URL of button under point within Emacs.
The URL is passed to `hackernews-internal-browser-function',
which see."
  (interactive)
  (funcall hackernews-internal-browser-function
           (button-get (button-at (point)) 'shr-url)))

(defun hackernews--button-string (type label url)
  "Return button string of TYPE pointing to URL with LABEL."
  (make-text-button label nil 'type type 'help-echo url 'shr-url url)
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
                       'hackernews-link
                       (format hackernews-title-format title)
                       (or item-url comments-url))
                   ?c (hackernews--button-string
                       'hackernews-comment-count
                       (format hackernews-comments-format (or descendants 0))
                       comments-url))))))

(defun hackernews--render (state)
  "Render items in and pop to hackernews buffer stored in STATE."
  (with-current-buffer (plist-get state :buffer)
    (let* ((items (plist-get state :items))
           (nitem (length items))
           ;; Allow hooks to modify buffer as well
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
      ;; Done
      (pop-to-buffer (current-buffer))
      (run-hooks 'hackernews-finalize-hook))))

;; TODO: Derive from `tabulated-list-mode'?
(define-derived-mode hackernews-mode special-mode "HN"
  "Mode for browsing Hacker News.

\\{hackernews-mode-map}"
  :group 'hackernews
  ;; We could initialize `hackernews-state' to nil here and reset
  ;; `:offset' elsewhere, but initializing to a non-empty plist has
  ;; the benefit of allowing direct modification by `plist-put'.
  (setq hackernews-state (list :offset 0))
  (setq truncate-lines t)
  (buffer-disable-undo))

(defun hackernews--ensure-major-mode ()
  "Barf if current buffer is not derived from `hackernews-mode'."
  (unless (derived-mode-p #'hackernews-mode)
    (hackernews--error "Not a hackernews buffer")))

;;;; Retrieval

(defun hackernews--maybe-done (state)
  ""
  (when (<= (plist-get state :nitem) 0)
    (let ((journo (plist-get state :journo)))
      (when journo (progress-reporter-done journo)))
    (plist-put state :offset (+ (plist-get state :offset)
                                (length (plist-get state :items))))
    (hackernews--render state)))

(defun hackernews--store-items (plist &rest items)
  "Store ITEMS in PLIST's :state.
This function is intended as a generic asynchronous callback."
  (let* ((state   (plist-get plist :state))
         (journo  (plist-get state :journo))
         (itemvec (plist-get state :items))
         (nitem   (- (plist-get state :nitem)
                     (length items))))
    (dolist (index (plist-get plist :indices))
      (aset itemvec index (pop items)))
    (when journo (progress-reporter-update journo (- nitem)))
    (plist-put state :nitem nitem)
    (hackernews--maybe-done state)))

(defun hackernews--dispatch (job state)
  "[REWORD] Partition BUFFER's items evenly across a vector.
The length of the resultant vector is between 1 and
`hackernews-async-processes', inclusive.  Each of its elements is
a list of cells (INDEX . URL), where URL specifies a Hacker News
item to be fetched and INDEX determines its ordering in BUFFER's
items vector."
  (hackernews--maybe-done state)
  (let* ((ids    (plist-get state :ids))
         (offset (plist-get state :offset))
         (nitem  (plist-get state :nitem))
         (njob   (max 1 (min nitem hackernews-async-processes)))
         (jobs   (make-vector njob ())))
    (dotimes (i nitem)
      (let ((j (% i njob)))
        (aset jobs j
              (cons (cons i (hackernews--item-url (aref ids (+ offset i))))
                    (aref jobs j)))))
    (mapc (lambda (tasks)
            (funcall job tasks state))
          jobs)))

(defun hackernews--retrieve-items (state)
  "Retrieve and store items in STATE."
  (let ((nitem (max 0 (min (- (length (plist-get state :ids))
                              (plist-get state :offset))
                           (plist-get state :nitem)))))
    (plist-put state :items (make-vector nitem ()))
    (plist-put state :nitem nitem)
    (when (plist-get state :journo)
      (plist-put state :journo (make-progress-reporter
                                (format "Retrieving %d %s..." nitem
                                        (hackernews--feed-name
                                         (plist-get state :feed)))
                                (- nitem) 0))))
  (funcall (plist-get (plist-get state :backend) :items) state))

(defun hackernews--store-ids (plist ids)
  "Store IDS vector in PLIST's :state and start item retrieval.
This function is intended as a generic asynchronous callback."
  (let ((state (plist-get plist :state)))
    (plist-put state :ids ids)
    (hackernews--retrieve-items state)))

(defun hackernews--load-stories (feed n &optional append)
  "Retrieve and render at most N items from FEED.
Create and setup corresponding hackernews buffer if necessary.

If APPEND is nil, refresh the list of items from FEED and render
at most N of its top items, overwriting any previous hackernews
buffer contents.  Otherwise, if APPEND is non-nil, retrieve and
render at most N items starting past the items currently
displayed in the corresponding hackernews buffer."
  ;; TODO: * Allow negative N?
  ;;       * Handle multiple asynchronous invocations of same feed
  ;;       * Support aborting retrievals
  ;;       * Handle quits
  (let* ((name   (hackernews--feed-name feed))
         (buffer (get-buffer-create (format "*hackernews %s*" name))))
    ;; Prepare buffer
    (unless append
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer))
        (hackernews-mode)))
    (let ((state   (buffer-local-value 'hackernews-state buffer))
          (backend (cdr (assq hackernews-backend hackernews-backends))))
      ;; Prepare state
      (plist-put state :feed    feed)
      (plist-put state :buffer  buffer)
      (plist-put state :backend backend)
      (plist-put state :nitem   (if n
                                    (prefix-numeric-value n)
                                  hackernews-items-per-page))
      (if append
          ;; Have IDs; skip to retrieving items subset
          (hackernews--retrieve-items state)
        ;; Retrieve IDs
        (plist-put state :journo (and hackernews-report-progress
                                      (make-progress-reporter
                                       (format "Retrieving %s..." name)
                                       0 0)))
        (funcall (plist-get backend :ids) state)))))

;; `url'

(defalias 'hackernews--url-insert-buffer
  (if (fboundp 'url-insert-buffer-contents)
      #'url-insert-buffer-contents
    (lambda (buffer url &optional _visit _beg _end _replace)
      (save-excursion
        (let ((size-and-charset (url-insert buffer)))
          (kill-buffer buffer)
          (unless (cadr size-and-charset)
            (decode-coding-inserted-region (point-min) (point-max) url))
          (after-insert-file-set-coding (car size-and-charset))))))
  "Compatibility shim for `url-insert-buffer-contents'.

\(fn BUFFER URL)")

(defun hackernews--url-callback (status &rest plist)
  "[REWORD] Callback for `url-retrieve'.
Performs error handling on the STATUS plist before
returning (apply CALLBACK JSON ARGS), where JSON is parsed from
the contents of URL."
  (let ((buf (current-buffer))
        (url (url-recreate-url url-current-object))
        (err (plist-get status :error)))
    (when err
      (kill-buffer buf)
      (hackernews--error "Error retrieving %s: %s" url (cdr err)))
    (funcall (plist-get plist :callback)
             plist
             (hackernews--read #'hackernews--url-insert-buffer buf url))))

;; `url-queue'

(defun hackernews--url-queue-retrieve (url &rest plist)
  "Like `hackernews--url-retrieve', but using `url-queue'."
  (url-queue-retrieve url #'hackernews--url-callback plist
                      hackernews-suppress-url-status))

(defun hackernews-url-queue-ids (state)
  "Asynchronously retrieve and store IDs vector in STATE."
  (hackernews--url-queue-retrieve (hackernews--feed-url (plist-get state :feed))
                                  :callback #'hackernews--store-ids
                                  :state    state))

(defun hackernews--url-queue-job (tasks state)
  ""
  (dolist (task tasks)
    (hackernews--url-queue-retrieve (cdr task)
                                    :callback #'hackernews--store-items
                                    :indices  (list (car task))
                                    :state    state)))

(defun hackernews-url-queue-items (state)
  "Asynchronously retrieve and store items in STATE."
  (hackernews--dispatch #'hackernews--url-queue-job state))

;; `url-chain'

(defun hackernews--url-retrieve (url &rest plist)
  "[REWORD] Retrieve URL asynchronously with `url-retrieve'.
This is a convenience wrapper which passes CALLBACK and ARGS to
`hackernews--url-callback' after retrieval."
  (let ((url-show-status (unless hackernews-suppress-url-status
                           url-show-status)))
    (url-retrieve url #'hackernews--url-callback plist)))

(defun hackernews-url-chain-ids (state)
  "Asynchronously retrieve and store IDs vector in STATE."
  (hackernews--url-retrieve (hackernews--feed-url (plist-get state :feed))
                            :callback #'hackernews--store-ids :state state))

(defun hackernews--url-chain-item (item index tasks state)
  ""
  (hackernews--store-item item index state)
  (when tasks (hackernews--url-chain-job tasks state)))

(defun hackernews--url-chain-job (tasks state)
  ""
  (hackernews--url-retrieve (cdar tasks) #'hackernews--url-chain-item
                            (caar tasks) (cdr tasks) state))

(defun hackernews-url-chain-items (state)
  "Asynchronously retrieve and store items in STATE."
  (hackernews--dispatch #'hackernews--url-chain-job state))

;; `url-sync'

(defun hackernews--url-insert-file (url)
  "Like `url-insert-file-contents', but optionally silent."
  (let ((url-show-status (unless hackernews-suppress-url-status
                           url-show-status)))
    (url-insert-file-contents url)))

(defun hackernews-url-sync-ids (state)
  "Synchronously retrieve and store IDs vector in STATE."
  (hackernews--store-ids (hackernews--read
                          #'hackernews--url-insert-file
                          (hackernews--feed-url (plist-get state :feed)))
                         state))

(defun hackernews--url-sync-job (tasks state)
  ""
  (dolist (task tasks)
    (hackernews--store-item (hackernews--read #'hackernews--url-insert-file
                                              (cdr task))
                            (car task)
                            state)))

(defun hackernews-url-sync-items (state)
  "Synchronously retrieve and store items in STATE."
  (hackernews--dispatch #'hackernews--url-sync-job state))

;; `mm-url'

(defun hackernews--mm-command ()
  "Return list of `mm-url' external program and its arguments."
  (if (symbolp mm-url-program)
      (cdr (assq mm-url-program mm-url-predefined-programs))
    (cons mm-url-program mm-url-arguments)))

(defun hackernews--mm-insert (&rest urls)
  "Synchronously insert contents of URLS before point."
  (let* ((command (hackernews--mm-command))
         (status  (apply #'call-process (car command) nil t nil
                         (append (cdr command) urls))))
    (unless (eq status 0)
      (hackernews--error "Program `%s' returned exit status: %s"
                         (car command) status))))

(defun hackernews--mm-sentinel (process msg)
  ""
  (when (eq (process-status process) 'exit)
    (let ((buffer  (process-buffer process))
          (program (car (process-command process))))
      (unless (zerop (process-exit-status process))
        (hackernews--error "Program `%s' %s" program (substring msg 0 -1)))
      (unless (buffer-live-p buffer)
        (hackernews--error "Program `%s' process buffer killed" program))
      (apply (process-get process :callback)
             (process-plist process)
             (prog1 (hackernews--reads #'insert-buffer-substring buffer)
               (kill-buffer buffer))))))

(defun hackernews--mm-retrieve (plist &rest urls)
  ""
  (let* (process-connection-type
         (name    " *hackernews*")
         (process (apply #'start-process name (generate-new-buffer-name name)
                         (append (hackernews--mm-command) urls))))
    (set-process-sentinel process #'hackernews--mm-sentinel)
    (set-process-plist    process plist)
    process))

;; `mm-async'

(defun hackernews-mm-async-ids (state)
  "Asynchronously retrieve and store IDs vector in STATE."
  (hackernews--mm-retrieve (list :callback #'hackernews--store-ids :state state)
                           (hackernews--feed-url (plist-get state :feed))))

(defun hackernews--mm-async-item (plist item)
  ""
  (let ((state (plist-get plist :state))
        (tasks (plist-get plist :tasks)))
    (hackernews--store-item item (plist-get plist :index) state)
    (when tasks (hackernews--mm-async-job tasks state))))

(defun hackernews--mm-async-job (tasks state)
  ""
  (hackernews--mm-retrieve (list :callback #'hackernews--mm-async-item
                                 :index    (caar tasks)
                                 :tasks    (cdr tasks)
                                 :state    state)
                           (cdar tasks)))

(defun hackernews-mm-async-items (state)
  "Asynchronously retrieve and store items in STATE."
  (hackernews--dispatch #'hackernews--mm-async-job state))

;; `mm-asyncs'

(defun hackernews--mm-store-items (plist &rest items)
  ""
  (let ((state (plist-get plist :state)))
    (dolist (index (plist-get plist :indices))
      (hackernews--store-item (pop items) index state))))

(defun hackernews--mm-asyncs-job (tasks state)
  ""
  (apply #'hackernews--mm-retrieve
         (list :callback #'hackernews--mm-store-items
               :indices  (mapcar #'car tasks)
               :state    state)
         (mapcar #'cdr tasks)))

(defun hackernews-mm-asyncs-items (state)
  "Asynchronously retrieve and store items in STATE."
  (hackernews--dispatch #'hackernews--mm-asyncs-job state))

;; `mm-sync'

(defun hackernews-mm-sync-ids (state)
  "Synchronously retrieve and store IDs vector in STATE."
  (hackernews--store-ids (hackernews--read
                          #'hackernews--mm-insert
                          (hackernews--feed-url (plist-get state :feed)))
                         state))

(defun hackernews--mm-sync-job (tasks state)
  ""
  (dolist (task tasks)
    (hackernews--store-item (hackernews--read #'hackernews--mm-insert
                                              (cdr task))
                            (car task)
                            state)))

(defun hackernews-mm-sync-items (state)
  "Synchronously retrieve and store items in STATE."
  (hackernews--dispatch #'hackernews--mm-sync-job state))

;; `mm-syncs'

(defun hackernews--mm-syncs-job (tasks state)
  ""
  (apply #'hackernews--mm-store-items
         (list :state state :indices (mapcar #'car tasks))
         (apply #'hackernews--reads
                #'hackernews--mm-insert
                (mapcar #'cdr tasks))))

(defun hackernews-mm-syncs-items (state)
  "Synchronously retrieve and store items in STATE."
  (hackernews--dispatch #'hackernews--mm-syncs-job state))

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
  (interactive "P")
  (hackernews--ensure-major-mode)
  (let ((feed (plist-get hackernews-state :feed)))
    (if feed
        (hackernews--load-stories feed n)
      (hackernews--error "Buffer unassociated with feed"))))

(defun hackernews-load-more-stories (&optional n)
  "Load N more stories into hackernews buffer.
N defaults to `hackernews-items-per-page'."
  (interactive "P")
  (hackernews--ensure-major-mode)
  (let ((feed   (plist-get hackernews-state :feed))
        (ids    (plist-get hackernews-state :ids))
        (offset (plist-get hackernews-state :offset)))
    (unless (and feed ids offset)
      (hackernews--error "Buffer in invalid state"))
    (if (>= offset (length ids))
        (message "%s" (substitute-command-keys "\
End of feed; type \\[hackernews-reload] to load new items."))
      (hackernews--load-stories feed n t))))

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
