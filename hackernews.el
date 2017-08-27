;;; hackernews.el --- Hacker News Client for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2012  Lincoln de Sousa <lincoln@comum.org>

;; Author: Lincoln de Sousa <lincoln@comum.org>
;; Keywords: hackernews
;; Version: 0.3.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Read Hacker News from Emacs.
;;
;; Enjoy!

;;; Code:

(require 'json)
(require 'url)
(eval-when-compile (require 'cl))

(defgroup hackernews nil
  "Simple Hacker News Emacs client."
  :group 'external
  :prefix "hackernews-")

(defface hackernews-link-face
  '((t :foreground "green"))
  "Face used for links to articles."
  :group 'hackernews)

(defface hackernews-comment-count-face
  '((t :inherit hackernews-link-face))
  "Face used for comment counts."
  :group 'hackernews)

(defface hackernews-score-face
  '((t :inherit default))
  "Face used for the score of an article."
  :group 'hackernews)

(defvar hackernews-top-story-list ()
  "The list of stories to display.")

(defvar hackernews-top-story-limit 20
  "Retrieve details for at most this many stories.
This should not exceed 100.")

(defvar hackernews-top-stories-url
  "https://hacker-news.firebaseio.com/v0/topstories.json"
  "The URL from which to grab top story IDs.")

(defvar hackernews-item-url "https://hacker-news.firebaseio.com/v0/item/%s.json"
  "The URL format from which to grab an item's details.")

(defvar hackernews-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g"             #'hackernews)
    (define-key map "q"             #'quit-window)
    (define-key map "m"             #'hackernews-load-more-stories)
    (define-key map "n"             #'hackernews-next-item)
    (define-key map "p"             #'hackernews-previous-item)
    (define-key map "\t"            #'hackernews-next-comment)
    (define-key map [backtab]       #'hackernews-previous-comment)
    (define-key map [S-iso-lefttab] #'hackernews-previous-comment)
    (define-key map [S-tab]         #'hackernews-previous-comment)
    map)
  "The keymap to use with hackernews.")

(defun hackernews-internal-browser (url)
  "Open URL within Emacs.
Try `eww' if available, otherwise `browse-url-text-browser'."
  (if (fboundp 'eww-browse-url)
      (eww-browse-url url)
    (browse-url-text-emacs url)))

;;; Interactive functions

(defun hackernews-first-item ()
  "Move point to first article link in hackernews buffer."
  (interactive)
  (goto-char (point-min))
  (hackernews-next-item))

(defun hackernews-next-item ()
  "Skip to next article link in hackernews buffer."
  (interactive)
  (re-search-forward "^\\[[[:digit:]]+][[:space:]]*" nil t))

(defun hackernews-previous-item ()
  "Skip to previous article link in hackernews buffer."
  (interactive)
  (forward-line -1)
  (hackernews-next-item))

(defun hackernews-next-comment ()
  "Skip to next article comments link in hackernews buffer."
  (interactive)
  (when (re-search-forward " ([[:digit:]]+ comments)$" nil t)
    (goto-char (1+ (match-beginning 0)))))

(defun hackernews-previous-comment ()
  "Skip to previous article comments link in hackernews buffer."
  (interactive)
  (forward-line -1)
  (hackernews-next-comment))

;;;###autoload
(defun hackernews ()
  "Read Hacker News."
  (interactive)
  (setq hackernews-top-story-list ())
  (condition-case err
      (hackernews-format-results
       (mapcar #'hackernews-get-item
               (hackernews-top-stories hackernews-top-story-limit)))
    (error (message "hackernews error: %s" (error-message-string err))))
  (hackernews-first-item))

(defun hackernews-load-more-stories ()
  "Load more stories into the hackernews buffer."
  (interactive)
  (let ((stories (hackernews-top-stories
                  hackernews-top-story-limit
                  (count-lines (point-min) (point-max)))))
    (hackernews-format-results
     (mapcar #'hackernews-get-item stories)
     t)
    (forward-line (- (length stories)))
    (hackernews-next-item)))

;;; UI Functions

(defun hackernews-comment-url (id)
  (format "https://news.ycombinator.com/item?id=%s" id))

(defun hackernews-link-of-url (url)
  (replace-regexp-in-string "\\`/comments/\\(.*\\)\\'"
                            (lambda (match)
                              (hackernews-comment-url (match-string 1 match)))
                            url))

(defun hackernews-create-link-in-buffer (title url face)
  "Insert clickable string into current buffer."
  (lexical-let ((url url)
                (map (make-sparse-keymap)))
    (define-key map "\r"
      (lambda () (interactive) (browse-url url)))
    (define-key map "t"
      (lambda () (interactive) (hackernews-internal-browser url)))
    (define-key map [down-mouse-1]
      (lambda () (interactive) (browse-url url)))
    (insert
     (propertize
      title
      'face face
      'keymap map
      'mouse-face 'highlight))))

(defun hackernews-encoding (string)
  "Encode STRING for hackernews."
  (decode-coding-string
   (encode-coding-string string 'utf-8) 'utf-8))

(defun hackernews-render-post (post)
  "Render single hackernews POST in current buffer.
Add POST title as a link and print its points and number of
comments."
  (let ((id    (cdr (assq 'id    post)))
        (title (cdr (assq 'title post)))
        (url   (cdr (assq 'url   post)))
        (score (cdr (assq 'score post)))
        (kids  (cdr (assq 'kids  post))))
    (insert (format "%-6s" (propertize (format "[%s]" score)
                                       'face 'hackernews-score-face)))
    (hackernews-create-link-in-buffer
     (hackernews-encoding title)
     (if url
         (hackernews-link-of-url (hackernews-encoding url))
       (hackernews-comment-url id))
     'hackernews-link-face)
    (insert ?\s)
    (hackernews-create-link-in-buffer
     (format "(%d comments)" (length kids))
     (hackernews-comment-url id)
     'hackernews-comment-count-face)
    (insert ?\n)))

(defun hackernews-format-results (results &optional append)
  "Create hackernews buffer to render RESULTS in.
When APPEND is non-nil, the RESULTS are appended to the existing
buffer if available."
  (let* ((buf-name "*hackernews*")
         (buf (get-buffer buf-name)))
    (if (and append buf)
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (mapc #'hackernews-render-post results)))
      (with-output-to-temp-buffer buf-name
        (switch-to-buffer buf-name)
        (setq font-lock-mode nil)
        (use-local-map hackernews-map)
        (mapc #'hackernews-render-post results)))))

;;; Retrieving and parsing

(defun hackernews-top-stories (&optional limit offset)
  "Get a list of stories.
When specified, ignore all list entries after LIMIT and before
OFFSET."
  (unless hackernews-top-story-list
    (setq hackernews-top-story-list
          (append (hackernews-retrieve-and-parse hackernews-top-stories-url) ())))
  (let ((reverse-offset (- (length hackernews-top-story-list) (or offset 0))))
    (when (<= reverse-offset 0)
      (error "No more stories available"))
    (reverse (last (reverse (last hackernews-top-story-list reverse-offset)) limit))))

(defun hackernews-get-item (id)
  "Build URL for item based on its ID then retreave & parse it."
  (hackernews-retrieve-and-parse (format hackernews-item-url id)))

(defun hackernews-retrieve-and-parse (url)
  "Retrieve contents from URL and parse it."
  (hackernews-parse (hackernews-retrieve url)))

(defun hackernews-retrieve (url)
  "Download URL and remove HTTP envelope."
  (let (json)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (unless (string-match-p "200 OK" (buffer-string))
        (error "Problem connecting to the server"))
      (re-search-forward "^$" nil 'move)
      (setq json (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer))
    json))

(defun hackernews-parse (contents)
  "Parse CONTENTS as JSON."
  (json-read-from-string contents))

(provide 'hackernews)

;;; hackernews.el ends here
