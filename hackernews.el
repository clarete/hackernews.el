;;; hackernews.el --- Hacker News Client for Emacs

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
;;
;; Read HackerNews from Emacs
;;
;; Enjoy!

;;; Code:

(require 'json)
(require 'url)
(require 'eww nil :noerror)
(eval-when-compile (require 'cl))

(defgroup hackernews nil
  "Simple hackernews emacs client"
  :group 'external
  :prefix "hackernews-")

(defface hackernews-link-face
  '((t (:foreground "green")))
  "Face used for links to articles"
  :group 'hackernews)

(defface hackernews-comment-count-face
  '((t (:foreground "green")))
  "Face used for comment counts"
  :group 'hackernews)

(defface hackernews-score-face
  '((t (:foreground nil)))
  "Face used for the score of an article"
  :group 'hackernews)

(defvar hackernews-top-story-list nil
  "The list of stories to display")

(defvar hackernews-top-story-limit 20
  "Retrieve details for at most this many stories. This should not exceed 100.")

(defvar hackernews-top-stories-url "https://hacker-news.firebaseio.com/v0/topstories.json"
  "The url to grab the top story ids")

(defvar hackernews-item-url "https://hacker-news.firebaseio.com/v0/item/%s.json"
  "The url to grab an item's details")

(defvar hackernews-map (make-sparse-keymap)
  "The keymap to use with hackernews")

(defun hackernews-internal-browser (url)
  (if (featurep 'eww)
      (eww-browse-url url)
    (browse-url-text-emacs url)))

(defun hackernews-first-item ()
  (interactive)
  (goto-char (point-min))
  (hackernews-next-item))

(defun hackernews-next-item ()
  (interactive)
  (re-search-forward "^\[\[0-9]+\]\s*" nil t 1))

(defun hackernews-previous-item ()
  (interactive)
  (forward-line -1)
  (beginning-of-line)
  (hackernews-next-item))

(defun hackernews-next-comment ()
  (interactive)
  (re-search-forward " \([0-9]+ comments\)$" nil t 1)
  (search-backward "("))

(defun hackernews-previous-comment ()
  (interactive)
  (forward-line -1)
  (hackernews-next-comment))

(if hackernews-map
    (progn
      (define-key hackernews-map (kbd "g") 'hackernews)
      (define-key hackernews-map (kbd "q") 'bury-buffer)
      (define-key hackernews-map (kbd "m") 'hackernews-load-more-stories)
      (define-key hackernews-map (kbd "n") 'hackernews-next-item)
      (define-key hackernews-map (kbd "p") 'hackernews-previous-item)
      (define-key hackernews-map (kbd "<tab>") 'hackernews-next-comment)
      (define-key hackernews-map (kbd "<backtab>") 'hackernews-previous-comment)))

;;; Interactive functions

;;;###autoload
(defun hackernews ()
  "The entry point of our client"
  (interactive)
  (setq hackernews-top-story-list nil)
  (condition-case ex
      (hackernews-format-results
       (mapcar 'hackernews-get-item
               (hackernews-top-stories hackernews-top-story-limit)))
    ('error
     (message (format "hackernewsclient error: %s" (car (cdr ex))))))
  (hackernews-first-item))

(defun hackernews-load-more-stories ()
  "Load more stories into the buffer"
  (interactive)
  (let ((stories (hackernews-top-stories
                  hackernews-top-story-limit
                  (count-lines (point-min) (point-max)))))
    (hackernews-format-results
     (mapcar 'hackernews-get-item stories)
     t)
    (forward-line (* (length stories) -1))
    (beginning-of-line)
    (hackernews-next-item)))

;;; UI Functions

(defun hackernews-comment-url (id)
  (format "https://news.ycombinator.com/item?id=%s" id))

(defun hackernews-link-of-url (url)
  (lexical-let ((url url)
		(hackernews-item "/comments/"))
    (if (string-prefix-p hackernews-item url)
	(hackernews-comment-url (substring url (length hackernews-item)))
      url)))

(defun hackernews-create-link-in-buffer (title url face)
  "Insert clickable string inside a buffer"
  (lexical-let ((title title)
                (url url)
                (face face)
                (map (make-sparse-keymap)))
    (define-key map (kbd "<RET>")
      #'(lambda (e) (interactive "p") (browse-url url)))
    (define-key map (kbd "t")
      #'(lambda (e) (interactive "p") (hackernews-internal-browser url)))
    (define-key map (kbd "<down-mouse-1>")
      #'(lambda (e) (interactive "p") (browse-url url)))
    (insert
     (propertize
      title
      'face face
      'keymap map
      'mouse-face 'highlight))))

(defun hackernews-space-fill (string n)
  "Makes sure that string is at least n characters long, and
   if it isn't, it adds SPACE-characters to the end"
  (while (< (length string) n)
    (setf string (concat string " ")))
  (identity string))

(defun hackernews-encoding (string)
  "encoding"
  (decode-coding-string
   (encode-coding-string string 'utf-8) 'utf-8))

(defun hackernews-render-post (post)
  "Render a single post to the current buffer
  Add the post title as a link, and print the points and number of
  comments."
  (let ((id (cdr (assoc 'id post)))
        (title (cdr (assoc 'title post)))
        (url (cdr (assoc 'url post)))
        (score (cdr (assoc 'score post)))
        (kids (cdr (assoc 'kids post))))
    (insert (hackernews-space-fill
             (propertize
              (format "[%s]" score)
              'face 'hackernews-score-face)
             6))
    (hackernews-create-link-in-buffer
     (hackernews-encoding title)
     (if url
         (hackernews-link-of-url (hackernews-encoding url))
       (hackernews-comment-url id))
     'hackernews-link-face)
    (hackernews-create-link-in-buffer
     (format " (%d comments)" (length kids))
     (hackernews-comment-url id)
     'hackernews-comment-count-face)
    (insert "\n")))

(defun hackernews-format-results (results &optional append)
  "Create the buffer to render all the info.
When APPEND is non-nil, the results are appended to the
existing buffer if available."
  (let* ((buf-name "*hackernews*")
         (buf (get-buffer buf-name)))
    (if (and append buf)
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (mapcar 'hackernews-render-post results)))
      (with-output-to-temp-buffer buf-name
        (switch-to-buffer buf-name)
        (setq font-lock-mode nil)
        (use-local-map hackernews-map)
        (mapcar 'hackernews-render-post results)))))

;;; Retrieving and parsing

(defun hackernews-top-stories (&optional limit offset)
  "Get a list of stories.
When LIMIT is given, ignore all list entries past the limit.
When OFFSET is given, ignore all list entries before the offset."
  (if (null hackernews-top-story-list)
      (setq hackernews-top-story-list
            (append (hackernews-retrieve-and-parse hackernews-top-stories-url) nil)))
  (let ((reverse-offset (- (length hackernews-top-story-list) (or offset 0))))
    (if (<= reverse-offset 0)
        (error "No more stories available"))
    (reverse (last (reverse (last hackernews-top-story-list reverse-offset)) limit))))

(defun hackernews-get-item (id)
  (hackernews-retrieve-and-parse (format hackernews-item-url id)))

(defun hackernews-retrieve-and-parse (url)
  (hackernews-parse (hackernews-retrieve url)))

(defun hackernews-retrieve (url)
  (let (json)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (when (not (string-match "200 OK" (buffer-string)))
        (error "Problem connecting to the server"))
      (re-search-forward "^$" nil 'move)
      (setq json (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer (current-buffer)))
    json))

(defun hackernews-parse (contents)
  (json-read-from-string contents))

(provide 'hackernews)

;;; hackernews.el ends here
