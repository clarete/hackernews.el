;;; hackernews.el --- Hacker News Client for Emacs

;; Copyright (C) 2012  Lincoln de Sousa <lincoln@comum.org>

;; Author: Lincoln de Sousa <lincoln@comum.org>
;; Keywords: hackernews
;; Version: 0.1

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
;; This is a very simple extention to search for files and for contents
;; inside files.
;; 
;; Enjoy!

;;; Code:

(require 'json)
(require 'url)

;; "http://apihackernews.herokuapp.com/"

(defvar hackernews-url "http://api.ihackernews.com/page"
  "The url to grab the list of news")

;;; Interactive functions

;;;###autoload
(defun hackernews ()
  "The entry point of our client"
  (interactive)
  (condition-case ex
      (hackernews-format-results
       (hackernews-parse
        (hackernews-retrieve hackernews-url)))
    ('error
     (message (format "Bad news, bro: %s" (car (cdr ex)))))))

;;; UI Functions

(defun hackernews-create-link-in-buffer (title url)
  "Insert clickable string inside a buffer"
  (lexical-let ((title title)
                (url url)
                (map (make-sparse-keymap)))
    (define-key map (kbd "<RET>")
      #'(lambda (e) (interactive "p") (browse-url url)))
    (define-key map (kbd "<down-mouse-1>")
      #'(lambda (e) (interactive "p") (browse-url url)))
    (insert
     (propertize
      title
      'face '(:foreground "green")
      'keymap map
      'mouse-face 'highlight))))

(defun hackernews-render-post (post)
  "Render a single post to the current buffer

Add the post title as a link, and print the points and number of
comments."
  (princ (format "[%s]\t" (cdr (assoc 'points post))))
  (hackernews-create-link-in-buffer
   (cdr (assoc 'title post))
   (cdr (assoc 'url post)))
  (insert
   (propertize
    (format " (%d comments)" (cdr (assoc 'commentCount post)))
    'face '(:foreground "gray")))
  (princ "\n"))

(defun hackernews-format-results (results)
  "Create the buffer to render all the info"
  (with-output-to-temp-buffer "*hackernews*"
    (switch-to-buffer-other-window "*hackernews*")
    (setq font-lock-mode nil)
    (princ "Your hacker News Emacs client\n\n")
    (mapcar #'hackernews-render-post
             (cdr (assoc 'items results)))))

;;; Retrieving and parsing

(defun hackernews-retrieve (url)
  (let ((buffer (url-retrieve-synchronously url))
        (json nil))
    (save-excursion
      (set-buffer buffer)
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
