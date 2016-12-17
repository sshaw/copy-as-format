;;; copy-as-format.el --- Copy buffer locations as GitHub/Slack/JIRA/HipChat/... formatted text

;; Author: Skye Shaw <skye.shaw@gmail.com>
;; Package-Version: 0.0.1
;; Keywords: github, slack, jira, hipchat, gitlab, bitbucket, tools, convenience
;; URL: https://github.com/sshaw/copy-as-format
;; Package-Requires: ((cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
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

;; Copy buffer locations as GitHub/Slack/JIRA/HipChat/... formatted code and add them
;; to the kill ring.  The buffer will not be modified.
;;
;; With a prefix argument prompt for the format.  Defaults to `copy-as-format-default'.

;;; Code:

(require 'cl-lib)
(require 'xml)

(defvar copy-as-format-default "markdown"
  "Name of the default formatter, defaults to `markdown'.")

(defvar copy-as-format-format-alist
  '(("bitbucket" copy-as-format--github)
    ("disqus"    copy-as-format--disqus)
    ("github"    copy-as-format--github)
    ("gitlab"    copy-as-format--github)
    ("hipchat"   copy-as-format--hipchat)
    ("html"      copy-as-format--html)
    ("jira"      copy-as-format--jira)
    ("markdown"  copy-as-format--markdown)
    ("slack"     copy-as-format--slack))
  "Alist of format names and the function to do the formatting.")

(defconst copy-as-format--jira-supported-languages
  '(("as"  "actionscript")
    ("htm" "html")
    ("js"  "javascript")))

(dolist (lang '("html" "java" "sql" "xhtml" "xml"))
  (add-to-list 'copy-as-format--jira-supported-languages (list lang lang)))

(defun copy-as-format--extract-text ()
  (if (not (use-region-p))
      (buffer-substring-no-properties (line-beginning-position) (line-end-position))
    ;; Avoid adding an extra blank line to the selection. This happens when point or mark
    ;; is at the start of the next line.
    ;;
    ;; When selection is from bottom to top, exchange point and mark
    ;; so that the `point' and `(region-end)' are the same.
   (when (< (point) (mark))
     (exchange-point-and-mark))
    (let ((end (region-end)))
      (when (= end (line-beginning-position))
        (setq end (1- end)))
      (buffer-substring-no-properties (region-beginning) end))))

(defun copy-as-format--disqus (text multiline)
  (format "<pre><code class='%s'>\n%s\n</code></pre>\n"
          (copy-as-format--language)
          (xml-escape-string text)))

(defun copy-as-format--github (text multiline)
  (if multiline
      (format "```%s\n%s\n```\n" (copy-as-format--language) text)
    (copy-as-format--inline-markdown text)))

(defun copy-as-format--hipchat (text multiline)
  ;; If I recall HipChat treats multiline and single line the same
  ;; TODO: does leading whitspace need to be trimmed?
  (format "/code %s" text))

(defun copy-as-format--html (text multiline)
  (setq text (xml-escape-string text))
  (if multiline
      (format "<pre><code>\n%s\n</code></pre>\n" text)
    (format "<code>%s</code>" text)))

(defun copy-as-format--jira (text multiline)
  (if multiline
      (let ((lang (car (assoc (copy-as-format--language)
			      copy-as-format--jira-supported-languages))))
	(format "{code%s}\n%s\n{code}\n"
		(if (null lang) "" (concat ":" lang))
		text))
    (format "{{%s}}" (copy-as-format--trim text))))

(defun copy-as-format--markdown (text multiline)
  (if multiline
      (with-temp-buffer
        (insert text)
        (indent-rigidly 1 (point-max) 4)
        (buffer-string))
    (copy-as-format--inline-markdown text)))

(defun copy-as-format--slack (text multiline)
  (if multiline
      (format "```\n%s\n```\n" text)
    (copy-as-format--inline-markdown
     ;; Slack preserves leading and trailing whitespace
     (copy-as-format--trim text))))

(defun copy-as-format--inline-markdown (text)
  (format "`%s`" text))

(defun copy-as-format--language ()
  (if (buffer-file-name)
      ;; There may be no extension so downcase filename to avoid nil check
      (file-name-extension (downcase (buffer-file-name)))
    ""))

(defun copy-as-format--trim (s)
  (replace-regexp-in-string "^[[:space:]]+\\|[[:space:]]+$" "" s))


;;;###autoload
(defun copy-as-format ()
  "Copy the current line or active region and add it to the kill ring as
GitHub/Slack/JIRA/HipChat/... formatted code. Format defaults to
`copy-as-format-default'. The buffer will not be modified.

With a prefix argument prompt for the format."
  (interactive)
  (let* ((text (copy-as-format--extract-text))
         (format (if current-prefix-arg
                     (completing-read "Format: "
                                      (mapcar 'car copy-as-format-format-alist)
                                      nil
                                      t
                                      ""
                                      nil
                                      copy-as-format-default)
                   copy-as-format-default))
         (func (cadr (assoc format copy-as-format-format-alist))))

    (when (string= text "")
      (error "No text selected"))

    (when (not (fboundp func))
      (error "Missing or invalid format function for `%s'" format))

    (kill-new (funcall
               func
               text
               (use-region-p)))

    (setq deactivate-mark t)))

;; Generate format specific functions
(cl-loop for (name) in copy-as-format-format-alist
         do (fset (intern (concat "copy-as-format-" name))
                  `(lambda ()
                     (interactive)
                     (setq copy-as-format-default ,name)
                     (copy-as-format))))

;;;###autoload (autoload 'copy-as-format-bitbucket "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-disqus    "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-github    "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-gitlab    "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-hipchat   "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-html      "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-jira      "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-markdown  "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-slack     "copy-as-format" nil t)

(provide 'copy-as-format)
;;; copy-as-format.el ends here
