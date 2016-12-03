;;; copy-as-format.el --- Copy buffer locations as GitHub/Slack/JIRA/HipChat/... formatted text

;; Author: Skye Shaw <skye.shaw@gmail.com>
;; Version: 0.0.1
;; Keywords: github, slack, jira, hipchat, gitlab, bitbucket, tools, convenience
;; URL: http://github.com/sshaw/copy-as-format

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
;; to the kill ring. The buffer will not be modified.
;;
;; With a prefix argument prompt for the format. Defaults to `copy-as-format-default'.

;;; Code:

(require 'xml)

(defvar copy-as-format-default "markdown"
  "Name of the default formatter, defaults to `markdown'")

(defvar copy-as-format-format-alist
  '(("bitbucket" copy-as-format--github)
    ("disqus"    copy-as-format--disqus)
    ("github"    copy-as-format--github)
    ("gitlab"    copy-as-format--gitlab)
    ("hipchat"   copy-as-format--hipchat)
    ("html"      copy-as-format--html)
    ("jira"      copy-as-format--jira)
    ("markdown"  copy-as-format--markdown)
    ("slack"     copy-as-format--slack))
  "Alist of format names and the function to do the formatting")

(defun copy-as-format ()
  "Copy the current line or active region and add it to the kill ring as
GitHub/Slack/JIRA/HipChat/... formatted code. Format defaults to `copy-as-format-default'.
The buffer will not be modified.

With a prefix argument prompt for the format.
"
  (interactive)
  (let ((format (if current-prefix-arg
		    (completing-read "Format: "
				     (mapcar 'car copy-as-format-format-alist)
				     nil
				     t
				     ""
				     nil
				     copy-as-format-default)
		  copy-as-format-default))

	(text (if (use-region-p)
		  (buffer-substring (region-beginning) (region-end))
		(buffer-substring (line-beginning-position) (line-end-position)))))

    (setq deactivate-mark t)
    (kill-new (funcall
	       (cadr (assoc format copy-as-format-format-alist))
	       ;;TODO: need to fix double \n when appending multiline closing delimiters?
	       text
	       (use-region-p)))))

(defun copy-as-format--disqus (text multiline)
  (format "<pre><code class='%s'>%s</code></pre>"
	  (copy-as-format--language)
	  (xml-escape-string text)))

(defun copy-as-format--github (text multiline)
  (if multiline
      (concat "```"
	      (copy-as-format--language)
	      "\n"
	      text
	      "\n```")
    (copy-as-format--inline-markdown text)))

(defun copy-as-format--hipchat (text multiline)
  ;; If I recall HipChat treats multiline and single line the same
  ;; TODO: does leading whitspace need to be trimmed?
  (concat "/code " text))

(defun copy-as-format--html (text multiline)
  (setq text (xml-escape-string text))
  (if multiline
      (concat "<pre><code>" text "</code></pre>")
    (concat "<code>" text "</code>")))

(defun copy-as-format--jira (text multiline)
  (if multiline
      ;; Do we want filenames? What extentions work?
      (concat "{code}\n" text "\n{code}")
    (concat "{{" text "}}")))

(defun copy-as-format--markdown (text multiline)
  (if multiline
      (with-temp-buffer
	(insert text)
	(indent-rigidly 1 (point-max) 4)
	(buffer-string))
    (copy-as-format--inline-markdown text)))

(defun copy-as-format--slack (text multiline)
  (if multiline
      (concat "```\n" text "\n```")
    (copy-as-format--inline-markdown
     ;; Slack preserves leading and trailing whitespace
     (replace-regexp-in-string "^[[:space:]]+\\|[[:space:]]+$" "" text))))

(defun copy-as-format--inline-markdown (text)
  (concat "`" text "`"))

(defun copy-as-format--language ()
  (if (buffer-file-name)
      (file-name-extension (buffer-file-name))
    ""))

(provide 'copy-as-format)
;;; copy-as-format.el ends here
