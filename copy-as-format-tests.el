;;; copy-as-format-tests.el --- Tests for copy-as-format -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; This file is NOT part of GNU Emacs.

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

;; Tests for copy-as-format package using ERT (Emacs Regression Testing).

;;; Code:

(require 'ert)
(require 'copy-as-format)

;;; Helper functions

(defun copy-as-format-tests--with-test-buffer (content &optional filename major-mode-sym region-start region-end)
  "Create a test buffer with CONTENT and optional FILENAME and MAJOR-MODE-SYM.
If REGION-START and REGION-END are provided, create a region selection."
  (with-temp-buffer
    (when filename
      (setq buffer-file-name filename))
    (when major-mode-sym
      (funcall major-mode-sym))
    (insert content)
    (when (and region-start region-end)
      (goto-char region-start)
      (set-mark region-end))
    (current-buffer)))

(defun copy-as-format-tests--test-format (format-name text multiline expected)
  "Test FORMAT-NAME with TEXT (MULTILINE flag) expecting EXPECTED result."
  (let ((func (cadr (assoc format-name copy-as-format-format-alist))))
    (should (fboundp func))
    (should (string= (funcall func text multiline) expected))))

;;; Single line tests

(ert-deftest copy-as-format-test-asciidoc-single-line ()
  "Test AsciiDoc format with single line."
  (copy-as-format-tests--test-format "asciidoc" "console.log('hello');" nil "`console.log('hello');`"))

(ert-deftest copy-as-format-test-bitbucket-single-line ()
  "Test Bitbucket format with single line."
  (copy-as-format-tests--test-format "bitbucket" "console.log('hello');" nil "`console.log('hello');`"))

(ert-deftest copy-as-format-test-disqus-single-line ()
  "Test Disqus format with single line."
  (copy-as-format-tests--test-format "disqus" "console.log('hello');" nil "<pre>console.log(&apos;hello&apos;);</pre>\n"))

(ert-deftest copy-as-format-test-github-single-line ()
  "Test GitHub format with single line."
  (copy-as-format-tests--test-format "github" "console.log('hello');" nil "`console.log('hello');`"))

(ert-deftest copy-as-format-test-gitlab-single-line ()
  "Test GitLab format with single line."
  (copy-as-format-tests--test-format "gitlab" "console.log('hello');" nil "`console.log('hello');`"))

(ert-deftest copy-as-format-test-hipchat-single-line ()
  "Test HipChat format with single line."
  (copy-as-format-tests--test-format "hipchat" "console.log('hello');" nil "/code console.log('hello');"))

(ert-deftest copy-as-format-test-html-single-line ()
  "Test HTML format with single line."
  (copy-as-format-tests--test-format "html" "console.log('hello');" nil "<code>console.log(&apos;hello&apos;);</code>"))

(ert-deftest copy-as-format-test-jira-single-line ()
  "Test JIRA format with single line."
  (copy-as-format-tests--test-format "jira" "console.log('hello');" nil "{{console.log('hello');}}"))

(ert-deftest copy-as-format-test-markdown-single-line ()
  "Test Markdown format with single line."
  (copy-as-format-tests--test-format "markdown" "console.log('hello');" nil "`console.log('hello');`"))

(ert-deftest copy-as-format-test-mediawiki-single-line ()
  "Test MediaWiki format with single line."
  (with-temp-buffer
    (setq buffer-file-name "test.js")
    (let ((result (copy-as-format--mediawiki "console.log('hello');" nil)))
      (should (string= result "<syntaxhighlight lang='js' inline>\nconsole.log('hello');\n</syntaxhighlight>")))))

(ert-deftest copy-as-format-test-org-mode-single-line ()
  "Test Org-mode format with single line."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((result (copy-as-format--org-mode "console.log('hello');" nil)))
      (should (string= result "#+BEGIN_SRC emacs-lisp\nconsole.log('hello');\n#+END_SRC\n")))))

(ert-deftest copy-as-format-test-pod-single-line ()
  "Test POD format with single line."
  (copy-as-format-tests--test-format "pod" "console.log('hello');" nil "C<< console.log('hello'); >>"))

(ert-deftest copy-as-format-test-rst-single-line ()
  "Test reStructuredText format with single line."
  (copy-as-format-tests--test-format "rst" "console.log('hello');" nil "``console.log('hello');``"))

(ert-deftest copy-as-format-test-slack-single-line ()
  "Test Slack format with single line."
  (copy-as-format-tests--test-format "slack" "console.log('hello');" nil "`console.log('hello');`"))

(ert-deftest copy-as-format-test-telegram-single-line ()
  "Test Telegram format with single line."
  (copy-as-format-tests--test-format "telegram" "console.log('hello');" nil "`console.log('hello');`"))

(ert-deftest copy-as-format-test-whatsapp-single-line ()
  "Test WhatsApp format with single line."
  (copy-as-format-tests--test-format "whatsapp" "console.log('hello');" nil "```console.log('hello');```"))

;;; Multi-line tests

(ert-deftest copy-as-format-test-asciidoc-multi-line ()
  "Test AsciiDoc format with multiple lines."
  (with-temp-buffer
    (setq buffer-file-name "test.js")
    (let ((text "function hello() {\n  console.log('world');\n}")
          (expected "[source,js]\n----\nfunction hello() {\n  console.log('world');\n}\n----\n"))
      (should (string= (copy-as-format--asciidoc text t) expected)))))

(ert-deftest copy-as-format-test-github-multi-line ()
  "Test GitHub format with multiple lines."
  (with-temp-buffer
    (setq buffer-file-name "test.js")
    (let ((text "function hello() {\n  console.log('world');\n}")
          (expected "```js\nfunction hello() {\n  console.log('world');\n}\n```\n"))
      (should (string= (copy-as-format--github text t) expected)))))

(ert-deftest copy-as-format-test-html-multi-line ()
  "Test HTML format with multiple lines."
  (let ((text "function hello() {\n  console.log('world');\n}")
        (expected "<pre><code>\nfunction hello() {\n  console.log(&apos;world&apos;);\n}\n</code></pre>\n"))
    (should (string= (copy-as-format--html text t) expected))))

(ert-deftest copy-as-format-test-jira-multi-line ()
  "Test JIRA format with multiple lines."
  (with-temp-buffer
    (setq buffer-file-name "test.js")
    (let ((text "function hello() {\n  console.log('world');\n}")
          (expected "{code:js}\nfunction hello() {\n  console.log('world');\n}\n{code}\n"))
      (should (string= (copy-as-format--jira text t) expected)))))

(ert-deftest copy-as-format-test-markdown-multi-line ()
  "Test Markdown format with multiple lines."
  (let ((text "function hello() {\n  console.log('world');\n}")
        (expected "    function hello() {\n      console.log('world');\n    }"))
    (should (string= (copy-as-format--markdown text t) expected))))

(ert-deftest copy-as-format-test-mediawiki-multi-line ()
  "Test MediaWiki format with multiple lines."
  (with-temp-buffer
    (setq buffer-file-name "test.js")
    (let ((text "function hello() {\n  console.log('world');\n}")
          (expected "<syntaxhighlight lang='js'>\nfunction hello() {\n  console.log('world');\n}\n</syntaxhighlight>"))
      (should (string= (copy-as-format--mediawiki text t) expected)))))

(ert-deftest copy-as-format-test-pod-multi-line ()
  "Test POD format with multiple lines."
  (let ((text "function hello() {\n  console.log('world');\n}")
        (expected "  function hello() {\n    console.log('world');\n  }"))
    (should (string= (copy-as-format--pod text t) expected))))

(ert-deftest copy-as-format-test-rst-multi-line ()
  "Test reStructuredText format with multiple lines."
  (let ((text "function hello() {\n  console.log('world');\n}")
        (expected ".. code::\n\n    function hello() {\n      console.log('world');\n    }\n"))
    (should (string= (copy-as-format--rst text t) expected))))

(ert-deftest copy-as-format-test-slack-multi-line ()
  "Test Slack format with multiple lines."
  (let ((text "function hello() {\n  console.log('world');\n}")
        (expected "```\nfunction hello() {\n  console.log('world');\n}\n```\n"))
    (should (string= (copy-as-format--slack text t) expected))))

;;; Edge case tests

(ert-deftest copy-as-format-test-empty-input ()
  "Test handling of empty input."
  (should-error (with-temp-buffer
                  (copy-as-format))
                :type 'error))

(ert-deftest copy-as-format-test-whitespace-trimming ()
  "Test whitespace trimming for single line formats."
  (copy-as-format-tests--test-format "slack" "  hello world  " nil "`hello world`"))

(ert-deftest copy-as-format-test-jira-whitespace-trimming ()
  "Test JIRA whitespace trimming for single line."
  (copy-as-format-tests--test-format "jira" "  hello world  " nil "{{hello world}}"))

(ert-deftest copy-as-format-test-rst-whitespace-trimming ()
  "Test RST whitespace trimming for single line."
  (copy-as-format-tests--test-format "rst" "  hello world  " nil "``hello world``"))

(ert-deftest copy-as-format-test-asciidoc-with-filename ()
  "Test AsciiDoc format with filename inclusion."
  (let ((copy-as-format-asciidoc-include-file-name t))
    (with-temp-buffer
      (setq buffer-file-name "test.py")
      (let ((text "print('hello')")
            (expected ".test.py\n[source,py]\n----\nprint('hello')\n----\n"))
        (should (string= (copy-as-format--asciidoc text t) expected))))))

(ert-deftest copy-as-format-test-asciidoc-without-filename ()
  "Test AsciiDoc format without filename inclusion."
  (let ((copy-as-format-asciidoc-include-file-name nil))
    (with-temp-buffer
      (setq buffer-file-name "test.py")
      (let ((text "print('hello')")
            (expected "[source,py]\n----\nprint('hello')\n----\n"))
        (should (string= (copy-as-format--asciidoc text t) expected))))))

(ert-deftest copy-as-format-test-multiline-leading-whitespace ()
  "Test multiline handling with leading whitespace."
  (with-temp-buffer
    (transient-mark-mode 1)
    (insert "    line1\n    line2\n    line3")
    (goto-char (point-min))
    (push-mark (point-max) nil t)
    (let ((text (copy-as-format--extract-text)))
      (should (string= text "line1\nline2\nline3")))))

(ert-deftest copy-as-format-test-single-line-extraction ()
  "Test single line text extraction."
  (with-temp-buffer
    (insert "hello world")
    (goto-char (point-min))
    (let ((text (copy-as-format--extract-text)))
      (should (string= text "hello world")))))

(ert-deftest copy-as-format-test-region-extraction ()
  "Test region text extraction."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    (set-mark (+ (point-min) 5))
    (let ((text (copy-as-format--extract-text)))
      (should (string= text "line1")))))

(ert-deftest copy-as-format-test-all-formats-multiline ()
  "Test that all formats handle multiline text without errors."
  (let ((text "line1\nline2"))
    (dolist (format-pair copy-as-format-format-alist)
      (let ((format-name (car format-pair))
            (func (cadr format-pair)))
        (should (stringp (funcall func text t)))))))

(ert-deftest copy-as-format-test-all-formats-single-line ()
  "Test that all formats handle single line text without errors."
  (let ((text "hello world"))
    (dolist (format-pair copy-as-format-format-alist)
      (let ((format-name (car format-pair))
            (func (cadr format-pair)))
        (should (stringp (funcall func text nil)))))))

(ert-deftest copy-as-format-test-jira-language-mapping ()
  "Test JIRA language mapping functionality."
  (with-temp-buffer
    (setq buffer-file-name "test.py")
    (let ((text "print('hello')")
          (expected "{code:python}\nprint('hello')\n{code}\n"))
      (should (string= (copy-as-format--jira text t) expected))))
  (with-temp-buffer
    (setq buffer-file-name "test.unknown")
    (let ((text "some code")
          (expected "{code:none}\nsome code\n{code}\n"))
      (should (string= (copy-as-format--jira text t) expected)))))

(ert-deftest copy-as-format-test-special-characters ()
  "Test handling of special characters in different formats."
  (let ((text "< > & \" '"))
    ;; HTML should escape these
    (should (string-match "&lt;" (copy-as-format--html text nil)))
    (should (string-match "&gt;" (copy-as-format--html text nil)))
    (should (string-match "&amp;" (copy-as-format--html text nil)))
    ;; Markdown should not escape these in backticks
    (should (string= (copy-as-format--markdown text nil) "`< > & \" '`"))))

(ert-deftest copy-as-format-test-org-mode-language-extraction ()
  "Test org-mode language extraction from major mode."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((text "(message \"hello\")")
          (expected "#+BEGIN_SRC emacs-lisp\n(message \"hello\")\n#+END_SRC\n"))
      (should (string= (copy-as-format--org-mode text t) expected))))
  (with-temp-buffer
    (python-mode)
    (let ((text "print('hello')")
          (expected "#+BEGIN_SRC python\nprint('hello')\n#+END_SRC\n"))
      (should (string= (copy-as-format--org-mode text t) expected)))))

(ert-deftest copy-as-format-test-disqus-no-language ()
  "Test Disqus format with no language."
  (with-temp-buffer
    (setq buffer-file-name nil)
    (let ((text "hello world")
          (expected "<pre>hello world</pre>\n"))
      (should (string= (copy-as-format--disqus text nil) expected)))))

(ert-deftest copy-as-format-test-disqus-with-language ()
  "Test Disqus format with language."
  (with-temp-buffer
    (setq buffer-file-name "test.js")
    (let ((text "console.log('hello')")
          (expected "<pre><code class='js'>\nconsole.log(&apos;hello&apos;)\n</code></pre>\n"))
      (should (string= (copy-as-format--disqus text nil) expected)))))

;;; Language detection tests

(ert-deftest copy-as-format-test-language-detection ()
  "Test language detection from file extension."
  (with-temp-buffer
    (setq buffer-file-name "test.py")
    (should (string= (copy-as-format--language) "py")))
  (with-temp-buffer
    (setq buffer-file-name "test.rb")
    (should (string= (copy-as-format--language) "rb")))
  (with-temp-buffer
    (setq buffer-file-name "noextension")
    (should (string= (copy-as-format--language) ""))))

;;; Interactive function tests

(ert-deftest copy-as-format-test-interactive-functions-exist ()
  "Test that all interactive format functions are defined."
  (dolist (format-pair copy-as-format-format-alist)
    (let ((format-name (car format-pair))
          (func-name (intern (concat "copy-as-format-" (car format-pair)))))
      (should (fboundp func-name)))))

(ert-deftest copy-as-format-test-kill-ring-integration ()
  "Test that copy-as-format adds text to kill ring."
  (with-temp-buffer
    (insert "test line")
    (goto-char (point-min))
    (let ((kill-ring nil))
      (copy-as-format-markdown)
      (should (string= (car kill-ring) "`test line`")))))

(ert-deftest copy-as-format-test-main-function-default ()
  "Test main copy-as-format function with default format."
  (with-temp-buffer
    (let ((copy-as-format-default "github"))
      (insert "test code")
      (goto-char (point-min))
      (let ((kill-ring nil))
        (copy-as-format)
        (should (string= (car kill-ring) "`test code`"))))))

(ert-deftest copy-as-format-test-format-function-sets-default ()
  "Test that format-specific functions set the default format."
  (let ((original-default copy-as-format-default))
    (unwind-protect
        (with-temp-buffer
          (insert "test")
          (goto-char (point-min))
          (let ((kill-ring nil))
            (copy-as-format-slack)
            (should (string= copy-as-format-default "slack"))
            (copy-as-format-jira)
            (should (string= copy-as-format-default "jira"))))
      (setq copy-as-format-default original-default))))

(ert-deftest copy-as-format-test-format-validation ()
  "Test that invalid format functions are handled."
  (let ((copy-as-format-format-alist '(("invalid" invalid-function))))
    (with-temp-buffer
      (insert "test")
      (should-error (copy-as-format) :type 'error))))

(ert-deftest copy-as-format-test-comprehensive-integration ()
  "Test full integration with multiple formats and different scenarios."
  (dolist (format-pair copy-as-format-format-alist)
    (let ((format-name (car format-pair))
          (copy-as-format-default (car format-pair)))
      (with-temp-buffer
        (insert "function test() {\n  return 42;\n}")
        (goto-char (point-min))
        (let ((kill-ring nil))
          ;; Test single line
          (copy-as-format)
          (should kill-ring)
          (should (stringp (car kill-ring)))
          ;; Test multiline
          (setq kill-ring nil)
          (transient-mark-mode 1)
          (push-mark (point-max) nil t)
          (copy-as-format)
          (should kill-ring)
          (should (stringp (car kill-ring))))))))

(provide 'copy-as-format-tests)
;;; copy-as-format-tests.el ends here