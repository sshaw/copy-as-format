# copy-as-format

Emacs function to copy buffer locations as GitHub/Slack/JIRA/HipChat/...
formatted code

## Supported Formats/Services

* BitBucket
* Disqus
* GitHub
* GitLab
* HipChat
* HTML
* JIRA
* Markdown
* Slack

## Usage

`M-x copy-as-format` or `C-u M-x copy-as-format`

Copy the current line or active region and add it to the kill ring as
GitHub/Slack/JIRA/HipChat/... formatted code. Format defaults to `copy-as-format-default`.
The buffer will not be modified.

With a prefix argument prompt for the format.

## Adding Formats

Create your format function:

```el
(defun some-great-format (text multiline)
  (if multiline
      (multiline-format text)
    (single-line-format text)))
```

Then add an entry to `copy-as-format-format-alist`:

```el
(add-to-list 'copy-as-format-format-alist '("great-format" some-great-format))
```

## See Also

* [git-link](https://github.com/sshaw/git-link)
