;;; file-complete.el --- core utilities for various file-completion styles and tables.  -*-lexical-binding:t-*-

(defconst file-complete-match-styles '(absolute root-relative basename)
  "Filename matching styles supported by `file-complete-completion-table'.

- absolute - match entire string against absolute file names,
  anchored at the string beginning.

- root-relative - match entire string against file name relative
  to a constant root.

- basename - match basename portion of string against basename
  portion of file names, and also directory name portions, not anchored.
  For example, \"foo/c\" will match \"/root/foo/bar/car.text\".")

(defun file-complete--iter-pcm-regex (string match-style root)
  "Return dir and file regexes constructed from STRING (a partial file name)."
  ;; `file-complete-completion-table' matches against directories from a
  ;; `path-iterator', and files within those directories. Thus we
  ;; construct two regexps from `string'.
  (let* ((dir-name (file-name-directory string)) ;; nil, or ends in /
	 (file-name (file-name-nondirectory string))

	 (file-pattern (completion-pcm--string->pattern file-name))
	 (file-regex (completion-pcm--pattern->regex file-pattern))

	 (dir-pattern (and dir-name (completion-pcm--string->pattern dir-name)))

	 (dir-regex
	  (cl-ecase match-style
	    (absolute
	     (completion-pcm--pattern->regex dir-pattern))

	    (root-relative
	     (cond
	      ((null dir-name)
	       (if (= 0 (length file-name))
		   (concat "\\`" root)
		 (concat "\\`" root
			 (when (eq (car file-pattern) 'star) ".*?")
			 "\\(" (substring
				(completion-pcm--pattern->regex
				 (append file-pattern (list 'star)))
				2) ;; strip \`
			 "\\)?\\'")))

	      (t
	       (concat root
		       (substring (completion-pcm--pattern->regex dir-pattern) 2) ;; strip \`
		       (if (= 0 (length file-name))
			   ""
			 (concat
			  "\\("
			  ;; The non-directory portion of STRING may
			  ;; be intended to match the next directory
			  ;; level.
			  (substring (completion-pcm--pattern->regex file-pattern) 2) ;; strip \`
			  "\\)?"))))
	      ))

	    (basename
	     (substring (completion-pcm--pattern->regex dir-pattern) 2)) ;; strip \`
	    )))
    (list dir-regex file-regex)))

(defun file-complete-completion-table (path-iter match-style root string pred action)
  "Implement a completion table for file names in PATH-ITER.

PATH-ITER is a `path-iterator' object. It will be restarted for
each call to `file-complete-completion-table'.

MATCH-STYLE is one of `file-complete-match-styles', which see.
ROOT is only non-nil for root-relative.

STRING, PRED, ACTION are completion table arguments:

STRING is a partial file name.  `*' is treated as a wildcard, as
in a shell glob pattern.

If PRED is nil, it is ignored. If non-nil, it must be a function
that takes one argument; the absolute file name.  The file name
is included in the result if PRED returns non-nil. In either
case, `completion-ignored-extensions', `completion-regexp-list',
`completion-ignore-case' are used as described in
`file-name-all-completions'.

ACTION is the current completion action; one of:

- nil; return common prefix of all completions of STRING, nil or
  t; see `try-completion'.

- t; return all completions; see `all-completions'

- lambda; return non-nil if string is a valid completion; see
  `test-completion'.

- '(boundaries . SUFFIX); return the completion region
  '(boundaries START . END) within STRING; see
  `completion-boundaries'.

- 'metadata; return (metadata . ALIST) as defined by
  `completion-metadata'."

  (cl-assert (memq match-style file-complete-match-styles))

  (cond
   ((eq (car-safe action) 'boundaries)
    ;; We don't use boundaries; return the default definition.
    (cons 'boundaries
	  (cons 0 (length (cdr action)))))

   ((eq action 'metadata)
    (cons 'metadata
	  (list
	   '(category . project-file)
	   )))

   ((memq action
	  '(nil    ;; Called from `try-completion'.
	    lambda ;; Called from `test-completion'.
	     t))   ;; Called from `all-completions'.

    ;; In file-name-all-completions, `completion-regexp-list', is
    ;; matched against file names and directories relative to `dir'.
    ;; Thus to handle partial completion delimiters in `string', we
    ;; construct two regexps from `string'; one from the directory
    ;; portion, and one from the non-directory portion.  We use the
    ;; directory regexp here, and pass the non-directory regexp to
    ;; `file-name-all-completions' via `completion-regexp-list'.  The
    ;; `string' input to `file-name-all-completions' is redundant with
    ;; the regexp, so we always build a regexp, and pass an empty
    ;; string.

    (pcase-let ((`(,dir-regex ,file-regex)
		 (file-complete--iter-pcm-regex string match-style root)))
      (let ((result nil))

	(path-iter-restart path-iter)

	(let ((case-fold-search completion-ignore-case)
	      dir)
	  (while (setq dir (path-iter-next path-iter))
	    (when (string-match dir-regex dir)
	      ;; A project that deals only with C files might set
	      ;; `completion-regexp-list' to match only *.c, *.h, so we
	      ;; preserve that here.
	      (let ((completion-regexp-list
		     (if (match-string 1 dir)
			 ;; Non-directory portion of STRING matches
			 ;; dir, so don't match it against files in
			 ;; dir.
			 completion-regexp-list
		       (cons file-regex completion-regexp-list))))
	    	(cl-mapc
		 (lambda (file-name)
		   (let ((absfile (concat (file-name-as-directory dir) file-name)))
		     (when (and (not (directory-name-p file-name))
				(or (null pred)
				    (funcall pred absfile)))
		       (push absfile result))))
		 (file-name-all-completions "" dir))
		))
	    ))
	(cond
	 ((null action)
	  ;; Called from `try-completion'; find common prefix of `result'.
	  (try-completion "" result))

         ((eq action 'lambda)
	  ;; Called from `test-completion'. Note that this call
	  ;; includes the `completion-to-table-input' advice, which in
	  ;; this case converts STRING to data format (= absolute file
	  ;; name).  But that fails for root-relative match-style,
	  ;; because the result list does not know about ROOT.  So we
	  ;; have to handle that here.
	  (cl-case match-style
	    ((absolute basename)
	     (test-completion string result))

	    (root-relative
	     (test-completion (concat root string) result))
	    ))

	 ((eq action t)
	  ;; Called from all-completions
	  result)
	 ))
      ))
   ))

(provide 'file-complete)
;; file-complete.el ends here.
