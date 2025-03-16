(defun search-string-in-dir (directory pattern)
  "Search string in directory recursively."
  (let ((default-directory directory))
    (with-temp-buffer
      (call-process-shell-command
       (format "rg --line-number --no-heading --color=always %s"
               (shell-quote-argument pattern))
       nil t)
      (let ((results '()))
        (goto-char (point-min))
        (while (not (eobp))
          (when (looking-at "\\(.+\\):\\([0-9]+\\):\\(.*\\)$")
            (let ((file (match-string 1))
                  (line (string-to-number (match-string 2)))
                  (text (match-string 3)))
              (push (list file line text) results)))
          (forward-line 1))
        (nreverse results)))))

(defun list-current-files (dir)
  "Get list of files in DIR using ripgrep."
  (let ((default-directory dir))
    (split-string
     (shell-command-to-string
      "rg --files --follow --hidden -g !.git")
     "\n" t)))

(defun find-project-root ()
  "Find the root of the current directory"
  (let ((current-dir (expand-file-name default-directory)))
    (cl-loop while (and current-dir (not (string= current-dir "/")))
             if (file-exists-p (expand-file-name ".git" current-dir))
             return current-dir
             else do (setq current-dir (file-name-directory
                                        (directory-file-name current-dir))))
    (or current-dir default-directory)))

;; Fuzzy search files
(defun fuzzy-find-file()
  "Find files using ripgrep."
  (interactive)
  (let* ((project-root (find-project-root))
         (files (list-current-files project-root))
         (file (completing-read "Find file: " files nil t))
         (full-path (expand-file-name file project-root)))
    (when file
      (find-file full-path))))

;; Fuzzy search text in files
(defun fuzzy-search-text()
  "Search text in root folder recursively"
  (interactive)
  (let* ((project-root (find-project-root))
        (search-text (read-string "Look for text: "))
        (matches (search-string-in-dir project-root search-text)))
    (if (null matches)
        (message "No matches found for '%s' in %s" search-text project-root)
      (let* ((selection (completing-read
                         (format "Matches count: " (length matches))
                         (mapcar (lambda (item)
                                   (format "%s:%d: %s"
                                           (car item)
                                           (nth 1 item)
                                           (nth 2 item)))
                                 matches)
                         nil t))
             (match (when (string-match "\\(.+\\):\\([0-9]+\\): " selection)
                      (cons (match-string 1 selection)
                            (string-to-number (match-string 2 seletion))))))
        (when match
          (find-file (expand-file-name (car match) project-root))
          (goto-char (point-min))
          (forward-line (1- (cdr match))))))))

;; Provide the module
(provide 'frgrep)
