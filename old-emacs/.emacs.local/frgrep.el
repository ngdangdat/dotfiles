;; Define the mode
(define-derived-mode frgrep-mode special-mode "Fuzzy Grep"
  "Fuzzy search results using ripgrep."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (hl-line-mode 1))

;; Styling
(defvar frgrep-buffer-name "*Fuzzy Search*"
  "Name of the buffer displaying match result.")

(defface frgrep-file-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for displaying file names.")

(defface fgrep-line-face
  '((t :inherit font-lock-constant-face))
  "Face for displaying line number.")

(defface frgrep-match-face
  '((t :inherit match :weight bold))
  "Face for highlighting matched text.")

;; Functions
(defun search-string-in-dir (directory pattern)
  "Search string in directory recursively."
  (let ((default-directory directory))
    (with-temp-buffer
      (call-process-shell-command
       (format "rg --line-number --no-heading %s"
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

(defun format-match (match)
  "Format match result in the list"
  (let ((file (car match))
        (line (nth 1 match))
        (text (nth 2 match)))
    (format "%s:%s: %s"
            (propertize file 'face 'fgrep-file-face)
            (propertize (number-to-string line) 'face 'frgrep-line-face)
            text)))

;; Fuzzy search text in files
(defun fuzzy-search-text()
  "Search text in root folder recursively"
  (interactive)
  (let* ((project-root (find-project-root))
        (search-text (read-string "Look for text: "))
        (matches (search-string-in-dir project-root search-text)))
    (if (null matches)
        (message "No matches found for '%s' in %s" search-text project-root)
      (frgrep-display-results search-text matches project-root)

(defun frgrep-display-results (search-text matches project-root)
  "Display results in frgrep buffer."
  (let ((buffer (get-buffer-create frgrep-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (frgrep-mode)                   ; to be defined later

        ;; header
        (insert (propertize (format "Search results for: %s (%d matches)\n\n"
                                    search-text (length matches))
                            'face 'bold))
        ;; matches
        (let ((index 0))
          (dolist (match matches)
            (let ((file (car match))
                  (line (nth 1 match))
                  (text (nth 2 match)))
              (insert (propertize (format "%3d " index ) 'face 'frgrep-line-face))
              (insert (propertize file 'face 'frgrep-file-face))
              (insert ":")
              (insert (propertize (number-to-string line) 'face 'frgrep-line-face))
              (insert ":")
              (insert text)
              (insert "\n")
              (setq index (1+ index)))))
      (goto-char (point-min))
      (forward-line 2)                  ; move to 1st result
      (setq-local frgrep-matches matches)
      (setq-local frgrep-project-root project-root)))
    (switch-to-buffer buffer))))))

;; Provide the module
(provide 'frgrep)
