;; smudge-mode, a major mode for Smudge source

(setq smudge-highlights
      '(("//.*$" . font-lock-comment-face)
        ("^#[-_0-9A-Za-z]+\\(\\(=\\|\\( \\|\t\\)+\\).*\\)?$" . font-lock-preprocessor-face)
        ("@[a-zA-Z][a-zA-Z0-9]*" . font-lock-function-name-face)))

(defun smudge-indent-line ()
  "Indent current line as Smudge code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)

    (let ((not-indented t) (cur-indent 0))
      (if (and (looking-at "^[ \t]*$") (not (= (current-indentation) 0)))
          (setq cur-indent 0)
        (progn
          (if (or 
               (looking-at "^[\t ]*}")
               (looking-at "^[\t ]*\]"))
              (progn
                (save-excursion
                  (forward-line -1)
                  (setq cur-indent (- (current-indentation) default-tab-width))))
            (save-excursion
              (while not-indented
                (forward-line -1)
                (if (or (looking-at "^[ \t]*]")
                        (looking-at "^[ \t]*}"))
                    (progn
                      (setq cur-indent (current-indentation))
                      (setq not-indented nil))
                  (if (or (looking-at "^[ \t]*{[ \t]*$")
                          (looking-at "^[ \t]*[[]"))
                      (progn
                        (setq cur-indent (+ (current-indentation) default-tab-width))
                        (setq not-indented nil))
                    (if (bobp)
                        (setq not-indented nil)))))))))
      (if cur-indent
          (progn
                                        ;(if (looking-at "^[\t ]*\*") (setq cur-indent (- cur-indent 1)))
            (if (< cur-indent 0)
                (setq cur-indent 0))
            (indent-line-to cur-indent))
        (indent-line-to 0)))))

(define-derived-mode smudge-mode fundamental-mode
  (setq font-lock-defaults '(smudge-highlights))
  (setq mode-name "Smudge")
  (setq indent-line-function 'smudge-indent-line))

(provide 'smudge-mode)
