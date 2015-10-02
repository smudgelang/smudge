;; smudge-mode, a major mode for Smudge source

(setq smudge-highlights
      '(("//.*$" . font-lock-comment-face)
        ("@[a-zA-Z][a-zA-Z0-9]*" . font-lock-function-name-face)))

(defun smudge-indent-line ()
  "Indent current line as Smudge code."
  (interactive)
  (beginning-of-line)
  (if (bobp) ; Rule 1
      (indent-line-to 0)

    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*$")
          (setq cur-indent 0)
        (if (or 
             (looking-at "^[\t ]*}") ; Rule 5
             (looking-at "^[\t ]*\]")) ; Rule 4
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
                      (setq not-indented nil))))))))
      (if cur-indent
          (progn
            (if (< cur-indent 0)
                (setq cur-indent 0))
            (indent-line-to cur-indent))
        (indent-line-to 0)))))

(define-derived-mode smudge-mode fundamental-mode
  (setq font-lock-defaults '(smudge-highlights))
  (setq mode-name "Smudge")
  (setq indent-line-function 'smudge-indent-line))

(provide 'smudge)
