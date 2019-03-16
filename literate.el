;; This file has diverged from literate.lagda.
;;
;; Changes:
;; - rewrote rewrite ends
;; - inserted org-goto-line
;; - made the mode switch a toggle bound to C-x C-a

(defun org-goto-line (line)
  "Go to the indicated line, unfolding the parent Org header.

   Implementation: Go to the line, then look at the 1st previous
   org header, now we can unfold it whence we do so, then we go
   back to the line we want to be at.
  "
  (goto-line line)
  (org-back-to-heading 1)
  (org-cycle)
  (goto-line line)
  )

(defun rewrite-ends (pre post new-pre new-post)
  "Perform the following in-buffer rewrite: ⟨pre⟩⋯⟨post⟩ ↦ ⟨newPre⟩⋯⟨newPost⟩.
  For example, for rewriting begin-end code blocks from Org-mode to something
  else, say a language's default literate mode.

  The search for the string ⟨pre⟩⋯⟨post⟩ is non-greedy, i.e. will find
  (in order) the minimal strings matching ⟨pre⟩⋯⟨post⟩.

  In the arguments, only symbol `\` needs to be escaped.
  "
  (let ((rx-pre  (concat "\\(" (regexp-quote pre)  "\\)"))
        (rx-post (concat "\\(" (regexp-quote post) "\\)"))
        ;; Code to match any characters (including newlines) based on https://www.emacswiki.org/emacs/MultilineRegexp
        ;; This version requires we end in a newline,
        ;; and uses the “non-greedy” * operator, *?, so we will match the minimal string.
        (body "\\(.*\n\\)*?"))
    (beginning-of-buffer)
    (while (re-search-forward (concat rx-pre body rx-post) nil t) ;; nil to search whole buffer, t to not error
      ;; Matched string 1 is the pre, matched string 3 is the post.
      ;; Optionals: fixed-case, literal, use buffer, substring
      (replace-match new-pre  t t nil 1)
      (replace-match new-post t t nil 3)
      )
    )
  )


(defun lagda-to-org ()
  "Transform literate Agda blocks into Org-mode source blocks.
   Use haskell as the Org source block language since I do not have nice colouring otherwise.
  "
  (interactive)
  (let ((here-line (line-number-at-pos)) ;; remember current line
	(here-column (current-column)))
    (rewrite-ends "\\begin{code}" "\\end{code}" "#+BEGIN_SRC org-agda" "#+END_SRC")
    (rewrite-ends "\\begin{spec}" "\\end{spec}" "#+BEGIN_EXAMPLE org-agda" "#+END_EXAMPLE")
    ;; (sit-for 2) ;; necessary for the slight delay between the agda2 commands
    (org-mode)
    (org-goto-line here-line) ;; defined above
    (move-to-column here-column)
  )
)

(defun org-to-lagda ()
  "Transform Org-mode source blocks into literate Agda blocks.
   Use haskell as the Org source block language since I do not have nice colouring otherwise.
  "
  (interactive)
  (let ((here-line (line-number-at-pos)) ;; remember current line
	(here-column (current-column)))  ;; and current column
    (rewrite-ends "#+BEGIN_SRC org-agda" "#+END_SRC" "\\begin{code}" "\\end{code}")
    (rewrite-ends "#+BEGIN_EXAMPLE org-agda" "#+END_EXAMPLE" "\\begin{spec}" "\\end{spec}")
    (agda2-mode)
    (sit-for 1) ;; necessary for the slight delay between the agda2 commands
    (agda2-load)
    (goto-line here-line)
    (move-to-column here-column)
  )
)

(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "C-x C-a") 'org-to-lagda)))
(add-hook 'agda2-mode-hook
          (lambda () (local-set-key (kbd "C-x C-a") 'lagda-to-org)))
