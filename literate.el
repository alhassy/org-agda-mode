;; This file is generated from literate.lagda.

;; “The long lost Emacs string manipulation library”
;; https://github.com/magnars/s.el
(require 's)

(defun strip (pre post it)  
  "A simple extraction: it = ⟨pre⟩it₀⟨post⟩ ↦ it₀." 
  (s-chop-prefix pre (s-chop-suffix post it)) )

(defun rewrite-ends (pre post newPre newPost)
  "Perform the following in-buffer rewrite: ⟨pre⟩⋯⟨post⟩ ↦ ⟨newPre⟩⋯⟨newPost⟩.
  For example, for rewriting begin-end code blocks from Org-mode to something
  else, say a language's default literate mode.

  Warning: The body, the “⋯”, cannot contain the `#` character.
  I do this so that the search does not go to the very last occurence of `#+END_SRC`;
  which is my primary instance of `pre`.

  In the arguments, only symbol `\` needs to be escaped.

  Implementation: Match the pre, then any characteer that is not `#`, then the post.
  Hence, the body cannot contain a `#` character!
  In Agda this is not an issue, since we can use its Unicode cousin `♯` instead.
  "
  (let* ((rxPre     (regexp-quote pre))
         (rxPost    (regexp-quote post))
         (altered (replace-regexp-in-string (concat rxPre "\\([^\\#]\\|\n\\)*" rxPost)
                  (lambda (x) (concat newPre (strip pre post x) newPost))
                  (buffer-string) 'no-fixed-case 'new-text-is-literal)))
      (erase-buffer)
      (insert altered)
   )
)

(defun lagda-to-org ()
  "Transform literate Agda blocks into Org-mode source blocks.
   Use haskell as the Org source block language since I do not have nice colouring otherwise.
  "
  (interactive)
  (let ((here (line-number-at-pos))) ;; remember current line
    (rewrite-ends "\\begin{code}\n" "\n\\end{code}" "#+BEGIN_SRC org-agda\n" "\n#+END_SRC")
    (rewrite-ends "\\begin{spec}\n" "\n\\end{spec}" "#+BEGIN_EXAMPLE org-agda\n" "\n#+END_EXAMPLE")
    ;; (sit-for 2) ;; necessary for the slight delay between the agda2 commands
    (org-mode)
    (org-goto-line here)    ;; personal function, see my init.org
  )
)

(defun org-to-lagda ()
  "Transform Org-mode source blocks into literate Agda blocks.
   Use haskell as the Org source block language since I do not have nice colouring otherwise.
  "
  (interactive)
  (let ((here (line-number-at-pos))) ;; remember current line
    (rewrite-ends "#+BEGIN_SRC org-agda\n" "#+END_SRC" "\\begin{code}\n" "\\end{code}")
    (rewrite-ends "#+BEGIN_EXAMPLE org-agda\n" "#+END_EXAMPLE" "\\begin{spec}\n" "\\end{spec}")
    (agda2-mode)
    (sit-for 1) ;; necessary for the slight delay between the agda2 commands
    (agda2-load)
    (goto-line here)
  )
)

(local-set-key (kbd "C-x C-a") 'org-to-lagda)
(local-set-key (kbd "C-x C-o") 'lagda-to-org)

;; Maybe consider a simple “toggle” instead?
