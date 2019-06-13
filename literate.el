;; This file is generated from literate.lagda.

;; To use generic-mode later below.
(require 'generic-x)

(add-to-list 'org-src-lang-modes '("agda" . ob-agda))

(defface agda2-highlight-keyword-face
  '((t (:foreground "DarkOrange3")))
  "The face used for keywords."
    :group 'font-lock-faces)

(setq font-lock-keyword-face 'agda2-highlight-keyword-face)

(defface agda2-highlight-symbol-face
  '((((background light)) (:foreground "gray25"))
    (((background dark))  (:foreground "gray75")))
  "The face used for symbols like forall, =, as, ->, etc."
  :group 'font-lock-faces)

(setq org-agda-keywords
  '("=" "|" "->" "→" ":" "?" "\\" "λ" "∀" ".." "..." "abstract" "codata"
  "coinductive" "constructor" "data" "do" "eta-equality" "field"
  "forall" "hiding" "import" "in" "inductive" "infix" "infixl"
  "infixr" "instance" "let" "macro" "module" "mutual" "no-eta-equality"
  "open" "overlap" "pattern" "postulate" "primitive" "private" "public"
  "quote" "quoteContext" "quoteGoal" "quoteTerm" "record" "renaming"
  "rewrite" "Set" "syntax" "tactic" "unquote" "unquoteDecl" "unquoteDef"
  "using" "where" "with"))

(define-generic-mode

    'ob-agda-mode                      ;; name of the mode

    (list '("{-" . "-}"))               ;; comments delimiter

    org-agda-keywords

    ;; font lock list: Order of colouring matters;
    ;; the numbers refer to the subpart, or the whole(0), that should be coloured.
    (list

     ;; To begin with, after "module" or after "import" should be purple
     ;; Note the SPACE below.
     '("\\(module\\|import\\) \\([a-zA-Z0-9\-_\.]+\\)" 2 '((t (:foreground "purple"))))

     ;; Agda special symbols: as
     '(" as" 0 'agda2-highlight-symbol-face)

     ;; Type, and constructor, names begin with a capital letter  --personal convention.
     ;; They're preceded by either a space or an open delimiter character.
     '("\\( \\|\s(\\)\\([A-Z]+\\)\\([a-zA-Z0-9\-_]*\\)" 0 'font-lock-type-face)
     '("ℕ" 0 'font-lock-type-face)

     ;; variables & function names, as a personal convention, begin with a lower case
     '("\\([a-z]+\\)\\([a-zA-Z0-9\-_]*\\)" 0 '((t (:foreground "medium blue"))))

     ;; colour numbers
     '("\\([0-9]+\\)" 1   '((t (:foreground "purple"))))

     ;; other faces to consider:
     ;; 'font-lock-keyword-face 'font-lock-builtin-face 'font-lock-function-name-face
     ;; 'font-lock-variable-name-face 'font-lock-constant-face
     )

     ;; files that trigger this mode
     nil

     ;; any other functions to call
     nil

     ;; doc string
     "My custom Agda highlighting mode for use *within* Org-mode."
)

(provide 'org-agda-mode)

; (describe-symbol 'define-generic-mode)
; (describe-symbol 'font-lock-function-name-face)

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
        (here-column (current-column))
        (enable-local-variables :safe)
        )
    (rewrite-ends "\n\\begin{code}"              "\n\\end{code}"
                  "\n#+BEGIN_SRC org-agda"       "\n#+END_SRC")
    (rewrite-ends "\n\\begin{spec}"              "\n\\end{spec}"
                  "\n#+BEGIN_EXAMPLE org-agda"   "\n#+END_EXAMPLE")
    (org-mode)
    (org-goto-line here-line) ;; defined above
    (move-to-column here-column)
  )
  (message "Welcome to Org-mode, %s!" user-full-name)
)

(defun org-to-lagda ()
  "Transform Org-mode source blocks into literate Agda blocks.
   Use haskell as the Org source block language since I do not have nice colouring otherwise.
  "
  (interactive)
  (let ((here-line (line-number-at-pos)) ;; remember current line
        (here-column (current-column))  ;; and current column
        (enable-local-variables :safe))

    (rewrite-ends "\n#+BEGIN_SRC org-agda"       "\n#+END_SRC"
                  "\n\\begin{code}"              "\n\\end{code}")
    (rewrite-ends "\n#+BEGIN_EXAMPLE org-agda"   "\n#+END_EXAMPLE"
                  "\n\\begin{spec}"              "\n\\end{spec}")
    (agda2-mode)
    (sit-for 0.1) ;; necessary for the slight delay between the agda2 commands
    (agda2-load)
    (goto-line here-line)
    (move-to-column here-column)
  )
   (message "Welcome to Agda-mode, %s!" user-full-name)
)

(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "C-x C-a") 'org-to-lagda)))

(add-hook 'agda2-mode-hook
          (lambda () (local-set-key (kbd "C-x C-a") 'lagda-to-org)))
