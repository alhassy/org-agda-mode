;; This file is generated from literate.lagda.

;; To use generic-mode later below.
(require 'generic-x)

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

(setq org-agda-keywords '("=" "|" "->" "→" ":" "?" "\\" "λ" "∀" ".." "..." "abstract" "codata"
                          "coinductive" "constructor" "data" "do" "eta-equality" "field"
                          "forall" "hiding" "import" "in" "inductive" "infix" "infixl"
                          "infixr" "instance" "let" "macro" "module" "mutual" "no-eta-equality"
                          "open" "overlap" "pattern" "postulate" "primitive" "private" "public"
                          "quote" "quoteContext" "quoteGoal" "quoteTerm" "record" "renaming"
                          "rewrite" "Set" "syntax" "tactic" "unquote" "unquoteDecl" "unquoteDef"
                          "using" "where" "with"))

(define-generic-mode
    'org-agda-mode                      ;; name of the mode
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
     '("\\([0-9]+\\)" 1   '((t (:foreground "purple")))) ;; 'font-lock-constant-face)

     ;; other faces to consider:
     ;; 'font-lock-keyword-face 'font-lock-builtin-face 'font-lock-function-name-face
     ;;' font-lock-variable-name-face
     )

     nil                                                   ;; files that trigger this mode
     nil                                                   ;; any other functions to call
    "My custom Agda highlighting mode for use *within* Org-mode."     ;; doc string
)

(provide 'org-agda-mode)

; (describe-symbol 'define-generic-mode)
; (describe-symbol 'font-lock-function-name-face)
