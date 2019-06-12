#+TITLE: Literate Agda with Org-mode
#+DESCRIPTION: An Org-mode utility for Agda.
#+AUTHOR: Musa Al-hassy
#+EMAIL: alhassy@gmail.com
#+DESCRIPTION: An Emacs mode for working with Agda code in an Org-mode like fashion, more or less.
#+STARTUP: indent
#+CATEGORIES: Agda Org Emacs
#+OPTIONS: html-postamble:nil toc:nil
#+IMAGE: ../assets/img/org_logo.png
#+SOURCE: https://raw.githubusercontent.com/alhassy/org-agda-mode/master/literate.lagda

# (shell-command "export PATH=\"~/.cabal/bin/$PATH\"")
#
# LaTeX_HEADER:   \usepackage{agda}
# INCLUDE: ~/Dropbox/MyUnicodeSymbols.org

* Abstract       :ignore:
#+BEGIN_CENTER
*Abstract*
#+END_CENTER

[[https://en.wikipedia.org/wiki/Literate_programming][Literate Programming]] is essentially the idea that code is enclosed in documentation
rather than the comments being surrounded by code. The idea is that software
ought to be written like an essay to be read by a human; from this, code for the
machine can then be extracted.

The articles on this blog are meant to be in such a format and as such
I use [[https://www.offerzen.com/blog/literate-programming-empower-your-writing-with-emacs-org-mode][Org-mode]] as my markup for producing the HTMLs and PDFs.

This article aims to produce an Org-friendly approach to working
with the [[http://wiki.portal.chalmers.se/agda/pmwiki.php][Agda language]], which is special in comparison to many other languages:
Coding is interactive via holes and it permits almost any sequence of characters
as a legal lexeme thereby rendering a static highlighting theme impossible.

The result of this Elisp exploration is that by ~C-x C-a~
we can toggle into Agda-mode and use its interactive features to construct our program;
then return to an Org-mode literate programming style afterwards with
another ~C-x C-a~
---/both translations remember the position we're working at and allow the editing features of their respective modes!/

( Thanks to [[https://github.com/armkeh][Mark Armstrong]] for significant testing and contributions! )

#+HTML: <!--
#+BEGIN_SRC emacs-lisp
;; This file is generated from literate.lagda.
#+END_SRC
#+BEGIN_SRC emacs-lisp :tangle org-agda-mode.el
;; This file is generated from literate.lagda.
#+END_SRC
#+HTML: -->

* “Agda now supports org files” ---Not Really

As of Agda 2.6.0 ---which came after this article was originally written---
there is now support for literate Org-mode support using ~agda2~ org-src blocks.

The [[https://github.com/agda/agda/pull/3548][pull request]] was by one of my then students who found the use of this ‘org-agda’
setup to be sufficiently useful to be appreciated by the whole Agda community out-of-the-box.

Unfortunately, currently working with a ~myfile.lagda.org~
comes with discouraging compromises between the Org- and Agda-modes. Namely:
1. Interactivity with Agda holes is /not/ supported.
2. The full editorial capabilities of Org-mode are limited since some
   features clash with those of Agda-mode.

The solution outline here is not to limit or compromise each role, but rather
provide both and instead allow the user, you, to control when you would like
to be /documenting vs. developing/ ---the resulting system is sufficiently fast
to toggle between the modes; e.g., the somewhat large categorical development
[[https://alhassy.github.io/PathCat/][Graphs are to categories as lists are to monoids]] is written literately using org-agda.

Besides the core capability to switch between the different modes, we also provide
an elementary yet /extensible/ syntax colouring mechanism for Agda's non-standard highlighting.

* Agda Syntax Highlighting With ~org-agda-mode~

We produce a new mode in a file named ~org-agda-mode.el~
so that Org-mode blocks marked with ~org-agda~ will have Agda /approximated/
syntax.

#+BEGIN_SRC emacs-lisp :tangle org-agda-mode.el
;; To use generic-mode later below.
(require 'generic-x)
#+END_SRC

** Keywords

We look at the ~agda2-highlight.el~ source file from the Agda repository
for colours of keywords and reserved symbols such as ~=, ∀,~ etc.

#+BEGIN_SRC emacs-lisp :tangle org-agda-mode.el
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
#+END_SRC

From Agda's [[https://agda.readthedocs.io/en/v2.5.4.1/language/lexical-structure.html?highlight=keywords][“read the docs”]] website, we obtain the keywords for the language:

#+BEGIN_SRC emacs-lisp :tangle org-agda-mode.el
(setq org-agda-keywords '("=" "|" "->" "→" ":" "?" "\\" "λ" "∀" ".." "..." "abstract" "codata"
                          "coinductive" "constructor" "data" "do" "eta-equality" "field"
                          "forall" "hiding" "import" "in" "inductive" "infix" "infixl"
                          "infixr" "instance" "let" "macro" "module" "mutual" "no-eta-equality"
                          "open" "overlap" "pattern" "postulate" "primitive" "private" "public"
                          "quote" "quoteContext" "quoteGoal" "quoteTerm" "record" "renaming"
                          "rewrite" "Set" "syntax" "tactic" "unquote" "unquoteDecl" "unquoteDef"
                          "using" "where" "with"))
#+END_SRC

** The ~generic-mode~ Definition

Agda colouring is approximated as defined below, but a convention is made:
Function symbols begin with a lower case letter, whereas type symbols begin
with a capital letter. Otherwise, I would need to resort to Agda's mechanism
for determining whether a name is a type or not:
#+BEGIN_CENTER
/Parsing is Typechecking!/
#+END_CENTER

#+BEGIN_SRC emacs-lisp :tangle org-agda-mode.el
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
#+END_SRC

I do not insist that ~org-agda-mode~ be activated on any particular files by default.

* (~lagda-to-org~) and (~org-to-lagda~)

Previously, Agda would not typecheck a non-~lagda~, or non-~agda~, file therefore
I could not use Org-mode multiple mode settings.

Recent versions of Agda will typecheck files with other extensions,
but as of 2.6.0, the interactive mode does not work on such files.

I will instead merely
swap the syntax of the modes then reload the desired mode.
--It may not be ideal, but it does what I want in a fast enough fashion.

In order to maintain position when switching back to Org-mode,
I define a function which not only goes to the appropriate line,
but unfolds the document to show that line.

#+BEGIN_SRC emacs-lisp
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
#+END_SRC

Below we put together a way to make rewrites ~⟨pre⟩⋯⟨post⟩ ↦ ⟨newPre⟩⋯⟨newPost⟩~
then use that with the rewrite tokens being ~#+BEGIN_SRC~ and ~|begin{code}~ for
literate Agda, as well as their closing partners.
# Using a real `\' results in parse errors when Agda mode is activated.

#+BEGIN_SRC emacs-lisp
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
#+END_SRC

# Note that remembering-position in `rewrite-ends` does not work as expected
# since I would go to the position, then change to a different mode, say org-mode,
# which then folds all sections thereby obscuring the position we were at.

The two rewriting utilities:

#+BEGIN_SRC emacs-lisp
(defun lagda-to-org ()
  "Transform literate Agda blocks into Org-mode source blocks.
   Use haskell as the Org source block language since I do not have nice colouring otherwise.
  "
  (interactive)
  (let ((here-line (line-number-at-pos)) ;; remember current line
        (here-column (current-column)))
    (rewrite-ends "\n\\begin{code}"              "\n\\end{code}"
                  "\n#+BEGIN_SRC org-agda"       "\n#+END_SRC")
    (rewrite-ends "\n\\begin{spec}"              "\n\\end{spec}"
                  "\n#+BEGIN_EXAMPLE org-agda"   "\n#+END_EXAMPLE")
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
)
#+END_SRC

Handy-dandy shortcuts, which are alternated on mode change:

#+BEGIN_SRC emacs-lisp
(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "C-x C-a") 'org-to-lagda)))
(add-hook 'agda2-mode-hook
          (lambda () (local-set-key (kbd "C-x C-a") 'lagda-to-org)))
#+END_SRC

# *TODO* Method to turn an begin{spec} into a begin{code}
# Maybe this exists for Org-blocks: SRC ↔ EXAMPLE?
# Maybe this exists for agda2-mode?

* Example

# Useful for debugging.
#
#+HTML: <!--
#+BEGIN_EXAMPLE emacs-lisp
(unload-feature 'org-agda-mode)
(load-file "org-agda-mode.el")
#+END_EXAMPLE
#+HTML: -->

Here's some sample fragments, whose editing can be turned on with ~C-x C-a~.
\begin{code}
module literate where

data ℕ : Set where
  Zero : ℕ
  Succ : ℕ → ℕ

double : ℕ → ℕ
double Zero = Zero
double (Succ n) = Succ (Succ (double n))

{- lengthy
      multiline
        comment -}

{- No one line comment colouring … Yet -}

open import Data.Nat as Lib

camelCaseIdentifier-01 : Lib.ℕ
camelCaseIdentifier-01 = let it = 1234 in it

postulate magic : Set

hole : magic
hole = {!!}

\end{code}

Here's a literate Agda ~spec~-ification environment, which corresponds to an Org-mode ~EXAMPLE~ block.
#+BEGIN_EXAMPLE org-agda
module this-is-a-spec {A : Set} (_≤_ : A → A → Set) where

  maximum-specfication : (candidate : A) → Set
  maximum-specfication c = ?
#+END_EXAMPLE

* Summary
# of Utilities Provided

We now have the utility functions:

| _Command_ | _Action_                                                      |
| ~C-x C-a~ | transform org ~org-agda~ blocks to literate Agda blocs        |
| ~C-x C-o~ | transform literate Agda code delimiters to org ~org-agda~ src |

This was fun: I learned a lot of elisp!
Hopefully I can make use of this, in the small, if not in the large
--in which case I'll need to return to the many ~COMMENT~-ed out sections
in this document.

# -- E.g., this begin{code} won't be rewritten;
# -- neither will the in-line #+END_SRC. Nice!
#
# Alt+x describe-key Ctrl+h k, then type the key combination.
# (describe-function 'agda2-module-contents-maybe-toplevel)

* Sources Consulted

+ [[http://www.ergoemacs.org/emacs/elisp_syntax_coloring.html][How to Write a Emacs Major Mode for Syntax Coloring]]
+ [[https://stackoverflow.com/questions/3887372/simplest-emacs-syntax-highlighting-tutorial][Simplest Emacs Syntax Highlighting Tutorial]]
+ [[https://stackoverflow.com/questions/1063115/a-hello-world-example-for-a-major-mode-in-emacs][“Hello World” for Emacs' Major Mode Creation]]
+ [[http://www.wilfred.me.uk/blog/2015/03/19/adding-a-new-language-to-emacs/][Adding A New Language to Emacs]]
+ [[https://nullprogram.com/blog/2013/02/06/][How to Make an Emacs Minor Mode]]
+ [[https://www.offerzen.com/blog/literate-programming-empower-your-writing-with-emacs-org-mode][Literate Programming: Empower Your Writing with Emacs Org-Mode]]
  - An elegant overview of literate programming, with Org-mode, and the capabilities it offers.
+ [[http://howardism.org/Technical/Emacs/literate-programming-tutorial.html][Introduction to Literate Programming]]
  - A nearly /comprehensive/ workshop on the fundamentals of literate programming with Org-mode.

* COMMENT Things that need to be adapted
1. Be capitalisation independent? E.g., SRC ≈ src?
2. Discussion on other options: outline-mode or outshine or multimod.e
3. Improved integration with [[https://alhassy.github.io/AlBasmala/][AlBasmala]] for better resulting PDFs.
4. Switch between EXAMPLE & SRC; ie begin vs spec.

* COMMENT Construction Sites to Eventually Return to :backlog:
** DONE [OLD] ~lagda-to-org~ and ~org-to-lagda~                :works:abandoned:

Rather than using a multiple mode setting, I will merely
swap the syntax of the modes then reload the desired mode.

(describe-symbol 'replace-regexp-in-string)

#+BEGIN_SRC emacs-lisp
(defun lagda-to-org ()
  "Transform literate Agda blocks into Org-mode source blocks.
   Use haskell as the Org source block language since I do not have nice colouring otherwise.
  "
  (interactive)
  (mapsto "^\\\\begin{code}" "#+BEGIN_SRC haskell")
  (mapsto "^\\\\end{code}"   "#+END_SRC")
  (let ((here (what-line))) ;; remember current line
    (org-mode)
    (org-goto-line here)    ;; personal function, see my init.org
  )
)

(defun org-to-lagda ()
  "Transform Org-mode source blocks into literate Agda blocks.
   Use haskell as the Org source block language since I do not have nice colouring otherwise.
  "
  (interactive)
  (mapsto "#\\+BEGIN_SRC haskell" "\\\\begin{code}")
  (mapsto "^#\\+END_SRC" "\\\\end{code}")
  (agda2-mode)
  (sit-for 1) ; necessary for the slight delay between the agda2 commands
  (agda2-load)
)

;; These are local to the buffer that loads this file.

(local-set-key (kbd "C-x C-a") 'org-to-lagda)
(local-set-key (kbd "C-x C-o") 'lagda-to-org)
#+END_SRC

*** TODO fixes

+ Only rewrite #+Begin_Src haskell ... #+End_Src
  - Currently we rewrite the #+End_Src of *all* blocks.

+ Accommodate for ~spec~ environments via #+Begin_Example.
** COMMENT Working with ~AlBasmala~         :feature:CONSTRUCTION_SITE:broken:

The ~AlBasmala~ toolkit works with ~.org~ files thereby
necessitating the creation of such a file before the
three main ~AlBasmala~ features: Preview, Commit, & Publish.

#+BEGIN_SRC emacs-lisp

;; The following three blocks of code are nearly identical
;; ( indeed they were quickly copied and pasted with minor alterations! )
;; as such it's best to clean this up by making a general parent function.

(defun lagda-with-albasmala-preview ()
   (interactive)
   (let* ((name-lagda (buffer-name))
          (name       (file-name-sans-extension name-lagda))
          (name-org   (concat name ".org"))
         )
   (copy-file name-lagda name-org 'overwrite) ;; Produce an org-file.
   (find-file name-org)
   (setq blogNAMEmd "why??? do i have to do this to blogNAMEmd??")
   (load-file "~/alhassy.github.io/content/AlBasmala.el")
   (preview-article)
   (kill-buffer)
   (delete-file name-org)
   )
)

(defun lagda-with-albasmala-commit ()
   (interactive)
   (let* ((name-lagda (buffer-name))
          (name       (file-name-sans-extension name-lagda))
          (name-org   (concat name ".org"))
         )
   (copy-file name-lagda name-org 'overwrite) ;; Produce an org-file.
   (find-file name-org)
   (setq blogNAMEmd "why??? do i have to do this to blogNAMEmd??")
   (load-file "~/alhassy.github.io/content/AlBasmala.el")
   (preview-article)
   (kill-buffer)
   (delete-file name-org)
   )
)

(defun lagda-with-albasmala-publish ()
   (interactive)
   (let* ((name-lagda (buffer-name))
          (name       (file-name-sans-extension name-lagda))
          (name-org   (concat name ".org"))
         )
   (copy-file name-lagda name-org 'overwrite) ;; Produce an org-file.
   (find-file name-org)
   (setq blogNAMEmd "why??? do i have to do this to blogNAMEmd??")
   (load-file "~/alhassy.github.io/content/AlBasmala.el")
   (publish)
   (kill-buffer)
   (delete-file name-org)
   )
)

(global-set-key (kbd "<f7>") 'lagda-with-albasmala-preview)
(global-set-key (kbd "<f8>") 'lagda-with-albasmala-commit)
(global-set-key (kbd "<f9>") 'lagda-with-albasmala-publish)

#+END_SRC

** COMMENT adaptions of ~MusaAgdaColour~ :feature:CONSTRUCTION_SITE:incomplete:

  "Produce coloured agda for given `NAME.lagda`, where `DirNameExt=Directory/NAME.ext`.

   If the `preview` flag is true, then we move the resulting PDF to the directory of
   the parent file, the one containg the #+INCLUDE. Then open the PDF for a glance.
   This is nice for previewing ; debugging individual files.
  "
#+BEGIN_SRC emacs-lisp
(setq DirNameExt (expand-file-name (buffer-name)))

; E.g., = Structures/TwoSorted.tex
(message (concat "Begun Colour Processing " DirNameExt))

(setq DirNAME (file-name-sans-extension DirNameExt)) ;       = Structures/TwoSorted
(setq NAME    (file-name-nondirectory DirNAME))      ;       = TwoSorted
(setq Dir     (file-name-directory    DirNAME))      ;       = Structures/

(setq DirNAMELagda     (concat DirNAME ".lagda"))
(setq InsertPostMatter (concat "echo \"\\end{document}\" >> " DirNAMELagda))
(setq DeletePostMatter (concat "sed -i '$ d' " DirNAMELagda))

(setq InsertPreMatter  (concat "sed -i '01i \\ \\\\documentclass[11pt]{article} \\
  \\\\usepackage{agda}                      \\
  \\\\usepackage{RathAgdaChars}             \\
  \\\\usepackage{verbatim}                   \\
  \\\\DeclareUnicodeCharacter{737}{\\\\ensuremath{737}} \\
  \\\\DeclareUnicodeCharacter{119922}{\\\\ensuremath{119922}} \\
  \\\\begin{document}' " DirNAMELagda))

(setq DeletePreMatter (concat "sed -e '1,7d' -i " DirNAMELagda))


(defun show-me-the-pretty ()

  (interactive)

  ;; Generate latex file, duh.
  (org-latex-export-to-latex)

  ;; Call that latex file a Literate Agda file.
  ;; Delete any exisitng ones lest we don't succeed.
  (delete-file "literate2.lagda")
  (rename-file "literate.tex" "literate2.lagda")

  ;; For some reason I get option clashes with: \usepackage[utf8]{inputenc}
  ;; So I'll erase it for now.
  (re-replace-in-file "literate2.lagda"
                      "\\\\usepackage\\[utf8\\]{inputenc}"
                      (lambda (x) "% musa commented out: inputenc "))


  (shell-command "agda --latex literate2.lagda && cd latex && pdflatex literate2.tex")
  ;; In-case our Agda code has an issue and it is displayed in the shell, show the shell regardless:
  (split-window-right 100) (other-window 1) (switch-to-buffer "*Shell Command Output*")

  ;; Santize and relocate the agda coloured tex file, as well as the generated PDF
  (copy-file "latex/literate2.tex" "./literate_coloured.tex" 'please-overwrite)
  (copy-file "latex/literate2.pdf" "./literate_coloured.pdf" 'please-overwrite)

  ;; to make the coloured tex standalone, also need the agda.sty
  (copy-file "latex/agda.sty" "./agda.sty" 'please-overwrite)

  (shell-command "evince literate_coloured.pdf")

  (kill-other-buffers)
)

;;
;; (progn
;;   (find-file "literate2.lagda")
;;   (goto-line 4)
;;   (kill-line)
;;   (save-buffer)
;;   (kill-buffer)
;; )

;; ToDo
;; replace spec with verbatim
;; ??? " ; sed -i 's/\\\\begin{spec}/\\\\begin{verbatim}/g' DirNAMELagda "
;; ??? " ; sed -i 's/\\\\end{spec}/\\\\end{verbatim}/g' DirNAMELagda "

(setq DirNAME (concat DirNAME "2"))
(setq NAME (concat NAME "2"))

;; (eshell-command (concat "DirNAME=" DirNAME "; Dir=. ; NAME=" NAME
;;                "; agda --latex $DirNAME.lagda "
;;                "&& cd latex && pdflatex $DirNAME.tex "))
;;
;; DirNAME=/home/musa/Dropbox/literate2; Dir=. ; NAME=literate2; agda --latex $DirNAME.lagda && cd latex && pdflatex $DirNAME.tex

(describe-function 'copy-file)

; The resuling pdf is not in Dir but rather in latex/, so we move NAME.pdf rather than DirNAME.pdf.
; (if preview "&& mv $NAME.pdf ../$Dir && cd .. && (evince $DirNAME.pdf &)" "")

;; ToDo
;;
;; delete prematter and postmatter from literate_coloured.tex
;; to obtain a coloured version of the file that can then be included in
;; other files. Why? For multi-file Agda programs that we'd like to colour.
;; prematter: top-\begin{document}\n\maketitle
;; postmatter: \end{document}-end.
;;
;; my old incantations from MusaAgdaColour.el:
;; (concat "; sed -i '$ d' ../" DirNAME ".tex") ;; delete postmatter
;;   (concat "; sed -e '1,7d' -i ../" DirNAME ".tex")    ;; delete prematter

#+END_SRC

** DONE Possible routes of activation :not_sure_i_actually_want_this:

We can simply load in the elisp file every time we open up a
~.lagda~ file, or we can place such a call into the local variables
of a file, or alternatively we execute the following /once/.
#+BEGIN_SRC emacs-lisp :tangle no
(set 'agda2-mode-hook nil)
(add-hook 'agda2-mode-hook
  (lambda ()
   (org-babel-load-file "~/Dropbox/lagda-with-org.org")

   (message "Loaded lagda-with-org")
  )
)

#+END_SRC

* COMMENT README

C-c C-c: evalute src block

#+NAME: make-readme
#+BEGIN_SRC emacs-lisp :results none
(with-temp-buffer
    (insert
    "#+EXPORT_FILE_NAME: README.md
     #+HTML: <h1> org-agda-mode </h1>

     An Emacs mode for working with
     Agda code in an Org-mode like fashion, more or less.

     The following can also be read as a [[https://alhassy.github.io/literate/][blog post]].

     #+TOC: headlines 2
     #+INCLUDE: literate.lagda
    ")
    ;; (set-visited-file-name "ReadIt2.md")
    (org-mode)
    (org-md-export-to-markdown)
)
#+END_SRC

* COMMENT footer

Note the existence of: (agda2-restart)
org-shifttab
orgstruct-mode

NOTE that AlBasmala calls the source file NAME.org, so below we change that to
this file's name.

# Local Variables:
# eval: (visual-line-mode t)
# eval: (when nil (load-file "~/alhassy.github.io/content/AlBasmala.el"))
# eval: (setq NAMEorg (buffer-name))
# eval: (setq pdfsLocation "~/alhassy.github.io/assets/pdfs/")
# eval: (org-mode)
# eval: (org-babel-tangle)
# eval: (org-babel-load-file "literate.lagda")
# eval: (progn (org-babel-goto-named-src-block "make-readme") (org-babel-execute-src-block) (outline-hide-sublevels 1))
# compile-command: (progn (org-babel-tangle) (my-org-html-export-to-html))
# End:
