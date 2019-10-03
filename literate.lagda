# -*- org -*-
#
# (org-babel-load-file "literate.lagda")

#+TITLE: Literate Agda with Org-mode
#+DESCRIPTION: An Org-mode utility for Agda.
#+AUTHOR: Musa Al-hassy
#+EMAIL: alhassy@gmail.com
#+DESCRIPTION: An Emacs mode for working with Agda code in an Org-mode like fashion, more or less.
#+DATE: 2018-11-03
#+STARTUP: indent
#+CATEGORIES: Agda Org Emacs
#+OPTIONS: html-postamble:nil toc:nil d:nil
#+IMAGE: ../assets/img/org_logo.png
#+SOURCE: https://raw.githubusercontent.com/alhassy/org-agda-mode/master/literate.lagda

# To avoid tangleing the agda blocks.
#+PROPERTY: header-args :tangle no

# (shell-command "export PATH=\"~/.cabal/bin/$PATH\"")
#
# LaTeX_HEADER:   \usepackage{agda}
# INCLUDE: ~/Dropbox/MyUnicodeSymbols.org

:hide:
#+BEGIN_SRC emacs-lisp  :tangle literate.el :comments link
;; Dependencies
(use-package s :demand t)
(use-package dash :demand t)
#+END_SRC
:end:

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
Moreover, we also allow user-defined colouring.

Jump to [[#installation]] to quickly get and use the setup.

( Thanks to [[https://github.com/armkeh][Mark Armstrong]] for significant testing and contributions! )

:Hide:
#+BEGIN_SRC emacs-lisp :tangle org-agda-mode.el
;; This file is generated from literate.lagda.
#+END_SRC
:End:

* Contents :TOC:QUOTE:ignore:
#+BEGIN_QUOTE
- [[#abstract][Abstract]]
- [[#agda-now-supports-org-files----not-really][“Agda now supports org files” ---Not Really]]
- [[#agda-syntax-highlighting][Agda Syntax Highlighting]]
  - [[#keywords][Keywords]]
  - [[#the-generic-mode-definition][The ~generic-mode~ Definition]]
  - [[#user-defined-colouring][User-defined Colouring]]
- [[#lagda-to-org-and-org-to-lagda][(~lagda-to-org~) and (~org-to-lagda~)]]
- [[#example-fragments][Example Fragments]]
- [[#summary][Summary]]
- [[#installation][Installation]]
- [[#sources-consulted][Sources Consulted]]
#+END_QUOTE

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

The solution outlined here is not to limit nor compromise each role, but rather
provide both and instead allow the user, you, to control when you would like
to be /documenting vs. developing/ ---the resulting system is sufficiently fast
to toggle between the modes; e.g., the somewhat large categorical development
[[https://alhassy.github.io/PathCat/][Graphs are to categories as lists are to monoids]] is written literately using org-agda.

Besides the core capability to switch between the different modes, we also provide
an elementary yet /extensible/ syntax colouring mechanism for Agda's non-standard highlighting.

* Agda Syntax Highlighting

We produce a new mode, calling it ~ob-agda-mode~,
so that Org-mode blocks marked with ~ob-agda~ will have Agda /approximated/
syntax. By default, if an Emacs major-mode ~<lang>-mode~ exists,
then blocks marked with ~<lang>~ use that major-mode for editing.

#+BEGIN_SRC emacs-lisp :tangle literate.el :comments link
;; To use generic-mode later below.
(require 'generic-x)
#+END_SRC

The “ob” is short for “org-babel” since we also wish to provide
Babel support for Agda. Using ~ob-agda~ marked blocks is awkward and exposes
some of our implementation, we will instead support an alias ~agda~ which refers to ~ob-agda~.

We can use the ~org-src-lang-modes~ variable to map any ---possibly more friendly or suggestive--- identifier to a language major mode.
#+BEGIN_SRC emacs-lisp :tangle literate.el :comments link
(add-to-list 'org-src-lang-modes '("agda" . ob-agda))
#+END_SRC

** Keywords

We look at the ~agda2-highlight.el~ source file from the Agda repository
for colours of keywords and reserved symbols such as ==, λ, ∀=, etc.

#+BEGIN_SRC emacs-lisp
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

#+BEGIN_SRC emacs-lisp
(setq org-agda-keywords
  '("=" "|" "->" "→" ":" "?" "\\" "λ" "∀" ".." "..." "abstract" "codata"
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

#+BEGIN_SRC emacs-lisp
; (defvar org-agda-extra-word-colours nil
; "other words that user of org-mode wants coloured, along with their specified font-lock-type-face")

;; When exporting to .lagda files, I overwrite these to "".
(defvar ob-agda-comment-start "{-")
(defvar ob-agda-comment-end "{-")

(define-generic-mode

    'ob-agda-mode                      ;; name of the mode

    (list (cons ob-agda-comment-start ob-agda-comment-end))               ;; comments delimiter

    org-agda-keywords

    ;; font lock list: Order of colouring matters;
    ;; the numbers refer to the subpart, or the whole(0), that should be coloured.

    (-concat  ;; ★★★ org-agda-extra-word-colours is a free variable,      ★★★
              ;; ★★★ user should define it /before/ loading org-agda-mode ★★★
               (if (boundp (quote org-agda-extra-word-colours)) org-agda-extra-word-colours nil)
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
     ))

     ;; files that trigger this mode
     nil

     ;; any other functions to call
     nil

     ;; doc string
     "My custom Agda highlighting mode for use *within* Org-mode."
)
#+END_SRC

I do not insist that ~org-agda-mode~ be activated on any particular files by default.

Here is an example code block that obtains this colouring schema.
#+BEGIN_SRC agda
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
#+END_SRC

Next, we turn to supporting Agda interactivity with holes.
** User-defined Colouring

Since true Agda colouring requires type-checking, it is desirable to allow the user to
input colouring for their own identifiers. Such <<<user-defined colouring>>> will be
via the delightful org-mode interface: A super simple intuitive table ♥‿♥

#+begin_quote org
For now, the user-defined Agda colouring mentioned here only serves for an enjoyable
literate programming experience. It currently is not picked up by the Org-mode LaTeX backend
nor the HTML backend.
#+end_quote

Anywhere in their buffer, the user should have a table with a column for identifiers
and the colours they should have, as follows.
#+BEGIN_SRC org
,#+RESULTS: ob-agda/colours
| one   | keyword       |
| two   | builtin       |
| three | function-name |
| four  | variable-name |
| five  | constant      |
#+END_SRC

:Hide:
#+RESULTS: ob-agda/colours
| one    | keyword       |
| twoish | builtin       |
| three  | function-name |
| four   | variable-name |
| five   | constant      |
:End:

Which yields the following colouring,
#+BEGIN_SRC agda
one   = Set
two   = Set
three = Set
four  = Set
five  = Set
#+END_SRC

We implement this as follows. We produce a function that realises such colouring assignments:
#+BEGIN_SRC emacs-lisp
(defun ob-agda/add-colour (word colour)
   "Refresh the ob-agda-mode to have the new ‘colour’ for ‘word’ in agda blocks.

    + ‘word’ is a string representing an Agda identifier.

    + ‘colour’ is either a symbol from ‘keyword’, ‘builtin’, ‘function-name’,
       ‘variable-name’, ‘constant’."
   ;; We only declare org-agda-extra-word-colours if the user needs it.
   ;; If we declare it in the file, as nil, then it will always be nil before
   ;; the ob-agda-mode is defined and so later changes to this variable will not take effect.
   ;;
   (unless (boundp (quote org-agda-extra-word-colours)) (setq org-agda-extra-word-colours nil))

   ;; Discard existing colour-scheme.
   (unload-feature 'ob-agda-mode)

   ;; Add new colour
   (if (-contains? '(keyword builtin function-name variable-name constant) colour)
       (add-to-list 'org-agda-extra-word-colours
                    `(,word 0 ,(intern (concat "font-lock-" (symbol-name colour) "-face"))))
     (message-box "colour %s" colour)
     (add-to-list 'org-agda-extra-word-colours
                  `(,word 0 ,colour)))

   ;; Load the new altered scheme.
   (require 'ob-agda-mode "~/.emacs.d/literate.el"))
#+END_SRC
:ExampleInovcations:
#+BEGIN_SRC emacs-lisp :tangle no
(add-to-list 'org-agda-extra-word-colours '("typeclass" 0 'agda2-highlight-keyword-face))
(add-to-list 'org-agda-extra-word-colours '("PackageFormer" 0 'font-lock-type-face))
(add-to-list 'org-agda-extra-word-colours '("_⨾_" 0 'font-lock-type-face))
(add-to-list 'org-agda-extra-word-colours '("assoc" 0 'font-lock-type-face))

; (ob-agda/add-colour "newkeyword" 'agda2-highlight-keyword-face)
(ob-agda/add-colour "newkeyword" 'builtin)
(ob-agda/add-colour "newkeyword" 'constant)
(ob-agda/add-colour "newkeyword" 'function-name)
#+END_SRC
:End:

Then lookup that user provided table, if it is there, and use it.
#+BEGIN_SRC emacs-lisp
(defun ob-agda/update-colours ()
 "Searchs current buffer for an ob-agda/colours named result table
  then uses that to update the colour scheme.
 "
 (interactive)
 (ignore-errors
   (save-excursion
     (org-babel-goto-named-result "ob-agda/colours")
     (forward-line)
     ;; (setq _it (org-table-to-lisp))
     (dolist (elem (org-table-to-lisp) org-agda-extra-word-colours)
       (ob-agda/add-colour (car elem) (intern (cadr elem)))))))
#+END_SRC

* (~lagda-to-org~) and (~org-to-lagda~)

Previously, Agda would not typecheck a non-~lagda~, or non-~agda~, file therefore
I could not use Org-mode multiple mode settings.

Recent versions of Agda will typecheck files with other extensions,
but as of 2.6.0, the interactive mode does not work on such files.

I will instead merely
swap the syntax of the modes then reload the desired mode.
--it may not be ideal, but it does what I want in a fast enough fashion.

In order to maintain position when switching back to Org-mode,
I define a function which not only goes to the appropriate line,
but unfolds the document to show that line.

#+BEGIN_SRC emacs-lisp :tangle literate.el :comments link
(defun org-goto-line (line)
  "Go to the indicated line, unfolding the parent Org header.

   Implementation: Go to the line, then look at the 1st previous
   org header, now we can unfold it whence we do so, then we go
   back to the line we want to be at."
  (goto-line line)
  (org-back-to-heading 1)
  (org-cycle)
  (goto-line line))
#+END_SRC
# MA: Warning, goto-line is different than forward-line!

Below we put together a way to make rewrites ~⟨pre⟩⋯⟨post⟩ ↦ ⟨newPre⟩⋯⟨newPost⟩~
then use that with the rewrite tokens being ~#+BEGIN_SRC~ and ~╲begin{code}~ for
literate Agda, as well as their closing partners.
# Using a real ‘\’ results in parse errors when Agda mode is activated.
#+BEGIN_SRC emacs-lisp :tangle literate.el :comments link
(defun rewrite-ends (pre post new-pre new-post)
  "Perform the following in-buffer rewrite: ⟨pre⟩⋯⟨post⟩ ↦ ⟨newPre⟩⋯⟨newPost⟩.
  For example, for rewriting begin-end code blocks from Org-mode to something
  else, say a language's default literate mode.

  The search for the string ⟨pre⟩⋯⟨post⟩ is non-greedy, i.e. will find
  (in order) the minimal strings matching ⟨pre⟩⋯⟨post⟩.

  We insist that the ends occur at the start of a newline; otherwise no
  rewrite is made. Note the “^” regexp marker below.

  In the arguments, only symbol `\` needs to be escaped."
  (let ((rx-pre  (concat "\\(^" (regexp-quote pre)  "\\)"))
        (rx-post (concat "\\(^" (regexp-quote post) "\\)"))
        ;; Code to match any characters (including newlines)
        ;; based on https://www.emacswiki.org/emacs/MultilineRegexp
        ;; This version requires we end in a newline,
        ;; and uses the “non-greedy” * operator, *?, so we will match the minimal string.
        (body "\\(.*\n\\)*?"))
    (goto-char (point-min))
    (while (re-search-forward (concat rx-pre body rx-post) nil t) ;; nil to search whole buffer, t to not error
      ;; Matched string 1 is the pre, matched string 3 is the post.
      ;; Optionals: fixed-case, literal, use buffer, substring
      (replace-match new-pre  t t nil 1)
      (replace-match new-post t t nil 3))))
#+END_SRC

The two rewriting utilities:
#+BEGIN_SRC emacs-lisp :tangle literate.el :comments link
(defun lagda-to-org ()
  "Transform literate Agda blocks into Org-mode source blocks.
   Use haskell as the Org source block language since I do not have nice colouring otherwise."
  (interactive)
  (let ((here-line (line-number-at-pos)) ;; remember current line
        (here-column (current-column))
        (enable-local-variables :safe))
    (rewrite-ends "\\begin{code}"          "\\end{code}"
                  "#+BEGIN_SRC agda"       "#+END_SRC")
    (rewrite-ends "\\begin{spec}"          "\\end{spec}"
                  "#+BEGIN_EXAMPLE agda"   "#+END_EXAMPLE")
    (org-mode)
    (org-goto-line here-line) ;; defined above
    (move-to-column here-column))
  (message "Welcome to Org-mode, %s!" user-full-name))

(defun org-to-lagda ()
  "Transform Org-mode source blocks into literate Agda blocks.
   Use haskell as the Org source block language since I do not have nice colouring otherwise."
  (interactive)
  (let ((here-line (line-number-at-pos)) ;; remember current line
        (here-column (current-column))  ;; and current column
        (enable-local-variables :safe))

    (rewrite-ends "#+BEGIN_SRC agda"       "#+END_SRC"
                  "\\begin{code}"          "\\end{code}")
    (rewrite-ends "#+BEGIN_EXAMPLE agda"   "#+END_EXAMPLE"
                  "\\begin{spec}"          "\\end{spec}")
    (agda2-mode)
    (sit-for 0.1) ;; necessary for the slight delay between the agda2 commands
    (agda2-load)
    (goto-line here-line)
    (move-to-column here-column))
  (message "Welcome to Agda-mode, %s!" user-full-name))
#+END_SRC

*Notice!* The toggling utilities automatically enable all /safe/ local variables
in an file ---c.f., the ~(enable-local-variables :all)~ lines above.
Many of our files tend to have local variables and that is the reason
we allow us.

Handy-dandy shortcuts, which are alternated on mode change:
#+BEGIN_SRC emacs-lisp :tangle literate.el :comments link
(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "C-x C-a") 'org-to-lagda)))

(add-hook 'agda2-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-a") 'lagda-to-org)
            (local-set-key (kbd "C-c C-v C-d")
                           (lambda (prefix)
                             (interactive "P") ;; Places value of universal argument into: current-prefix-arg
                             (insert (if (identity current-prefix-arg)
                                         "\n\\begin{spec}\n\n\\end{spec}"
                                       "\n\\begin{code}\n\n\\end{code}"))
                             (forward-line -1)))))
#+END_SRC

Org-mode, by default, lets us create a source block using ~C-c C-v C-d~, so we bring
this incantation to Agda-mode as well as having ~C-u C-c C-v C-d~ produce a ~spec~-environment.

* Summary

We now have the utility functions:

| _Command_ | _Action_                                                      |
| ~C-x C-a~ | transform org ~org-agda~ blocks to literate Agda blocs        |
| ~C-x C-a~ | transform literate Agda code delimiters to org ~org-agda~ src |

This was fun: I learned a lot of elisp!
Hopefully I can make use of this, in the small, if not in the large
--in which case I'll need to return to the many ~COMMENT~-ed out sections
in this document.

# -- E.g., this begin{code} won't be rewritten;
# -- neither will the in-line #+END_SRC. Nice!
#
# Alt+x describe-key Ctrl+h k, then type the key combination.
# (describe-function 'agda2-module-contents-maybe-toplevel)

* Installation
:PROPERTIES:
:CUSTOM_ID: installation
:END:

:Testing_install_in_a_clean_emacs:

 0. ~emacs -Q~

 1. In the scratch buffer, paste and execute the following.

:End:

 1. Add the following to the top of your Emacs configuration file, i.e., the =/.emacs= file.
    #+BEGIN_SRC emacs-lisp
(progn

(require 'package)
(push '("melpa-stable" . "http://stable.melpa.org/packages/") package-archives)
(package-initialize)
(package-refresh-contents)

;; Obtain & setup installation interface.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Necessary libraries for producing the system.
(use-package s)                  ;; “The long lost Emacs string manipulation library”.
(use-package dash)               ;; “A modern list library for Emacs”.

;; Next, obtain the Elisp file, load it, and attach it to Agda.
;; (shell-command "cp ~/org-agda-mode/literate.el ~/.emacs.d/literate.el")
(unless (file-exists-p "~/.emacs.d/literate.el")
  (shell-command (concat "curl "
    "https://raw.githubusercontent.com/alhassy/org-agda-mode/master/literate.el"
    ">> ~/.emacs.d/literate.el")))
(load-file "~/.emacs.d/literate.el")
;; (add-hook 'agda2-mode-hook (lambda () (load-file "~/.emacs.d/literate.el")))

;; Uncomment out the last line above if you want support for literate org-agda blocks
;; to ALWAYS be active on .lagda files.

;; You likely have this in your ~/.emacs file already
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "/usr/local/bin/agda-mode locate")))

) ;; ends the progn at the top.
#+END_SRC

 2. Make a new ~test.lagda~ file.
    #+BEGIN_SRC org :tangle test.lagda
# -*- org -*-
#
# (load-file "~/.emacs.d/literate.el")

Here's some sample fragments, whose editing can be turned on with ~C-x C-a~.

,* Example src

Press C-c C-v C-d to make src code blocks.

hello
\begin{code}
module test where

hole : Set₁
hole = {!!}
\end{code}
there

,* Example spec

A literate Agda ~spec~-ification environment, which corresponds to an Org-mode ~EXAMPLE~ block.

my
\begin{spec}
e : τ
\end{spec}
friends

In Agda mode, press C-u C-c C-v C-d to make spec blocks.
#+END_SRC

    # Useful for debugging.
    #
    :Hide:
    #+BEGIN_EXAMPLE emacs-lisp
    (unload-feature 'org-agda-mode)
    (load-file "org-agda-mode.el")
    #+END_EXAMPLE
    :End:

 3. Load the ~literate.el~ file.

 4. Now ~C-x C-a~ to switch to Agda mode and load the module.

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

# *TODO* Method to turn an begin{spec} into a begin{code}
# Maybe this exists for Org-blocks: SRC ↔ EXAMPLE?
# Maybe this exists for agda2-mode?


Allow colour to be  a face such as the symbol
‘agda2-highlight-keyword-face’


# Note that remembering-position in `rewrite-ends` does not work as expected
# since I would go to the position, then change to a different mode, say org-mode,
# which then folds all sections thereby obscuring the position we were at.

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

Evaluate the following source block with ~C-c C-c~ to produce a ~README~ file.

#+BEGIN_SRC emacs-lisp
(with-temp-buffer
    (insert
    "#+EXPORT_FILE_NAME: README.org
     # HTML: <h1> Programming Literately using Agda within Org-mode </h1>
     ,#+OPTIONS: toc:nil d:nil
     # Toc is displayed below at a strategic position.

     An Emacs mode for working with
     Agda code in an Org-mode like fashion, more or less.

     The following can also be read as an outdate [[https://alhassy.github.io/literate/][blog post]].

     Github recognizes ~.org~ files;
     Agda colouring is determined by typechecking, so Github will not provide certain colours.

     -----

     ,#+TOC: headlines 2
     ,#+INCLUDE: literate.lagda
    ")
    (org-mode)
    ;; (org-md-export-to-markdown)
    ;; (package-install 'toc-org)
    (toc-org-mode)
    (toc-org-insert-toc)
    ;; (setq org-toc-noexport-regexp ".*:ignore:.*") MA: Doesn't work.
    ;; (delete "TOC" org-export-exclude-tags)
    ;; (pop org-export-exclude-tags)
    (org-org-export-to-org)
    ;; (add-to-list 'org-export-exclude-tags "TOC")
)
#+END_SRC

#+RESULTS:
: README.org

Org Horizontal rules
A line consisting of only dashes, and at least 5 of them, will be exported as a horizontal line.

* COMMENT footer

Note the existence of: (agda2-restart)
org-shifttab
orgstruct-mode

NOTE: AlBasmala calls the source file NAME.org, so below we change that to
this file's name.

(preview-article :draft t)
(preview-article)

# Local Variables:
# eval: (visual-line-mode t)
# eval: (when nil (load-file "~/alhassy.github.io/content/AlBasmala.el"))
# eval: (setq file.org (buffer-name))
# eval: (setq pdfsLocation "~/alhassy.github.io/assets/pdfs/")
# eval: (org-babel-load-file "literate.lagda")
# eval: (progn (org-babel-goto-named-src-block "make-readme") (org-babel-execute-src-block) (outline-hide-sublevels 1))
# compile-command: (progn (org-babel-tangle) (my-org-html-export-to-html))
# End:
