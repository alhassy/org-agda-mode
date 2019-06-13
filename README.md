<h1> org-agda </h1>

An Emacs mode for working with
Agda code in an Org-mode like fashion, more or less.

The following can also be read as a [blog post](https://alhassy.github.io/literate/).


# Table of Contents

1.  [“Agda now supports org files” &#x2014;Not Really](#orge0028ed)
2.  [Agda Syntax Highlighting](#org6e7e25f)
    1.  [Keywords](#orgbc189d9)
    2.  [The `generic-mode` Definition](#orgd975c1e)
    3.  [User-defined Colouring](#org76a9760)
3.  [(`lagda-to-org`) and (`org-to-lagda`)](#org9f9594f)
4.  [Example](#orgfc76e4a)
5.  [Summary](#org2c2cd30)
6.  [Sources Consulted](#org75a5a73)

**TODO:** require dash and s? document these dependencies.

<div class="org-center">
**Abstract**
</div>

[Literate Programming](https://en.wikipedia.org/wiki/Literate_programming) is essentially the idea that code is enclosed in documentation
rather than the comments being surrounded by code. The idea is that software
ought to be written like an essay to be read by a human; from this, code for the
machine can then be extracted.

The articles on this blog are meant to be in such a format and as such
I use [Org-mode](https://www.offerzen.com/blog/literate-programming-empower-your-writing-with-emacs-org-mode) as my markup for producing the HTMLs and PDFs.

This article aims to produce an Org-friendly approach to working
with the [Agda language](http://wiki.portal.chalmers.se/agda/pmwiki.php), which is special in comparison to many other languages:
Coding is interactive via holes and it permits almost any sequence of characters
as a legal lexeme thereby rendering a static highlighting theme impossible.

The result of this Elisp exploration is that by `C-x C-a`
we can toggle into Agda-mode and use its interactive features to construct our program;
then return to an Org-mode literate programming style afterwards with
another `C-x C-a`
---*both translations remember the position we're working at and allow the editing features of their respective modes!*
Moreover, we also allow user-defined colouring.

( Thanks to [Mark Armstrong](https://github.com/armkeh) for significant testing and contributions! )

<!--

    ;; This file is generated from literate.lagda.

    ;; This file is generated from literate.lagda.

-->


<a id="orge0028ed"></a>

# “Agda now supports org files” &#x2014;Not Really

As of Agda 2.6.0 &#x2014;which came after this article was originally written&#x2014;
there is now support for literate Org-mode support using `agda2` org-src blocks.

The [pull request](https://github.com/agda/agda/pull/3548) was by one of my then students who found the use of this ‘org-agda’
setup to be sufficiently useful to be appreciated by the whole Agda community out-of-the-box.

Unfortunately, currently working with a `myfile.lagda.org`
comes with discouraging compromises between the Org- and Agda-modes. Namely:

1.  Interactivity with Agda holes is *not* supported.
2.  The full editorial capabilities of Org-mode are limited since some
    features clash with those of Agda-mode.

The solution outline here is not to limit or compromise each role, but rather
provide both and instead allow the user, you, to control when you would like
to be *documenting vs. developing* &#x2014;the resulting system is sufficiently fast
to toggle between the modes; e.g., the somewhat large categorical development
[Graphs are to categories as lists are to monoids](https://alhassy.github.io/PathCat/) is written literately using org-agda.

Besides the core capability to switch between the different modes, we also provide
an elementary yet *extensible* syntax colouring mechanism for Agda's non-standard highlighting.


<a id="org6e7e25f"></a>

# Agda Syntax Highlighting

We produce a new mode, calling it `ob-agda-mode`,
so that Org-mode blocks marked with `ob-agda` will have Agda *approximated*
syntax. By default, if an Emacs major-mode `<lang>-mode` exists,
then blocks marked with `<lang>` use that major-mode for editing.

    ;; To use generic-mode later below.
    (require 'generic-x)

The “ob” is short for “org-babel” since we also wish to provide
Babel support for Agda. Using `ob-agda` marked blocks is awkward and exposes
some of our implementation, we will instead support an alias `agda` which refers to `ob-agda`.

We can use the `org-src-lang-modes` variable to map any &#x2014;possibly more friendly or suggestive&#x2014; identifier to a language major mode.

    (add-to-list 'org-src-lang-modes '("agda" . ob-agda))


<a id="orgbc189d9"></a>

## Keywords

We look at the `agda2-highlight.el` source file from the Agda repository
for colours of keywords and reserved symbols such as `=, ∀,` etc.

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

From Agda's [“read the docs”](https://agda.readthedocs.io/en/v2.5.4.1/language/lexical-structure.html?highlight=keywords) website, we obtain the keywords for the language:

    (setq org-agda-keywords
      '("=" "|" "->" "→" ":" "?" "\\" "λ" "∀" ".." "..." "abstract" "codata"
      "coinductive" "constructor" "data" "do" "eta-equality" "field"
      "forall" "hiding" "import" "in" "inductive" "infix" "infixl"
      "infixr" "instance" "let" "macro" "module" "mutual" "no-eta-equality"
      "open" "overlap" "pattern" "postulate" "primitive" "private" "public"
      "quote" "quoteContext" "quoteGoal" "quoteTerm" "record" "renaming"
      "rewrite" "Set" "syntax" "tactic" "unquote" "unquoteDecl" "unquoteDef"
      "using" "where" "with"))


<a id="orgd975c1e"></a>

## The `generic-mode` Definition

Agda colouring is approximated as defined below, but a convention is made:
Function symbols begin with a lower case letter, whereas type symbols begin
with a capital letter. Otherwise, I would need to resort to Agda's mechanism
for determining whether a name is a type or not:

<div class="org-center">
*Parsing is Typechecking!*
</div>

    ; (defvar org-agda-extra-word-colours nil "other words that user of org-mode wants coloured, along with their specified font-lock-type-face")

    (define-generic-mode

	'ob-agda-mode                      ;; name of the mode

	(list '("{-" . "-}"))               ;; comments delimiter

	org-agda-keywords

	;; font lock list: Order of colouring matters;
	;; the numbers refer to the subpart, or the whole(0), that should be coloured.

	(-concat  ;; ★★★★★★★★★★★★★★ org-agda-extra-word-colours is a free variable, user should define it /before/ loading org-agda-mode ★★★★★★★★★★★★★★
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

    (provide 'ob-agda-mode)

    ; (describe-symbol 'define-generic-mode)
    ; (describe-symbol 'font-lock-function-name-face)

I do not insist that `org-agda-mode` be activated on any particular files by default.

Here is an example code block that obtains this colouring schema.

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

Next, we turn to supporting Agda interactivity with holes.


<a id="org76a9760"></a>

## User-defined Colouring

Since true Agda colouring requires type-checking, it is desirable to allow the user to
input colouring for their own identifiers. Such <a id="orgbdb5542">user-defined colouring</a> will be
via the delightful org-mode interface: A super simple intuitive table ♥‿♥

Anywhere in their buffer, the user should have a table with a column for identifiers
and the colours they should have, as follows.

    #+RESULTS: ob-agda/colours
    | one   | keyword       |
    | two   | builtin       |
    | three | function-name |
    | four  | variable-name |
    | five  | constant      |

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">one</td>
<td class="org-left">keyword</td>
</tr>


<tr>
<td class="org-left">twoish</td>
<td class="org-left">builtin</td>
</tr>


<tr>
<td class="org-left">three</td>
<td class="org-left">function-name</td>
</tr>


<tr>
<td class="org-left">four</td>
<td class="org-left">variable-name</td>
</tr>


<tr>
<td class="org-left">five</td>
<td class="org-left">constant</td>
</tr>
</tbody>
</table>

Which yields the following colouring,

    one   = Set
    two   = Set
    three = Set
    four  = Set
    five  = Set

(load-file "literate.el")

We implement this as follows. We produce a function that realises such colouring assignments:

    (defun ob-agda/add-colour (word colour)

       "
	Refresh the ob-agda-mode to have the new ‘colour’ for ‘word’ in agda blocks.

	+ ‘word’ is a string representing an Agda identifier.

	+ ‘colour’ is either a symbol from ‘keyword’, ‘builtin’, ‘function-name’,
	   ‘variable-name’, ‘constant’.
       "

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
	       `(,word 0 ,colour))
       )

       ;; Load the new altered scheme.
       (require 'ob-agda-mode "~/org-agda-mode/literate.el")

    )

    (add-to-list 'org-agda-extra-word-colours '("typeclass" 0 'agda2-highlight-keyword-face))
    (add-to-list 'org-agda-extra-word-colours '("PackageFormer" 0 'font-lock-type-face))
    (add-to-list 'org-agda-extra-word-colours '("_⨾_" 0 'font-lock-type-face))
    (add-to-list 'org-agda-extra-word-colours '("assoc" 0 'font-lock-type-face))

    ; (ob-agda/add-colour "newkeyword" 'agda2-highlight-keyword-face)
    (ob-agda/add-colour "newkeyword" 'builtin)
    (ob-agda/add-colour "newkeyword" 'constant)
    (ob-agda/add-colour "newkeyword" 'function-name)

Then lookup that user provided table, if it is there, and use it.

    (defun ob-agda/update-colours ()
     "Searchs current buffer for an ob-agda/colours named result table
      then uses that to update the colour scheme.
     "
     (interactive)
    (ignore-errors
    (save-excursion
      (org-babel-goto-named-result "ob-agda/colours")
      (forward-line)
      ; (setq _it (org-table-to-lisp))
      (dolist (elem (org-table-to-lisp) org-agda-extra-word-colours)
	(ob-agda/add-colour (car elem) (intern (cadr elem))))
    ))
    )

    ob-agda/update-colours


<a id="org9f9594f"></a>

# (`lagda-to-org`) and (`org-to-lagda`)

Previously, Agda would not typecheck a non-`lagda`, or non-`agda`, file therefore
I could not use Org-mode multiple mode settings.

Recent versions of Agda will typecheck files with other extensions,
but as of 2.6.0, the interactive mode does not work on such files.

I will instead merely
swap the syntax of the modes then reload the desired mode.
&#x2013;it may not be ideal, but it does what I want in a fast enough fashion.

In order to maintain position when switching back to Org-mode,
I define a function which not only goes to the appropriate line,
but unfolds the document to show that line.

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

Below we put together a way to make rewrites `⟨pre⟩⋯⟨post⟩ ↦ ⟨newPre⟩⋯⟨newPost⟩`
then use that with the rewrite tokens being `#+BEGIN_SRC` and `|begin{code}` for
literate Agda, as well as their closing partners.

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

The two rewriting utilities:

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

    org-to-lagda

**Notice!** The toggling utilities automatically enable all *safe* local variables
in an file &#x2014;c.f., the `(enable-local-variables :all)` lines above.
Many of our files tend to have local variables and that is the reason
we allow us.

Handy-dandy shortcuts, which are alternated on mode change:

    (add-hook 'org-mode-hook
	      (lambda () (local-set-key (kbd "C-x C-a") 'org-to-lagda)))

    (add-hook 'agda2-mode-hook
	      (lambda () (local-set-key (kbd "C-x C-a") 'lagda-to-org)))


<a id="orgfc76e4a"></a>

# Example

<!--

    (unload-feature 'org-agda-mode)
    (load-file "org-agda-mode.el")

-->

Here's some sample fragments, whose editing can be turned on with `C-x C-a`.

    postulate magic : Set

    hole : magic
    hole = {!!}

Here's a literate Agda `spec`-ification environment, which corresponds to an Org-mode `EXAMPLE` block.

    module this-is-a-spec {A : Set} (_≤_ : A → A → Set) where

      maximum-specfication : (candidate : A) → Set
      maximum-specfication c = ?


<a id="org2c2cd30"></a>

# Summary

We now have the utility functions:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left"><span class="underline">Command</span></td>
<td class="org-left"><span class="underline">Action</span></td>
</tr>


<tr>
<td class="org-left">`C-x C-a`</td>
<td class="org-left">transform org `org-agda` blocks to literate Agda blocs</td>
</tr>


<tr>
<td class="org-left">`C-x C-a`</td>
<td class="org-left">transform literate Agda code delimiters to org `org-agda` src</td>
</tr>
</tbody>
</table>

This was fun: I learned a lot of elisp!
Hopefully I can make use of this, in the small, if not in the large
&#x2013;in which case I'll need to return to the many `COMMENT`-ed out sections
in this document.


<a id="org75a5a73"></a>

# Sources Consulted

-   [How to Write a Emacs Major Mode for Syntax Coloring](http://www.ergoemacs.org/emacs/elisp_syntax_coloring.html)
-   [Simplest Emacs Syntax Highlighting Tutorial](https://stackoverflow.com/questions/3887372/simplest-emacs-syntax-highlighting-tutorial)
-   [“Hello World” for Emacs' Major Mode Creation](https://stackoverflow.com/questions/1063115/a-hello-world-example-for-a-major-mode-in-emacs)
-   [Adding A New Language to Emacs](http://www.wilfred.me.uk/blog/2015/03/19/adding-a-new-language-to-emacs/)
-   [How to Make an Emacs Minor Mode](https://nullprogram.com/blog/2013/02/06/)
-   [Literate Programming: Empower Your Writing with Emacs Org-Mode](https://www.offerzen.com/blog/literate-programming-empower-your-writing-with-emacs-org-mode)
    -   An elegant overview of literate programming, with Org-mode, and the capabilities it offers.
-   [Introduction to Literate Programming](http://howardism.org/Technical/Emacs/literate-programming-tutorial.html)
    -   A nearly *comprehensive* workshop on the fundamentals of literate programming with Org-mode.
