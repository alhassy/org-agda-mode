<h1> org-agda-mode </h1>

An Emacs mode for working with 
Agda code in an Org-mode like fashion, more or less.

The following can also be read as a [blog post](https://alhassy.github.io/literate/).


# Table of Contents

1.  [Agda Syntax Highlighting With `org-agda-mode`](#org803bf1b)
    1.  [Keywords](#org657fa57)
    2.  [The `generic-mode` Definition](#orgc1ae081)
2.  [(`lagda-to-org)` and (`org-to-lagda)`](#orgdb77653)
3.  [Example](#org1817317)
4.  [Summary](#orgd9a81e7)
5.  [Sources Consulted](#org701dea8)

<div class="org-center">
**Abstract**

[Literate Programming](https://en.wikipedia.org/wiki/Literate_programming) is essentially the idea that code is enclosed in documentation
rather than the comments being surrounded by code. The idea is that software
ought to be written like an essay to be read by a human; from this code for the
machine can be extracted.

The articles on this blog are meant to be in such a format and as such
I use [Org-mode](https://www.offerzen.com/blog/literate-programming-empower-your-writing-with-emacs-org-mode) as my markup for producing the HTMLs and PDFs.

This article aims to produce an Org-friendly approach to working
with the [Agda language](http://wiki.portal.chalmers.se/agda/pmwiki.php), which is special in comparison to many other languages:
Coding is interactive via holes and it permits almost any sequence of characters
as a legal lexeme thereby rendering a static highlighting theme impossible.

The result of this Elisp exploration is that by `C-x C-a`
we can shift into Agda-mode and use its interactive features to construct our program;
then return to an Org-mode literate programming style afterwards with `C-x C-o`
---*both translations remember the position we're working at!*
</div>

<!--

    ;; This file is generated from literate.lagda.

    ;; This file is generated from literate.lagda.

-->


<a id="org803bf1b"></a>

# Agda Syntax Highlighting With `org-agda-mode`

We produce a new mode in a file named `org-agda-mode.el`
so that Org-mode blocks marked with `org-agda` will have Agda *approximated*
syntax.

    ;; To use generic-mode later below.
    (require 'generic-x)


<a id="org657fa57"></a>

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

    (setq org-agda-keywords '("=" "|" "->" "→" ":" "?" "\\" "λ" "∀" ".." "..." "abstract" "codata"
                              "coinductive" "constructor" "data" "do" "eta-equality" "field"
                              "forall" "hiding" "import" "in" "inductive" "infix" "infixl"
                              "infixr" "instance" "let" "macro" "module" "mutual" "no-eta-equality"
                              "open" "overlap" "pattern" "postulate" "primitive" "private" "public"
                              "quote" "quoteContext" "quoteGoal" "quoteTerm" "record" "renaming"
                              "rewrite" "Set" "syntax" "tactic" "unquote" "unquoteDecl" "unquoteDef"
                              "using" "where" "with"))


<a id="orgc1ae081"></a>

## The `generic-mode` Definition

Agda colouring is approximated as defined below, but a convention is made:
Function symbols begin with a lower case letter, whereas type symbols begin
with a capital letter. Otherwise, I would need to resort to Agda's mechanism
for determining whether a name is a type or not:

<div class="org-center">
*Parsing is Typechecking!*
</div>

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

I do not insist that `org-agda-mode` be activated on any particular files by default.


<a id="orgdb77653"></a>

# (`lagda-to-org)` and (`org-to-lagda)`

Agda would not typecheck a non-`lagda`, or non-`agda`, file therefore
I cannot use Org-mode multiple mode settings.
I will instead merely
swap the syntax of the modes then reload the desired mode.
&#x2013;It may not be ideal, but it does what I want in a fast enough fashion.

Below we put together a way to make rewrites `⟨pre⟩⋯⟨post⟩ ↦ ⟨newPre⟩⋯⟨newPost⟩`
then use that with the rewrite tokens being `#+BEGIN_SRC` and `|begin{code}` for
literate Agda, as well as their closing partners.

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

The two rewriting utilities:

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

Handy-dandy shortcuts:

    (local-set-key (kbd "C-x C-a") 'org-to-lagda)
    (local-set-key (kbd "C-x C-o") 'lagda-to-org)
    
    ;; Maybe consider a simple “toggle” instead?


<a id="org1817317"></a>

# Example

<!--

    (unload-feature 'org-agda-mode)
    (load-file "org-agda-mode.el")

-->

Here's some sample fragments, whose editing can be turned on with `C-x C-a`.

\begin{code}
mmodule literate where

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

Here's a literate Agda `spec`-ification environment, which corresponds to an Org-mode `EXAMPLE` block.

\begin{spec}
module this-is-a-spec {A : Set} (_≤_ : A → A → Set) where

  maximum-specfication : (candidate : A) → Set
  maximum-specfication c = ?
\end{spec}


<a id="orgd9a81e7"></a>

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
<td class="org-left">`C-x C-o`</td>
<td class="org-left">transform literate Agda code delimiters to org `org-agda` src</td>
</tr>
</tbody>
</table>

This was fun: I learned a lot of elisp!
Hopefully I can make use of this, in the small, if not in the large
&#x2013;in which case I'll need to return to the many `COMMENT`-ed out sections
in this document.


<a id="org701dea8"></a>

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

