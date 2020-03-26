;;; org-agda-mode.el --- Major mode for working with literate org agda files
;;; -*- lexical-binding: t

;;; Commentary:

;; A Major mode for editing Agda code embedded in org files (.lagda.org files).
;; See Agda manual for more information:
;; https://agda.readthedocs.io/en/v2.6.1/tools/literate-programming.html#literate-org

;;; Code:

(require 'polymode)
(require 'agda2-mode)

(defgroup org-agda-mode nil
  "org-agda-mode customisations"
  :group 'languages)

(defcustom use-agda-input t
  "Whether to use Agda input mode in non agda parts of the file."
  :group 'org-agda-mode
  :type 'boolean)

(define-hostmode poly-org-agda-hostmode
  :mode 'org-mode
  :keep-in-mode 'host)

(define-innermode poly-org-agda-innermode
  :mode 'agda2-mode
  :head-matcher "#\\+begin_src agda2"
  :tail-matcher "#\\+end_src"
  :head-mode 'org-mode
  :tail-mode 'org-mode
  ;; Disable font-lock-mode, which interferes with Agda annotations,
  ;; and undo the change to indent-line-function Polymode makes.
  :init-functions '((lambda (_) (font-lock-mode 0))
                    (lambda (_) (setq indent-line-function #'indent-relative))))

(define-polymode org-agda-mode
  :hostmode 'poly-org-agda-hostmode
  :innermodes '(poly-org-agda-innermode)
  (when use-agda-input (set-input-method "Agda")))

(assq-delete-all 'background agda2-highlight-faces)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lagda.org" . org-agda-mode))

(provide 'org-agda-mode)
;;; org-agda-mode ends here
