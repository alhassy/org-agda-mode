(provide 'org-agda-mode)

(require 'polymode)

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
  :innermodes '(poly-org-agda-innermode))

(add-to-list 'auto-mode-alist '("\\.lagda.org" . org-agda-mode))

(assq-delete-all 'background agda2-highlight-faces)
