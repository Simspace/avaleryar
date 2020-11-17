(defvar soutei-mode-hook nil)

(defface soutei-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Soutei keywords")

(defface soutei-operator-face
  '((t (:inherit font-lock-builtin-face)))
  "Soutei builtin operators")

(defvar soutei-indent-width 2
  "indentation width used by auto-indent functions")

(defvar soutei-mode-map (make-keymap)
  "Keymap for soutei major mode")

(defvar soutei-mode-syntax-table
  (let ((syn-table (make-syntax-table)))
    (modify-syntax-entry ?- "_" syn-table)
    (modify-syntax-entry ?\; "<" syn-table)
    (modify-syntax-entry ?\n ">" syn-table)
    (modify-syntax-entry ?% "_" syn-table) ;; comment in prolog; override here
    syn-table)
  "Syntax table for `soutei-mode'"
  )

(defconst soutei-font-lock-keywords
  '(
    (";.*" . 'font-lock-comment-face)
    ("says" . 'soutei-keyword-face)
    (":-" . 'soutei-operator-face)
    ("," . 'soutei-operator-face)
    ("\\." . 'soutei-operator-face)
    ("#[tf]" . 'font-lock-constant-face)
    ("\\?[a-zA-Z!@$%&*/<=>~_^][a-zA-Z0-9!@$%&*/<=>~_^?+-]*" . 'font-lock-variable-name-face)
    ))

(define-derived-mode soutei-mode prolog-mode "Soutei"
  "Major mode for editing Soutei files"
  :syntax-table soutei-mode-syntax-table
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) ";+ *")
  (set (make-local-variable 'prolog-indent-width) soutei-indent-width)
  (set 'font-lock-defaults '(soutei-font-lock-keywords)))

(add-to-list 'auto-mode-alist '("\\.ava\\'" . soutei-mode))


(provide 'soutei-mode)
