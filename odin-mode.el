(defvar odin-mode-hook nil)

(defvar odin-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Odin major mode")

(defun odin-mode--build-regexp (strings)
  "Helper to build regexps used for odin-font-lock-keywords"
  (insert (string-replace "\\" "\\\\"
			  (concat "\\<"
				  (regexp-opt strings t)
				  "\\>"))))

(defun odin-mode--reload ()
  "Helper to reload Odin major mode for quicker development iteration."
  (interactive)
  (unload-feature 'odin-mode)
  (eval-buffer))

(defconst odin--font-lock-comment
  (list '("\\(//.*\\)" . font-lock-comment-face)
	'("\\(\\/\\*\\(.\\|\n\\)*\\*\\/\\)" . font-lock-comment-face))
  "Highlighting expressions for Odin comments")

(defconst odin--font-lock-directives
  (list '("#[a-zA-Z_]*" . font-lock-preprocessor-face))
  "Highlighting expressions for Odin directives")

(defconst odin--font-lock-keywords
  (list
   ;; (odin-mode--build-regexp '("package" "import" "struct" "enum" "proc" "if" "return" "for" "in" "switch" "case" "dynamic" "defer" "when" "else" "fallthrough" "distinct" "context" "using" "union" "map" "matrix" "bit_set" "or_else" "or_return" "foreign" "where"))
   '("\\<\\(bit_set\\|c\\(?:ase\\|ontext\\)\\|d\\(?:efer\\|istinct\\|ynamic\\)\\|e\\(?:lse\\|num\\)\\|f\\(?:allthrough\\|or\\(?:eign\\)?\\)\\|i\\(?:mport\\|[fn]\\)\\|ma\\(?:p\\|trix\\)\\|or_\\(?:else\\|return\\)\\|p\\(?:ackage\\|roc\\)\\|return\\|s\\(?:truct\\|witch\\)\\|u\\(?:nion\\|sing\\)\\|whe\\(?:n\\|re\\)\\)\\>" . font-lock-keyword-face))
  "Highlighting expressions for Odin keywords")

(defconst odin--font-lock-builtins
  (list
   ;; (odin-mode--build-regexp '("false" "true" "nil" "---"))
   '("\\<\\(---\\|false\\|nil\\|true\\)" . font-lock-builtin-face))
  "Highlighting expressions for Odin builtins")

(defconst odin--font-lock-functions
  (list '("\\<\\([a-zA-Z0-9_]*\\.\\)?\\([a-zA-Z0-9_]*\\)\\(?:(\\)" . (2 font-lock-function-name-face)))
  "Highlighting expressions for Odin functions")

(defconst odin--font-lock-types-builtin
  (list
     ;; (odin-mode--build-regexp '("bool" "b8" "b16" "b32" "b64" "int" "i8" "i16" "i32" "i64" "i128" "uint" "u8" "u16" "u32" "u64" "u128" "uintptr" "i16le" "i32le" "i64le" "i128le" "u16le" "u32le" "u64le" "u128le" "i16be" "i32be" "i64be" "i128be" "u16be" "u32be" "u64be" "u128be" "f16" "f32" "f64" "f16le" "f32le" "f64le" "f16be" "f32be" "f64be" "complex32" "complex64" "complex128" "quaternion64" "quaternion128" "quaternion256" "rune" "string" "cstring" "rawptr" "typeid" "any"))
   '("\\<\\(any\\|b\\(?:16\\|32\\|64\\|8\\|ool\\)\\|c\\(?:omplex\\(?:128\\|32\\|64\\)\\|string\\)\\|f\\(?:16\\(?:[bl]e\\)?\\|32\\(?:[bl]e\\)?\\|64\\(?:[bl]e\\)?\\)\\|i\\(?:1\\(?:28\\(?:[bl]e\\)?\\|6\\(?:[bl]e\\)?\\)\\|32\\(?:[bl]e\\)?\\|64\\(?:[bl]e\\)?\\|8\\|nt\\)\\|quaternion\\(?:128\\|256\\|64\\)\\|r\\(?:awptr\\|une\\)\\|string\\|typeid\\|u\\(?:1\\(?:28\\(?:[bl]e\\)?\\|6\\(?:[bl]e\\)?\\)\\|32\\(?:[bl]e\\)?\\|64\\(?:[bl]e\\)?\\|8\\|int\\(?:ptr\\)?\\)\\)\\>" . font-lock-type-face))
  "Highlighting expressions for Odin types")

(defconst odin-font-lock-keywords
  (append odin--font-lock-comment
	  odin--font-lock-directives
	  odin--font-lock-keywords
          odin--font-lock-functions
          odin--font-lock-types-builtin)
  "Minimal highlighting expressions for Odin mode")

(defvar odin-mode-syntax-table
  (let ((st (make-syntax-table)))
    st)
  "Syntax table for odin-mode")

(defun odin-mode ()
  "Major mode for editing Odin files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table odin-mode-syntax-table)
  (use-local-map odin-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(odin-font-lock-keywords))
  ;; (set (make-local-variable 'indent-line-function) 'odin-indent-line)
  (setq major-mode 'odin-mode)
  (setq mode-name "Odin")
  (run-hooks 'odin-mode-hook))

(add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-mode))
(provide 'odin-mode)
