(defvar odin-mode-hook nil)

(defun odin-align-const (start end)
  "Align blocks of const declarations in Odin"
  (interactive "r")
  (replace-regexp-in-region "[\s\t]+" " " start end)
  (align-regexp start end "\\(\\s-*\\)::" 1 1))

(defun odin-mode--reload ()
  "Helper to reload Odin major mode for quicker development iteration."
  (interactive)
  (unload-feature 'odin-mode t)
  (eval-buffer))

(defconst odin--odin-keywords
  '("bit_set" "case" "context" "defer" "distinct" "dynamic"
    "else" "enum" "fallthrough" "for" "foreign" "if"
    "import" "in" "map" "matrix" "or_else" "or_return"
    "package" "proc" "return" "struct" "switch" "union"
    "using" "when" "where"))

(defconst odin--odin-builtins
  '("---" "false" "true" "nil"))

(defconst odin--odin-builtin-types
  '("bool" "b8" "b16" "b32" "b64"
    "int" "i8" "i16" "i32" "i64" "i128"
    "uint" "u8" "u16" "u32" "u64" "u128" "uintptr"
    "i16le" "i32le" "i64le" "i128le"
    "u16le" "u32le" "u64le" "u128le"
    "i16be" "i32be" "i64be" "i128be"
    "u16be" "u32be" "u64be" "u128be"
    "f16" "f32" "f64"
    "f16le" "f32le" "f64le"
    "f16be" "f32be" "f64be"
    "complex32" "complex64" "complex128"
    "quaternion64" "quaternion128" "quaternion256"
    "rune" "string" "cstring"
    "rawptr" "typeid" "any"))

(defun odin--wrap-word (pattern)
  "Helper to wrap pattern with word boundaries"
  (concat "\\<" pattern "\\>"))

(defconst odin--font-lock-directives
  '("#[a-zA-Z_]*" . font-lock-preprocessor-face)
  "Highlighting expressions for Odin directives")

(defconst odin--font-lock-keywords
  `(,(odin--wrap-word (regexp-opt odin--odin-keywords))
    . font-lock-keyword-face)
  "Highlighting expressions for Odin keywords")

(defconst odin--font-lock-builtins
  `(,(regexp-opt odin--odin-builtins) . font-lock-builtin-face)
  "Highlighting expressions for Odin builtins")

(defconst odin--font-lock-functions
  '("\\<\\([a-zA-Z0-9_]*\\.\\)?\\([a-zA-Z0-9_]*\\)\\(?:(\\)"
    . (2 font-lock-function-name-face))
  "Highlighting expressions for Odin functions")

(defconst odin--font-lock-types-builtin
  `(,(odin--wrap-word (regexp-opt odin--odin-builtin-types))
    . font-lock-type-face)
  "Highlighting expressions for Odin types")

(defconst odin-font-lock-keywords
  (list odin--font-lock-directives
	odin--font-lock-keywords
	odin--font-lock-builtins
        odin--font-lock-functions
        odin--font-lock-types-builtin)
  "Minimal highlighting expressions for Odin mode")

(defvar odin-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w"      st) ;; underscore is part of a word
    (modify-syntax-entry ?/ ". 124b" st) ;; comments start with 1-2 "/" and might end with one
    (modify-syntax-entry ?* ". 23"   st) ;; comments have "*" as second char on start and first on end
    (modify-syntax-entry ?\n "> b"   st) ;; newline can end a comment
    st)
  "Syntax table for odin-mode")

(defun odin-indent-line ()
  "Indent current line as Odin code"
  (interactive)
  (beginning-of-line)
  (let ((start-of-multiline-block "^.*{\s*\\(?://.*\\)?$")
		(end-of-multiline-block "^[\s\t]*}")
		(end-of-function-call "^[\s\t]*)"))
	(if (bobp)
		(indent-line-to 0)
	  (let ((not-indented t)
			cur-indent)
		(cond ((looking-at "^[\s\t]*}") ;; closing a block
			   (progn
				 (save-excursion
				   (forward-line -1)
				   (setq cur-indent (- (current-indentation) tab-width)))
				 (if (< cur-indent 0)
					 (setq cur-indent 0))))
			  ((looking-at "^[\s\t]*case.*:.*") ;; open case block
			   (progn
				 (let ((inner-level 0))
				   (save-excursion
					 (while not-indented
					   (forward-line -1)
					   (cond ((looking-at ".*switch.*{")
							  (if (= inner-level 0)
								  (progn
									(setq cur-indent (current-indentation))
									(setq not-indented nil))
								(setq inner-level (- inner-level 1))))
							 ((looking-at start-of-multiline-block)
							  (setq inner-level (- inner-level 1)))
							 ((looking-at end-of-multiline-block)
							  ;; TODO: this doesn't allow anon blocks
							  (setq inner-level (+ inner-level 1)))))))))
			  ((looking-at end-of-function-call)
			   (progn
				 (save-excursion
				   (while not-indented
					 (forward-line -1)
					 (if (looking-at ".*($") ;; beginning of function call
						 (progn
						   (setq cur-indent (current-indentation))
						   (setq not-indented nil)))))))
			  (t (save-excursion ;; everything else
				   (while not-indented
					 (forward-line -1)
					 (cond ((looking-at start-of-multiline-block)
							(progn
							  (setq cur-indent
									(+ (current-indentation) tab-width))
							  (setq not-indented nil)))
						   ((looking-at end-of-multiline-block)
							(progn
							  (setq cur-indent (current-indentation))
							  (setq not-indented nil)))
						   ((looking-at end-of-function-call)
							(progn
							  (setq cur-indent (current-indentation))
							  (setq not-indented nil)))
						   ((looking-at ".*($") ;; beginning of function call
							(progn
							  (setq cur-indent
									(+ (current-indentation) tab-width))
							  (setq not-indented nil)))
						   ((looking-at "^[\s\t]*case.*:$") ;; case block
							(progn
							  (setq cur-indent
									(+ (current-indentation) tab-width))
							  (setq not-indented nil)))
						   ((bobp)
							(progn
							  (setq cur-indent 0)
							  (setq not-indented nil))))))))
		(if cur-indent
			(indent-line-to cur-indent)
		  (indent-line-to 0))))))

(defvar odin-mode-map
  (let ((map (make-keymap)))
	(define-key map "\C-c a" 'odin-align-const)
    map)
  "Keymap for Odin major mode")

;;;###autoload
(defun odin-mode ()
  "Major mode for editing Odin files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table odin-mode-syntax-table)
  (use-local-map odin-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(odin-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'odin-indent-line)
  (setq major-mode 'odin-mode)
  (setq mode-name "Odin")
  (run-hooks 'odin-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-mode))

(provide 'odin-mode)
