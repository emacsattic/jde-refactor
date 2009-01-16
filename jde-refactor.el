;;; jde-refactor.el --- Refactoring support for JDEE
;;
;; Copyright (c) 2008 Espen Wiborg <espenhw@grumblesmurf.org>
;;
;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.
;;

(defgroup jde-refactor nil
  "JDEE refactorings"
  :group 'jde
  :prefix "jde-refactor-")

(defcustom jde-refactor-rename-query-p nil
  "If non-NIL, `jde-refactor-rename' always uses `query-replace'
to do the replacement."
  :type 'boolean
  :group 'jde-refactor)

(defcustom jde-refactor-extract-constant-query-p nil
  "If non-NIL, `jde-refactor-extract-constant' uses
`query-replace' to do the replacement."
  :type 'boolean
  :group 'jde-refactor)

(defvar jde-refactor-java-scopes '((public "Public" "public ")
                                   (protected "Protected" "protected ")
                                   (package "Package private" "")
                                   (private "private" "private "))
  "Java scopes with descriptive strings.")

(defcustom jde-refactor-extract-constant-default-scope 'public
  "The default scope of the constant
`jde-refactor-extract-constant' extracts."
  :type `(choice ,@(mapcar #'(lambda (c)
                               `(const :tag ,(nth 1 c) ,(car c)))
                           jde-refactor-java-scopes))
  :group 'jde-refactor)

(defun jde-refactor-replace-function (query-p)
  (if query-p 'query-replace 'replace-string))

(defun jde-refactor-replace-regexp-function (query-p)
  (if query-p 'query-replace-regexp 'jde-refactor-replace-regexp))

(defun jde-refactor-replace-regexp (regexp to-string)
  (while (re-search-forward regexp nil t)
    (replace-match to-string nil nil)))

(defun jde-refactor-rename (to-name &optional query)
  "Rename the thing at point to TO-NAME. If query (the prefix
argument) is non-NIL, query each renaming.  Cannot cope with
variables declared in a Java 5 for-each loop (at least with the
standard JDEE, eh).  Doesn't deal very well with the normal
setter idiom of parameters shadowing member variables, either."
  ;; TODO: Deal with name conflicts
  ;; TODO-MAYBE: Class renaming
  ;; TODO-MAYBE: Deal with non-private members by renaming all external uses
  (interactive
   (progn (barf-if-buffer-read-only)
          (let ((thing-of-interest (thing-at-point 'symbol)))
            (list (read-string (concat "Rename " thing-of-interest " to: "))
                  current-prefix-arg))))
  (jde-refactor--rename-internal (jde-refactor-replace-function (or jde-refactor-rename-query-p
                                                                   query))
                                to-name))

(defun jde-refactor--rename-internal (rename-function to-name)
  (save-excursion
    (let* ((thing-of-interest (thing-at-point 'symbol))
           (pair (progn (end-of-thing 'symbol)
                        (jde-parse-java-variable-at-point))))
      (let ((boundaries
             (cond ((and (string= (car pair) "")
                         (jde-parse-find-declaration-of thing-of-interest))
                    (let ((function-tag (semantic-current-tag-of-class
                                         'function)))
                      (if (jde-refactor-parameter-of-method-p thing-of-interest
                                                              function-tag)
                          (cons (semantic-tag-start function-tag)
                                (semantic-tag-end function-tag))
                        (cons (jde-parse-find-declaration-of thing-of-interest)
                              (cdr (jde-refactor-block-boundaries))))))
                   ((jde-refactor-method-in-class-p
                     thing-of-interest (semantic-current-tag-of-class 'type))
                    ;; TODO-MAYBE: handle callers/overriders in other classes
                    ;; TODO-MAYBE: handle superclass/interface declarations
                    (let ((class-tag (semantic-current-tag-of-class 'type)))
                      (cons (semantic-tag-start class-tag)
                            (semantic-tag-end class-tag)))))))
        (funcall rename-function
                 thing-of-interest to-name
                 t (car boundaries) (cdr boundaries))))))

(defun jde-refactor-method-in-class-p (name class-tag)
  (semantic-find-tags-by-name name (semantic-tag-type-members class-tag)))

(defun jde-refactor-parameter-of-method-p (thing function-tag)
  (find thing (semantic-tag-function-arguments function-tag)
        :key #'semantic-tag-name :test #'string=))

(defun jde-refactor-block-boundaries ()
  "Return a pair (START . END) denoting the boundaries of the
block (currently really method) containing point."
  (save-excursion
    (while (not (semantic-up-context)))
    (cons (point)
          (progn (forward-sexp 1) (point)))))

(defun jde-refactor-read-scope (prompt &optional default-value)
  (intern
   (completing-read (concat prompt " (default " default-value "): ")
                    (mapcar #'(lambda (c) (symbol-name (car c)))
                            jde-refactor-java-scopes)
                    nil t nil nil default-value)))

(defun jde-refactor-extract-constant (name &optional scope value-spec)
  (interactive
   (let ((valspec (jde-refactor-constant-expression-at-point)))
     (progn (barf-if-buffer-read-only)
            (list (read-string (format "Make %s a constant named: "
                                       (car valspec)))
                  (and current-prefix-arg
                       (jde-refactor-read-scope "Constant scope"
                                                jde-refactor-extract-constant-default-scope))
                  valspec))))
  (unless scope
    (setq scope jde-refactor-extract-constant-default-scope))
  (unless value-spec
    (setq value-spec (jde-refactor-constant-expression-at-point)))
  (let ((constant-spec (format "%sstatic final %s %s = %s;\n"
                               (nth 2 (assoc scope jde-refactor-java-scopes))
                               (cdr value-spec)
                               name
                               (car value-spec))))
    (save-excursion
      (undo-boundary)
      (goto-char (point-min))
      (funcall (jde-refactor-replace-regexp-function jde-refactor-extract-constant-query-p)
               (let ((expr-delimiter "\\([\s(),=+;]\\)"))
                 (concat expr-delimiter (regexp-quote (car value-spec)) expr-delimiter))
               (concat "\\1" name "\\2"))
      (let ((first-member-tag
             (car (semantic-tag-type-members
                   (car (semantic-find-nonterminal-by-token
                         'type (semantic-fetch-tags)))))))
        (goto-char (semantic-tag-start first-member-tag))
        (forward-line 0)
        (insert constant-spec)
        (forward-line -1)
        (indent-according-to-mode))
      (undo-boundary))))

(defun jde-refactor-constant-expression-at-point ()
  (save-excursion
    (when (in-string-p)
      (skip-syntax-forward "^\""))
    (let* ((thing-type (if (or (in-string-p)
                               (eq (char-syntax (char-before)) ?\")
                               (eq (char-syntax (char-after)) ?\"))
                           'sexp
                         'symbol))
           (thing (thing-at-point thing-type))
           (type (jde-refactor-type-of thing))
           (thing-start (car (bounds-of-thing-at-point thing-type)))
           (real-thing (if (char-equal (char-before thing-start) ?-)
                           (concat "-" thing)
                         thing)))
      (if (jde-refactor-constant-p type)
          (cons real-thing type)
        (error "Not a constant expression: %s" thing)))))
  
(defmacro regexp-case (expr &rest clauses)
  "Evaluate EXPR (which must evaluate to a string) and choose
among clauses on that value. Each clause looks like (REGEXP
BODY-FORM).  EXPR is evaluated and attempted matched against each
REGEXP in turn, using `string-match'; the BODY-FORM corresponding
to the first match is evaluated.  If no clause succeeds,
`regexp-case' returns NIL.  A REGEXP of t is allowed in the final
clause, and matches if no other clauses match.
\n(fn EXPR (REGEXP BODY-FORM)...)"
  (let ((var (make-symbol "-regexp-case-")))
    `(let ((,var ,expr))
       (cond ,@(mapcar (lambda (c)
                         (if (eq (car c) t)
                             `(t ,(nth 1 c))
                           `((string-match ,(car c) ,var) ,(nth 1 c))))
                       clauses)))))

(put 'regexp-case 'lisp-indent-function 1)

(defun jde-refactor-type-of (thing)
  (regexp-case thing
    ("^[0-9]+$" 'int)
    ("^[0-9]+[lL]$" 'long)
    ("^[0-9]*.[0-9]+$" 'double)
    ("^[0-9]*.?[0-9]*[fF]$" 'float)
    ("^'.*'$" 'char)
    ("^\".*\"$" 'String)
    (t 'Object)))

(defvar jde-refactor-constant-types '(int long double float char String))

(defun jde-refactor-constant-p (type)
  (memq type jde-refactor-constant-types))

(defun jde-refactor-rename-inline ()
  "Performs an inline rename of the thing at point."
  (interactive "*")
  (jde-refactor--rename-internal 'jde-refactor--replace-linked nil))

(defun jde-refactor--replace-linked (thing to-name ignored start end)
  (jde-refactor--setup-overlays thing start end))

(defun jde-refactor--make-overlay-keymap (thing)
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\r" (jde-refactor--replace-linked-accept thing))
    keymap))

(defun jde-refactor--replace-linked-accept (thing)
  (lexical-let ((thing thing))
    (lambda ()
      (interactive)
      (jde-refactor--remove-overlays thing))))

(defun jde-refactor--remove-overlays (thing)
  (remove-overlays (point-min) (point-max)
                   'jde-refactor-rename-thing thing))

(defun jde-refactor--synchronize-overlays (thing start end)
  (lexical-let ((thing thing)
                (start start)
                (end end))
    (lambda (overlay afterp change-start change-end &optional length)
      (when (and afterp (not undo-in-progress))
        (save-excursion 
          (let ((new-value (buffer-substring (overlay-start overlay)
                                             (overlay-end overlay)))
                (inhibit-modification-hooks t))
            (mapc
             (lambda (o)
               (update-overlay-content o new-value))
             (remove overlay
                     (find-overlays
                      start end
                      'jde-refactor-rename-thing thing)))))))))

(defun update-overlay-content (o new-value)
  (let* ((o-start (overlay-start o))
         (o-end (overlay-end o))
         (o-length (- o-end o-start)))
    (goto-char o-start)
    (message "%s - %s" (buffer-substring-no-properties o-start o-end)
             new-value)
    (insert new-value)
    (when (eq o-length 0)
      (move-overlay o o-start (point)))
    (delete-char o-length)))

(defun find-overlays (start end property value)
  (overlay-recenter end)
  (let (overlays)
    (dolist (o (overlays-in start end))
      (when (eq (overlay-get o property) value)
        (push o overlays)))
    (nreverse overlays)))

(defun jde-refactor--setup-overlays (thing start end)
  (save-excursion
    (goto-char start)
    (while (re-search-forward (concat "\\b" thing "\\b") end t)
      (let ((overlay (make-overlay (match-beginning 0) (match-end 0)
                                   nil nil t))
            (overlay-keymap (jde-refactor--make-overlay-keymap thing))
            (hooks
             (list (jde-refactor--synchronize-overlays thing start end))))
        (overlay-put overlay 'face 'match)
        (overlay-put overlay 'jde-refactor-rename-thing thing)
        (overlay-put overlay 'modification-hooks hooks)
        (overlay-put overlay 'insert-in-front-hooks hooks)
        (overlay-put overlay 'insert-behind-hooks hooks)
        (overlay-put overlay 'keymap overlay-keymap)))))
  
(provide 'jde-refactor)
