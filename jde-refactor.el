(defun jde-refactor-rename (to-name &optional query)
  "Rename the thing at point to TO-NAME. If query (the prefix
argument) is non-NIL, query each renaming."
  ;; TODO: Handle methods (at least in-class uses)
  ;; TODO: Deal with name conflicts
  ;; TODO-MAYBE: Deal with non-private members by renaming all users
  (interactive
   (progn (barf-if-buffer-read-only)
          (let ((thing-of-interest (thing-at-point 'symbol)))
            (list (read-string (concat "Rename " thing-of-interest " to: "))
                  current-prefix-arg))))
  (save-excursion
    (let* ((thing-of-interest (thing-at-point 'symbol))
           (pair (progn (end-of-thing 'symbol)
                        (jde-parse-java-variable-at-point))))
      (let ((declaration-pos
             (and (string= (car pair) "")
                  (jde-parse-find-declaration-of thing-of-interest))))
        (when declaration-pos
          (let ((boundaries
                 (if (jde-refactor-parameter-of-current-method-p thing-of-interest)
                     (cons (semantic-tag-start (semantic-current-tag))
                           (semantic-tag-end (semantic-current-tag)))
                   (jde-refactor-block-boundaries))))
            (funcall (if query 'query-replace 'replace-string)
                     thing-of-interest to-name
                     t declaration-pos (cdr boundaries))))))))

(defun jde-refactor-parameter-of-current-method-p (thing)
  (find thing (semantic-tag-function-arguments (semantic-current-tag-of-class 'function))
        :key #'car :test #'string=))

(defun jde-refactor-block-boundaries ()
  "Return a pair (START . END) denoting the boundaries of the
block (currently really method) containing point."
  (save-excursion
    (while (not (semantic-up-context)))
    (cons (point)
          (progn (forward-sexp 1) (point)))))
