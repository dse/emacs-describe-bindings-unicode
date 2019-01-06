;;; describe-bindings-unicode --- add extra Unicode information.

;;; Commentary:

;;; Code:

(defgroup describe-bindings-unicode nil
  "Add extra Unicode information when describing key bindings."
  :group 'mule)

(defcustom describe-bindings-unicode-function
  nil
  "Function name to provide extra Unicode info for `describe-bindings'.

Named function should take a character code as an argument, and
return a string.

See definition of the `describe-bindings-unicode-default-function'
function for an example."
  :group 'describe-bindings-unicode
  :type '(choice (const :tags "Use default function" nil)
                 (symbol :tags "Function name"))
  :safe #'(lambda (x) (or (null x)
                          (stringp x)
                          (symbolp x))))

(defun describe-bindings-unicode-default-function (char)
  "Default function to generate the string for `describe-bindings-unicode'.

Overriden using the `describe-bindings-unicode-function' variable,
which is not to be confused with the function of the same name.

CHAR is a character code."
  (concat "              ("
          (format "U+%04X " char)
          (or (get-char-code-property char 'name) "[no name]")
          ")"))

(defun describe-bindings-unicode-function (char)
  "Generate the string inserted for `describe-bindings-unicode'.

CHAR is either a character code or a string from which the
first character is used."
  (let ((char (cond ((stringp char) (string-to-char char))
                    (t char)))
        (fun (or (and describe-bindings-unicode-function
                      (cond ((symbolp describe-bindings-unicode-function)
                             (symbol-function describe-bindings-unicode-function))
                            ((stringp describe-bindings-unicode-function)
                             (symbol-function (make-symbol describe-bindings-unicode-function)))))
                 #'describe-bindings-unicode-default-function)))
    (funcall fun char)))

(defun describe-bindings-unicode-advice (&optional prefix buffer)
  "`describe-bindings' advice function for adding Unicode info.

PREFIX and BUFFER arguments are the same as in the
`describe-bindings' function."
  (interactive)
  (with-current-buffer (help-buffer)
    (goto-char (point-min))
    ;; C-x 8 C-h => Key translations Starting With C-x 8:
    ;; C-x 8 " C-h => Key translations Starting With C-x 8 ":
    (if (looking-at-p "Key translations Starting With C-x 8")
        (let ((inhibit-read-only t))
          (while (re-search-forward "[^\n\t\f -~]" nil 'noerror)
            (let ((char (string-to-char (match-string 0))))
              (insert (describe-bindings-unicode-function char))))))))

(defun enable-describe-bindings-unicode ()
  "Enable extra Unicode info when doing `describe-bindings'."
  (interactive)
  (advice-add 'describe-bindings :after #'describe-bindings-unicode-advice))

(defun disable-describe-bindings-unicode ()
  "Disable extra Unicode info when doing `describe-bindings'."
  (interactive)
  (advice-add 'describe-bindings :after #'describe-bindings-unicode-advice))

(enable-describe-bindings-unicode)

(provide 'describe-bindings-unicode)
;;; describe-bindings-unicode.el ends here
