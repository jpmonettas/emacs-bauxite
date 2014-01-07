(require 'web)

(defvar bxt-cn)

(defun unescape-doublequote (str) (replace-regexp-in-string "\\\\\"" "\"" str))
(defun unescape-newline (str) (replace-regexp-in-string "\\\\n" "\n" str))
(defun unescape-quote (str) (replace-regexp-in-string "\\\\'" "'" str))
(defun unescape-creturn (str) (replace-regexp-in-string "\\\\r" "\r" str))
(defun unescape-tab (str) (replace-regexp-in-string "\\\\t" "\t" str))
(defun unescape-/ (str) (replace-regexp-in-string "\\\\/" "/" str))


(defun bxt-find-code (page-html type)
  (with-temp-buffer
       (goto-char (point-min))
       (insert page-html)
       (goto-char (point-min))
       (search-forward (format  "%sCodeAttributes['%sCode'].value = " type type) nil nil 2)
       (set-mark (point))
       (end-of-line)
       (unescape-creturn
        (unescape-doublequote
         (unescape-quote
          (unescape-newline
           (unescape-/
            (unescape-tab
             (buffer-substring-no-properties (incf (mark)) (- (point) 2))))))))))

(defun bxt-create-buffer (name data type)
  (with-current-buffer (get-buffer-create name)
         (goto-char (point-min))
         (insert data)
         (cond ((equal type :script) (groovy-mode))
               ((equal type :style) (css-mode))
               ((equal type :javascript) (js2-mode))
               ((equal type :template) (html-mode)))
         (goto-char (point-min))))

(defun bxt-get-component (cn)
  (setq bxt-cn cn)
  (interactive (list (helm :sources 'helm-source-components)))
  (web-http-call
   "GET"
   (lambda (conn headers data)
     (let ((groovy-buff-name (format "%s-%s" bxt-cn "GROOVY"))
           (style-buff-name (format "%s-%s" bxt-cn "STYLE"))
           (javascript-buff-name (format "%s-%s" bxt-cn "JAVASCRIPT"))
           (template-buff-name (format "%s-%s" bxt-cn "TEMPLATE"))
           (groovy-code (bxt-find-code data "script"))
           (style-code (bxt-find-code data "style"))
           (javascript-code (bxt-find-code data "javascript"))
           (template-code (bxt-find-code data "template")))
       (bxt-create-buffer groovy-buff-name groovy-code :script)
       (bxt-create-buffer style-buff-name style-code :style)
       (bxt-create-buffer javascript-buff-name javascript-code :javascript)
       (bxt-create-buffer template-buff-name template-code :template)
       (switch-to-buffer template-buff-name)))
   :url (format "%s%s" "http://bauxitevm3.internetbrands.com:8080/admin/4.0/editComponent?cn=" cn)
   :extra-headers (list (cons "Cookie" bxt-cookies))))

(defun bxt-switch-to (type)
  (let* ((buff-name (buffer-name))
         (component-name (car (split-string buff-name "-")))
         (dest-buff-name (format "%s-%s" component-name type)))
    (switch-to-buffer dest-buff-name)))


(defun bxt-switch-to-groovy ()
  (interactive)
  (bxt-switch-to "GROOVY"))

(defun bxt-switch-to-js ()
  (interactive)
  (bxt-switch-to "JAVASCRIPT"))

(defun bxt-switch-to-style ()
  (interactive)
  (bxt-switch-to "STYLE"))

(defun bxt-switch-to-template ()
  (interactive)
  (bxt-switch-to "TEMPLATE"))

(define-minor-mode bxt-mode
  "Bauxite mode"
  ;; The initial value - Set to 1 to enable by default
  nil
  ;; The indicator for the mode line.
  " BXT"
  ;; The minor mode keymap
  `((,(kbd "C-c j") . bxt-switch-to-js)
    (,(kbd "C-c g") . bxt-switch-to-groovy)
    (,(kbd "C-c s") . bxt-switch-to-style)
    (,(kbd "C-c t") . bxt-switch-to-template)))



(setq helm-source-components
  '((name . "Bauxite Components")
    (candidates . component-candidates)
    (pattern-transformer . (lambda (pattern) (regexp-quote pattern)))
    (action . identity)))
