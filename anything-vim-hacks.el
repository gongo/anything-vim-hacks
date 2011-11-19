;;------------------------------
;; Define constant variable
;;------------------------------
(defconst vim-hacks:buffer-name "*vim-hacks*"
  "Buffer name used to `vim-hacks:http-get'")

(defconst vim-hacks:url "http://vim-users.jp/vim-hacks-project/"
  "URL of Vim Hacks Project")

(defconst vim-hacks:list-reverse? nil
  "If not nil, display list in reverse order.")

(defvar vim-hacks:cache '()
  "Cache which stores vim hacks list obtained by `vim-hacks:get-hacks'")

;;------------------------------
;; Define exception error
;;------------------------------
(put 'vim-hacks:exception-not-retrieved 'error-message
     "Vim Hacks - Not retrieved")
(put 'vim-hacks:exception-not-retrieved 'error-conditions
     '(vim-hacks:exception-not-retrieved error))

;;------------------------------
;; System Function
;;------------------------------
(defun vim-hacks:http-get (&optional buffer)
  (if (null buffer) (setq buffer vim-hacks:buffer-name))
  (ignore-errors (kill-buffer buffer))
  (unless (eq 0 (call-process "curl" nil `(,buffer nil) nil
                              "-f"
                              "-X" "GET"
                              vim-hacks:url))
    (signal 'vim-hacks:exception-not-retrieved
            "The requested URL returned error")))

(defun vim-hacks:parse-http-get-response (&optional buffer)
  (cond ((stringp buffer) (setq buffer (get-buffer buffer)))
        ((null buffer) (setq buffer vim-hacks:buffer-name)))
  (save-current-buffer
    (set-buffer buffer)
    (goto-char (point-min))
    (let (begin end lines)
      (re-search-forward "<h2>Hacks[^<]+</h2>[.\t\n]+\\(<ul>\\)" nil t)
      (setq begin (match-beginning 1))
      (setq end (search-forward "</ul>"))
      (mapcar
       (lambda (x)
         (let ((atag (assq 'a x))
               (date (nth 2 x)))
           (cons (concat date (nth 2 atag))
                 (cdr (assq 'href (cadr atag))))))
       (xml-get-children (car (xml-parse-region begin end)) 'li)))))

(defun vim-hacks:get-hacks-from-http ()
  (vim-hacks:http-get)
  (vim-hacks:parse-http-get-response))

(defun vim-hacks:get-hacks ()
  (if (= (safe-length vim-hacks:cache) 0)
      (setq vim-hacks:cache (vim-hacks:get-hacks-from-http)))
  (if vim-hacks:list-reverse? (reverse vim-hacks:cache)
    vim-hacks:cache))

;;------------------------------
;; User Function
;;------------------------------
(defun vim-hacks:anything ()
  (interactive)
  (anything
   `((name . "Vim hacks")
     (candidates . vim-hacks:get-hacks)
     (action
      ("Open Browser" . (lambda (x) (browse-url x))))
     )))


(defun vim-hacks:anything-refresh ()
  (interactive)
  (setq vim-hacks:cache '())
  (vim-hacks:anything))

