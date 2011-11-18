(defconst vim-hacks:buffer-name "*vim-hacks*")
(defconst vim-hacks:url "http://vim-users.jp/vim-hacks-project/")

(put 'vim-hacks:exception-not-retrieved 'error-message "Vim Hacks - Not retrieved")
(put 'vim-hacks:exception-not-retrieved 'error-conditions
     '(vim-hacks:exception-not-retrieved error))

(defun vim-hacks:http-get ()
  (ignore-errors (kill-buffer vim-hacks:buffer-name))
  (unless (eq 0 (call-process "curl" nil `(,vim-hacks:buffer-name nil) nil
                              "-f"
                              "-X" "GET"
                              vim-hacks:url))
    (signal 'vim-hacks:exception-not-retrieved "The requested URL returned error")))

(defun vim-hacks:get-hacks (&optional buffer)
  (cond ((stringp buffer) (setq buffer (get-buffer buffer)))
        ((null buffer) (setq buffer vim-hacks:buffer-name)))
  (save-current-buffer
    (set-buffer buffer)
    (goto-char (point-min))
    (let (bgin end lines)
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

(defun vim-hacks:anything ()
  (interactive)
  (vim-hacks:http-get)
  (anything
   `((name . "Vim hacks")
     (candidates . vim-hacks:get-hacks)
     (action
      ("Open Browser" . (lambda (x) (browse-url x))))
     )))
