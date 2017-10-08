

(in-package codecheck-fw)

(defstruct ai
  name script)

(defun remove-bogus-words (last-word words)
  "Remove words which can never come up in the game
to save on memory and initial filtering in AIs"
  (let ((last-chars (loop :for word :in words
                              :collect (last-char word))))
    (push (last-char last-word) last-chars)
    (delete-if (lambda (word)
                 (not (member (char word 0)
                              last-chars)))
               words)))

(defun run-ai (ai last-word words)
  "Run AI"
  (let* ((cmdline (format nil "~A ~A ~{~A~^ ~}" (ai-script ai) last-word words)))
    (handler-case
        (sb-ext:with-timeout 5
          (uiop:run-program cmdline :output '(:string :stripped t)))
      (sb-ext:timeout ()
        ""))
    ))

(defun process-ai (ai answer last-word words)
  "Display AI's result"
  (let* ((correct (and (member answer words :test #'string=)
                       (char= (last-char last-word) (char answer 0))))
         (result (if correct "OK" "NG")))
    (format t "~A (~A): ~A~%" (ai-name ai) result answer)
    correct))

(defun main (script1 script2 last-word &rest words)
  (let ((ais (list (make-ai :name "FIRST" :script script1)
                   (make-ai :name "SECOND" :script script2))))
    (flet ((rotate-ais ()
             (rotatef (first ais) (second ais))))
      (setf words (remove-bogus-words last-word words))
      (loop (let* ((ai (first ais))
                   (answer (run-ai ai last-word words))
                   (correct (process-ai ai answer last-word words)))
              (unless correct
                (format t "WIN - ~A" (ai-name (second ais)))
                (uiop:quit))
              (deletef words answer :test #'string=)
              (setf last-word answer)
              (rotate-ais))))))

(defun cli-main ()
  (unless (>= (length sb-ext:*posix-argv*) 3)
    (uiop:quit 1))
  (apply #'main (rest sb-ext:*posix-argv*)))

(defun build-main ()
  (sb-ext:save-lisp-and-die "exam2a-fw" :toplevel #'cli-main :executable t))
