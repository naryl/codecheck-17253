
(in-package codecheck-ai)

(defparameter *recursion-limit* 100000)

(defvar *strings*)

(defun word (i)
  (aref *strings* i))

(defun filter-words (last-word words)
  (remove-if (lambda (word)
               (char/= (char (word word) 0) (last-char last-word)))
             words))

(defun try-candidate (candidate words depth max-depth)
  (block nil
    (when (>= depth max-depth)
      (return :uncertain))
    (let* ((words-without-candidate (remove candidate words))
           (next-words (filter-words (word candidate) words-without-candidate)))
      (loop :named enemy-turn
         :for next-word :in next-words
         :do (case (try-candidate next-word
                                  words-without-candidate
                                  (1+ depth)
                                  (min max-depth
                                       (/ *recursion-limit* (length next-words))))
               (:win
                ;; The enemy will have a winning move
                (return :lose))
               (:uncertain
                ;; There's a deep and wide recursion going on
                (return :uncertain))))
      ;; The enemy will have no winning moves
      :win)))

(defun solution (last-word words)
  (block nil
    (let ((candidates (filter-words last-word words))
          (uncertain-candidates nil))
      (unless candidates
        (return ""))
      ;; Look for a sure win
      (loop :named sure
         :for candidate :in candidates
         :do (case (try-candidate candidate words 0 *recursion-limit*)
               (:win (return (word candidate)))
               (:uncertain (push candidate uncertain-candidates))))
      ;; No candidate results in a sure win. Pick a random one preferring uncertain options
      ;; But will lose anyway against an intelligent ai
      (if uncertain-candidates
          (word (elt uncertain-candidates (random (length uncertain-candidates))))
          (word (elt candidates (random (length candidates))))))))

(defun main (last-word &rest words)
  (setf *strings* (make-array (length words) :initial-contents words))
  (format t "~A" (solution last-word (range (length words)))))

(defun cli-main ()
  (unless (>= (length sb-ext:*posix-argv*) 2)
    (uiop:quit 1))
  (apply #'main (rest sb-ext:*posix-argv*)))

(defun build-main ()
  (sb-ext:save-lisp-and-die "exam2a-ai" :toplevel #'cli-main :executable t))
