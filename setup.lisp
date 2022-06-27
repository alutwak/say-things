
(load "say-things.asd")
(ql:quickload :say-things)
(in-package :say-things)

(defun load-dicts ()
  (setq *nouns* (read-dictionary "dict/nouns.dict"))
  (setq *verbs* (read-dictionary "dict/verbs.dict"))
  (setq *adjectives* (read-dictionary "dict/adjectives.dict"))
  (setq *adverbs* (read-dictionary "dict/adverbs.dict"))
  (setq *prepositions* (read-dictionary "dict/prepositions.dict"))
  (mapc #'update-dict-weights (list *nouns* *verbs* *adjectives* *adverbs* *prepositions*)))

(defun str-time ()
  (multiple-value-bind (sec min hour date mon yr) (get-decoded-time)
    (format nil "~2,'0d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d" mon date yr hour min sec)))

(defun log-msg (fmt &rest msg)
  (apply #'format
         t
         (append
          (list
           (concatenate 'string "~a - " fmt)
           (str-time))
          msg)))

(defun say-random-sentence ()
  (log-msg "~a~%" (car (say (say-clause)))))

(defun speak-occasionally (&optional interval)
  (let ((interval (or interval (parse-integer (cadr sb-ext:*posix-argv*))))
        (*random-state* (make-random-state t)))
    (log-msg "randomly speaking at most every ~a seconds~%" interval)
    (handler-case
        (loop
          (finish-output *standard-output*)
          (sleep (+ (random interval) 3))
          (say-random-sentence))
      (sb-sys:interactive-interrupt () (sb-ext:exit)))))

(load-dicts)
(sb-ext:save-lisp-and-die #p"say-things" :toplevel #'speak-occasionally :executable t)

