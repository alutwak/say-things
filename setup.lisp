
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

(defun say-random-sentence ()
  (format t "~a~%" (car (say (say-clause)))))

(defun speak-occasionally (&optional interval)
  (let ((interval (or interval (parse-integer (cadr sb-ext:*posix-argv*))))
        (*random-state* (make-random-state t)))
    (format t "randomly speaking at most every ~a seconds~%" interval)
    (handler-case
        (loop
          (finish-output *standard-output*)
          (sleep (+ (random interval) 3))
          (say-random-sentence))
      (sb-sys:interactive-interrupt () (sb-ext:exit)))))

(load-dicts)
(sb-ext:save-lisp-and-die #p"say-things" :toplevel #'speak-occasionally :executable t)

