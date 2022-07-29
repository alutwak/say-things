
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

(defun speak-occasionally (interval)
  (let ((*random-state* (make-random-state t)))
    (log-msg "randomly speaking at most every ~a seconds~%" interval)
    (handler-case
        (loop
          (finish-output *standard-output*)
          (sleep (+ (random interval) 3))
          (say-random-sentence))
      (sb-sys:interactive-interrupt () (sb-ext:exit)))))

(defun parse-interval (arg)
  (handler-case
      (let ((interval (parse-integer arg)))
        (if (> interval 0)
            interval
            'error))
    (sb-int:simple-parse-error () 'error)))

(defvar *help-desc*
  "A valuable utility for those who need their Macintosh computers to say mostly unintelligible, random sentences

USEAGE:
    say-things [INTERVAL] [OPTIONS]

DESCRIPTION:
    If a positive integer interval is given, say-things will run indefinitely, saying interesting random sentences until the
    program is killed with a Ctl-C. If no interval is given, it will say a single random sentence and then exit.

OPTIONS:
    -h, --help    Print this help message")

(defun print-help ()
  (write-line *help-desc*))

(defun exec-from-argv (argv)
  (let* ((args (cdr argv))
         (interval (and args (parse-interval (car args)))))
    (cond
      ((eq interval 'error)
       (lambda ()
         (print-help)
         (format t "~%INTERVAL must be a positive integer~%")))
      ((or (find "-h" args :test #'string=)
           (find "--help" args :test #'string=))
       #'print-help)
      (interval
       (lambda ()
         (speak-occasionally interval)))
      (t
       (lambda ()
          (say (say-clause)))))))

(defun main ()
  (let ((exec (exec-from-argv sb-ext:*posix-argv*)))
    (funcall exec)))


(load-dicts)
(sb-ext:save-lisp-and-die #p"say-things" :toplevel #'main :executable t)
