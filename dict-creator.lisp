(in-package :say-things)

(defun index-line-lemma (line)
  (unless (char= #\  (elt line 0))
    (let ((lemma (car (uiop:split-string line))))
      (unless (find-if (lambda (c)
                         (or (member c '(#\. #\_))
                             (and (char>= c #\0) (char<= c #\9))))
                       lemma)
        lemma))))

(defun create-word-dict (type index-path)
  (let ((dict (make-instance 'dictionary)))
    (with-open-file (f index-path)
      (do ((line (read-line f nil :eof) (read-line f nil :eof)))
          ((eq line :eof))
        (let ((lemma (index-line-lemma line)))
          (when lemma
            (add-to-dict dict (make-instance type :lemma lemma)))))
      dict)))

(defun create-noun-dict (index-path exc-path)
  (let ((dict (create-word-dict 'noun index-path)))
    (with-open-file (f exc-path)
      (do ((line (read-line f nil :eof) (read-line f nil :eof)))
          ((eq line :eof))
        (let* ((split (uiop:split-string line))
               (wrd (find-word dict (cadr split))))
          (when wrd
            (setf (slot-value wrd 'plural) (car split))))))
    dict))


(defun create-dictionaries (noun-index noun-exc verb-index adj-index)
  (defvar *nouns* (create-noun-dict noun-index noun-exc))
  (defvar *verbs* (create-word-dict 'verb verb-index))
  (defvar *adjectives* (create-word-dict 'adj adj-index)))
