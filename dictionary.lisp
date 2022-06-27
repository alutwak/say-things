;;;; dictionary.lisp

(in-package #:say-things)

(defvar *nouns*)
(defvar *verbs*)
(defvar *adjectives*)
(defvar *adverbs*)
(defvar *prepositions*)
(defvar *wordmap*)

(defgeneric to-string (word)
  (:documentation "Gets the string from the word."))

(defmethod to-string ((wrd symbol))
  (string-downcase (string wrd)))

(defmethod to-string ((wrd string))
  wrd)

(defgeneric derivatives (wrd)
  (:documentation "Returns a list of all derivative words"))

(defgeneric to-alist (wrd &optional sub-a-list)
  (:documentation "Returns the word's properties as an alist"))

(defclass word ()
  ((lemma
    :initarg :lemma
    :reader lemma)
   (freq
    :initarg :freq
    :initform 1
    :reader freq)
   (cumul-weight
    :initform 0
    :accessor weight)))

(defmethod to-alist ((wrd word) &optional sub-a-list)
  (with-slots (lemma freq) wrd
    (append (list `(pos . ,(type-of wrd)) `(freq . ,freq) `(lemma . ,lemma)) sub-a-list)))

(defmethod derivatives ((wrd word))
  (list (lemma wrd)))

(defmethod print-object ((wrd word) stream)
  (format stream "~s" (to-alist wrd)))

(defclass noun (word)
  ((plural
    :initform nil
    :initarg :plural
    :reader plural)
   (numeral
    :initform 'singular
    :initarg :numeral
    :accessor numeral
    :documentation "The number of things the noun refers to ('singular or 'plural)")))

(defmethod initialize-instance :after ((wrd noun) &key plural)
  (if plural
      (setf (slot-value wrd 'plural) plural)
      (let* ((swrd (string (slot-value wrd 'lemma))))
        (setf (slot-value wrd 'plural)
              (add-s swrd)))))

(defmethod to-string ((wrd noun))
  (to-string
   (if (eq (numeral wrd) 'plural)
       (plural wrd)
       (lemma wrd))))

(defmethod to-alist ((wrd noun) &optional sub-a-list)
  (declare (ignore sub-a-list))
  (with-slots (plural) wrd
    (call-next-method wrd (list `(plural . ,plural)))))

(defmethod derivatives ((wrd noun))
  (list (lemma wrd) (plural wrd)))

(defclass verb (word)
  ((infin
    :initarg :infin
    :initform nil
    :reader infin)
   (past
    :initarg :past
    :initform nil
    :reader past)
   (present
    :initarg :present
    :initform nil    
    :reader present)
   (future
    :initarg :future
    :initform nil    
    :reader future)
   (pres-part
    :initarg :pres-part
    :initform nil
    :reader pres-part)
   (past-part
    :initarg :past-part
    :initform nil
    :reader past-part)
   (numeral
    :initform 'singular
    :initarg :numeral
    :accessor numeral
    :documentation "The number of subjects the verb refers to ('singular or 'plural)")
   (person
    :initform 'third
    :initarg :person
    :accessor person)
   (tense
    :initform 'present
    :initarg :tense
    :accessor tense)))

(defmethod initialize-instance :after ((wrd verb) &key present past future connector infin)
  (let ((lemma (string (slot-value wrd 'lemma))))
    (if present
        (setf (slot-value wrd 'present) present)
        (setf (slot-value wrd 'present) (add-s lemma)))
    (if past
        (setf (slot-value wrd 'past) past)
        (setf (slot-value wrd 'past) (add-ed lemma connector)))
    (if future
        (setf (slot-value wrd 'future) future)
        (setf (slot-value wrd 'future) (concatenate 'string "will " lemma)))
    (if infin
        (setf (slot-value wrd 'infin) infin)
        (setf (slot-value wrd 'infin) (add-ing lemma connector)))))

(defmethod to-string ((wrd verb))
  (to-string
   (conjugate-verb wrd)))

(defmethod to-alist ((wrd verb) &optional sub-a-list)
  (declare (ignore sub-a-list))
  (with-slots (infin past present future pres-part past-part) wrd
    (call-next-method wrd (list `(infin . ,infin) `(past . ,past) `(present . ,present)
                                `(future . ,future) `(pres-part . ,pres-part) `(past-part . ,past-part)))))

(defmethod derivatives ((wrd verb))
  (let ((derivs (list (lemma wrd))))
    (when (infin wrd)
      (setq derivs (cons (infin wrd) derivs)))
    (setq derivs
          (if (listp (past wrd))
              (nconc (mapcar #'cdr (past wrd)) derivs)
              (cons (past wrd) derivs)))
    (setq derivs
          (if (listp (present wrd))
              (nconc (mapcar #'cdr (present wrd)) derivs)
              (cons (present wrd) derivs)))
    (when (pres-part wrd)
      (setq derivs (cons (pres-part wrd) derivs)))
    (when (past-part wrd)
      (setq derivs (cons (past-part wrd) derivs)))
    derivs))

(defun conjugate-verb (wrd)
  (let ((conjugates (slot-value wrd (tense wrd))))
    (if (not (listp conjugates))
        conjugates
        (if (eq (numeral wrd) 'plural)
            (cdr (assoc 'plural conjugates))
            (case (person wrd)
              ((first)
               ;; return first-person if it exists, otherwise default to singular
               (or (cdr (assoc 'first conjugates))
                   (cdr (assoc 'singular conjugates))))
              ((second)
               (or (cdr (assoc 'second conjugates))
                   (cdr (assoc 'singular conjugates))))
              ((third)
               (or (cdr (assoc 'third conjugates))
                   (cdr (assoc 'singular conjugates)))))))))

(defclass descriptor (word)
  ((comparative
    :initform nil
    :initarg :comparative
    :reader comparative)
   (superlative
    :initform nil
    :initarg :superlative
    :reader superlative)
   (level
    :initarg :level
    :initform 'normal
    :accessor level
    :documentation "Whether the descriptor is 'normal, 'numeral, 'comp(arative) or 'super(lative)")))

(defmethod initialize-instance :after ((wrd descriptor) &key comparative superlative)
  (let ((lemma (string (slot-value wrd 'lemma))))
    (if comparative
        (setf (slot-value wrd 'comparative) comparative)
        (setf (slot-value wrd 'comparative) (concatenate 'string "more " lemma)))
    (if superlative
        (setf (slot-value wrd 'superlative) superlative)
        (setf (slot-value wrd 'superlative) (concatenate 'string "most " lemma)))))

(defmethod to-string ((wrd descriptor))
  (to-string
   (case (level wrd)
     ((normal numeral)
      (lemma wrd))
     ((super)
      (superlative wrd))
     ((comp)
      (comparative wrd)))))

(defmethod to-alist ((wrd descriptor) &optional sub-a-list)
  (declare (ignore sub-a-list))
  (with-slots (comparative superlative) wrd
    (call-next-method wrd (list `(comparative . ,comparative) `(superaltive . ,superlative)))))

(defmethod derivatives ((wrd descriptor))
  (let ((derivs))
    (mapc (lambda (deriv)
            (when deriv
              (setq derivs (cons deriv derivs))))
          `(,(lemma wrd)
            ,(comparative wrd)
            ,(superlative wrd)))
    derivs))

(defclass adjective (descriptor) ())

(defclass adverb (descriptor) ())

(defclass preposition (word) ())

(defvar *pronoun-map*
  '((first
     (singular
      .
      ((subject . "I") (object . "me") (determiner . "my") (possessive . "mine") (reflexive . "myself")))
     (plural
      .
      ((subject . "we") (object . "us") (determiner . "our") (possessive . "ours") (reflexive . "ourselves"))))
    
    (second
     (singular
      .
      ((subject . "you") (object . "you") (determiner . "your") (possessive . "yours") (reflexive . "yourself")))
     (plural
      .
      ((subject . "you") (object . "you") (determiner . "your") (possessive . "yours") (reflexive . "yourself"))))

    (third
     (masc
      .
      ((subject . "he") (object . "him") (determiner . "his") (possessive . "his") (reflexive . "himself")))
     (fem
      .
      ((subject . "she") (object . "her") (determiner . "her") (possessive . "hers") (reflexive . "herself")))
     (neut
      .
      ((subject . "it") (object . "it") (determiner . "its") (possessive . "its") (reflexive . "itself")))
     (epi
      .
      ((subject . "they") (object . "them") (determiner . "their") (possessive . "theirs") (reflexive . "themself")))
     (plural
      .
      ((subject . "they") (object . "them") (determiner . "their") (possessive . "theirs") (reflexive . "themselves"))))))

(defmacro assoc-ref (item a-list &key (test '#'eq))
  `(cdr (assoc ,item ,a-list :test ,test)))

(defun make-pronoun (&key (person 'third) (numeral 'singular) (gender 'neut) (which 'subject))
  (assoc-ref
   which
   (assoc-ref
    (or (when (eq numeral 'plural) 'plural)
        (when (eq person 'third) gender)
        numeral)
    (assoc-ref
     person
     *pronoun-map*))))

(defvar *add-es*
  '("ch" "sh" "j" "s" "x" "z"))

(defun add-suffix-to-y-word (wrd suffix)
  (let ((before-y (and (> (length wrd) 1) (elt wrd (- (length wrd) 2)))))
    (cond ((member before-y '(#\a #\o #\u))
           (concatenate 'string wrd suffix))
          ((equal before-y #\e)
           (replace-word-end wrd (make-ie-suffix suffix) (- (length wrd) 2)))
          (t
           (replace-word-end wrd (make-ie-suffix suffix) (1- (length wrd)))))))

(defun make-ie-suffix (suffix)
  (if (char= (elt suffix 0) #\e)
      (concatenate 'string "i" suffix)
      (concatenate 'string "ie" suffix)))

(defun add-s (wrd)
  (cond ((some (lambda (end)
                 (ends-with wrd end))
               *add-es*)
         (concatenate 'string wrd "es"))
        ((ends-with wrd "y")
         (add-suffix-to-y-word wrd "s"))
        (t
         (concatenate 'string wrd "s"))))

(defun add-e-suffix (wrd suffix &optional connector)
  (let ((wrd (apply-suffix wrd connector)))
    (cond ((ends-with wrd "e")
           (replace-word-end wrd suffix (- (length wrd) 1)))
          ((ends-with wrd "y")
           (add-suffix-to-y-word wrd suffix))
          (t
           (concatenate 'string wrd suffix)))))

(defun add-ed (wrd &optional connector)
  (add-e-suffix wrd "ed" connector))

(defun add-er (wrd &optional connector)
  (add-e-suffix wrd "er" connector))

(defun add-est (wrd &optional connector)
  (add-e-suffix wrd "est" connector))

(defun add-ing (wrd &optional connector)
  (if (ends-with wrd "e")
      (replace-word-end wrd "ing" (- (length wrd) 1))
      (concatenate 'string (apply-suffix wrd connector) "ing")))

(defun find-suffix-crossover (wrd suffix)
  "Finds the index in a word at which a suffix should be applied. The suffix will begin
   with the sequence of letters that cross over with the leters in the word where the
   suffix should begin (ie 'mingle' + 'ling' -> mingling"
  (do* ((suffix-end (length suffix)
                    (1- suffix-end))
        (sub-suffix (subseq suffix 0 suffix-end)
                    (subseq suffix 0 suffix-end))
        (crossover (search sub-suffix wrd :test #'string= :from-end t)
                   (search sub-suffix wrd :test #'string= :from-end t)))
       ((or crossover (= suffix-end 1)) crossover)))

(defun apply-suffix (wrd suffix)
  (let ((crossover (find-suffix-crossover wrd suffix)))
    (when crossover
        (replace-word-end wrd suffix crossover))))

(defun replace-word-end (wrd suffix join-idx)
  (concatenate 'string (subseq wrd 0 join-idx) suffix))

(defun ends-with (str end)
  (let* ((str-len (length str))
         (end-len (length end))
         (start (- str-len end-len)))
    (when (>= start 0)
      (string= (subseq str start) end))))

(defun last-n (seq n)
  (let ((len (length seq)))
    (subseq seq (max (- len n) 0))))

(defclass dictionary ()
  ((freq-total
    :initform 0
    :reader freq-total)
   (weight
    :initform 0
    :accessor weight)
   (words
    :initform (make-array 0 :element-type 'word :adjustable t :fill-pointer t)
    :reader words)))

(defmethod choose-random ((dict dictionary))
  (choose-random (words dict)))

(defmethod choose-random-weighted ((dict dictionary))
  (let ((weight (random (weight dict))))
    (find-word-by-weight weight dict)))

(defun add-to-dict (dict wrd)
  (let* ((freq (freq wrd))
         (dict-words (words dict))
         (this-word (derivatives wrd))
         (last-word (when (> (length dict-words) 0) (derivatives (elt dict-words (1- (length dict-words)))))))
    (unless (equal this-word last-word)
      (vector-push-extend wrd (words dict))
      (incf (slot-value dict 'freq-total) freq))))

(defun update-dict-weights (dict)
  (let ((weight 0))
    (map
     nil
     (lambda (wrd)
       (incf weight (freq wrd))
       (setf (weight wrd) weight))
     (words dict))
    (setf (weight dict) weight)))

(defun sorted-dict (dict)
  (sort (words dict) #'string< :key #'lemma)
  dict)

(defun find-sorted (item seq test-less test-equal key &key (begin 0) (end (length seq)) last-if-no-match)
  (if (> end (1+ begin))
      (let* ((middle (+ (floor (/ (- end begin) 2)) begin))
             (target (elt seq middle))
             (test (funcall key target)))
        (cond ((funcall test-equal item test)
               target)
              ((funcall test-less item test)
               (find-sorted item seq test-less test-equal key :begin begin :end middle :last-if-no-match last-if-no-match))
              (t
               (find-sorted item seq test-less test-equal key :begin middle :end end :last-if-no-match last-if-no-match))))
      (and last-if-no-match (if (>= end (length seq))
                                (elt seq begin)
                                (elt seq end)))))

(defun find-word (wrd dict)
  (find-sorted wrd (words dict) #'string< #'string= #'lemma))

(defun find-word-by-weight (weight dict)
  (find-sorted weight (words dict) #'< #'= #'weight :last-if-no-match t))

(defun incr-freq (dict wrd n)
  (let ((target (find-word dict wrd)))
    (when target
      (incf (slot-value target 'freq) n)
      (incf (slot-value dict 'freq-total) n))))

(defun print-dictionary (dict &optional stream)
  (format stream "(~%")
  (map nil
       (lambda (wrd)
         (format stream " ~s~%" wrd))
       (words dict))
  (format stream ")"))

(defun write-dictionary (dict path)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (print-dictionary dict stream)))

(defun read-dictionary (path)
  (with-open-file (stream path)
    (let ((dict (make-instance 'dictionary))
          (word-list (read stream)))
      (mapc
       (lambda (wrd-alist)
         (add-to-dict dict (word-from-alist wrd-alist)))
       word-list)
      (sorted-dict dict))))

(defun word-from-alist (wrd-alist)
  (case (assoc-ref 'pos wrd-alist)
    (noun
     (noun-from-alist wrd-alist))
    (verb
     (verb-from-alist wrd-alist))
    ((adjective adverb)
     (descriptor-from-alist wrd-alist))
    (t
     (make-instance
      (assoc-ref 'pos wrd-alist)
      :lemma (assoc-ref 'lemma wrd-alist)
      :freq (assoc-ref 'freq wrd-alist)))))

(defun noun-from-alist (wrd-alist)
  (make-instance
   'noun
   :lemma (assoc-ref 'lemma wrd-alist)
   :freq (assoc-ref 'freq wrd-alist)
   :plural (assoc-ref 'plural wrd-alist)))

(defun verb-from-alist (wrd-alist)
  (make-instance
   'verb
   :lemma (assoc-ref 'lemma wrd-alist)
   :freq (assoc-ref 'freq wrd-alist)
   :infin (assoc-ref 'infin wrd-alist)
   :past (assoc-ref 'past wrd-alist)
   :present (assoc-ref 'present wrd-alist)
   :future (assoc-ref 'future wrd-alist)
   :pres-part (assoc-ref 'pres-part wrd-alist)
   :past-part (assoc-ref 'past-part wrd-alist)))

(defun descriptor-from-alist (wrd-alist)
  (make-instance
   (assoc-ref 'pos wrd-alist)
   :lemma (assoc-ref 'lemma wrd-alist)
   :freq (assoc-ref 'freq wrd-alist)
   :comparative (assoc-ref 'comparative wrd-alist)
   :superlative (assoc-ref 'superlative wrd-alist)))

(defun dict-path-name (dir name)
  (make-pathname :directory `(:relative ,dir) :name name :type "dict"))

(defun dump-dictionaries (dir-path)
  (when (ensure-directories-exist dir-path)
    (write-dictionary *nouns* (dict-path-name dir-path "nouns"))
    (write-dictionary *verbs* (dict-path-name dir-path "verbs"))
    (write-dictionary *adjectives* (dict-path-name dir-path "adjectives"))
    (write-dictionary *adverbs* (dict-path-name dir-path "adverbs"))
    (write-dictionary *prepositions* (dict-path-name dir-path "prepositions"))))
