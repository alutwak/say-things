(in-package :say-things)

(defun convert-oxford-dict (path)
  (multiple-value-bind (nouns verbs adjectives adverbs prepositions)
      (read-oxford-dict path)
    (defvar *nouns* (sorted-dict nouns))
    (defvar *verbs* (sorted-dict verbs))
    (defvar *adjectives* (sorted-dict adjectives))
    (defvar *adverbs* (sorted-dict adverbs))
    (defvar *prepositions* (sorted-dict prepositions))
    (define-special-be-conjugates *verbs*)
    (defvar *wordmap* (create-wordmap *nouns* *verbs* *adjectives* *adverbs* *prepositions*))))

(defun define-special-be-conjugates (dict)
  (let ((be (find-word dict "be")))
    (setf (slot-value be 'infin) "being")
    (setf (slot-value be 'past)
          '((first . "was")
            (second . "were")
            (third . "was")
            (plural . "were")))
    (setf (slot-value be 'present)
          '((first . "am")
            (second . "are")
            (third . "is")
            (plural . "are")))
    (setf (slot-value be 'future)
          "will be")
    (setf (slot-value be 'pres-part)
          "being")
    (setf (slot-value be 'past-part)
          "been")))

(defun create-wordmap (&rest dicts)
  (let ((wordmap (make-hash-table :test #'equal)))
    (mapc (lambda (dict)
            (wordmap-include-dict dict wordmap))
          dicts)
    wordmap))

(defun wordmap-include-dict (dict wordmap)
  (map
   nil
   (lambda (wrd)
     (add-to-wordmap wrd wordmap))
   (words dict)))

(defun add-to-wordmap (wrd wordmap)
  (mapc (lambda (deriv)
          (map-word deriv wrd wordmap))
        (derivatives wrd)))

(defun map-word (deriv wrd wordmap)
  (pushnew wrd (gethash deriv wordmap) :test #'eq))

(defun unbind-dictionaries ()
  (makunbound '*nouns*)
  (makunbound '*verbs*)
  (makunbound '*adjectives*)
  (makunbound '*adverbs*)
  (makunbound '*prepositions*))

;; Oxford reader
(defun read-oxford-dict (path)
  (with-open-file (f path)
    (let ((nouns (make-instance 'dictionary))
          (verbs (make-instance 'dictionary))
          (adjectives (make-instance 'dictionary))
          (adverbs (make-instance 'dictionary))
          (prepositions (make-instance 'dictionary)))
      (do ((words (read-word f) (read-word f)))
          ((eq :eof words))
        (when words
          (mapc (lambda (wrd)
                  (typecase wrd
                    (noun (add-to-dict nouns wrd))
                    (verb
                     (add-to-dict verbs wrd))
                    (adj (add-to-dict adjectives wrd))
                    (adv (add-to-dict adverbs wrd))
                    (preposition (add-to-dict prepositions wrd))))
                words)))
      (values nouns verbs adjectives adverbs prepositions))))

(defvar *type-regex*
  "n\\.|v\\.|[Aa]dj\\.|[Aa]dv\\.|[Pp]rep\\.")

(defun read-word (stream)
  (let ((line (read-line stream nil :eof)))
    (cond ((eq :eof line) :eof)
          ((= 0 (length line)) nil)
          (t (parse-word line)))))

(defun parse-word (line)
  (ppcre:register-groups-bind (wrd def)
      ((format nil "([A-Z][^0-9.]*?)[0-9]?  (—?(?:~a).*)" *type-regex*) line)
    (parse-definition wrd def)))

(defun parse-definition (wrd def)
  (let ((defs))
    ;; (when (string= (string-downcase wrd) "drive")
    ;;   (break))
    (ppcre:do-register-groups (type1 type2 def) 
        ((format nil "(?:^| )—?(~a)(?: & )?(~a)? (\\(.*?\\))?" *type-regex* *type-regex*) def)
      (mapc (lambda (type)
              (when type
                (setq defs (cons (parse-typed-definition wrd type def) defs))))
            (list type1 type2)))
    defs))

(defun parse-typed-definition (wrd type def)
  (case (parse-word-type (string-downcase type))
    ((noun) (parse-noun wrd def))
    ((verb) (parse-verb wrd def))
    ((adj) (parse-adjective wrd def))
    ((adv) (parse-adverb wrd def))
    ((prep) (parse-preposition wrd def))))

(defun parse-word-type (type)
  (cond
    ((string= type "n.")
     'noun)
    ((string= type "v.")
     'verb)
    ((string= type "adj.")
     'adj)
    ((string= type "adv.")
     'adv)
    ((string= type "prep.")
     'prep)))

(defun parse-noun (wrd def)
  (let ((plural (ppcre:register-groups-bind (plural)
                    ("\\(pl\\. (.+)\\)\\." def)
                  plural)))
    (if plural
        (make-instance 'noun :lemma (string-downcase wrd) :plural (string-downcase plural))
        (make-instance 'noun :lemma (string-downcase wrd)))))

(defun parse-verb (wrd def)
  (let* ((wrd (string-downcase wrd))
         (conj-info (ppcre::register-groups-bind (conj-info)
                        ("^\\((.+?)\\)" def)
                      conj-info)))
    (if conj-info
          (make-verb-from-conj-info wrd conj-info)
          (make-instance 'verb :lemma wrd))))

(defvar *tense-regex*
  "pres(?:ent)?|past|pres\\. part\\.|past part\\.")

(defvar *person-regex*
  "1st|2nd|3rd")

(defvar *numeral-regex*
  "sing\\.|pl\\.")

(defun read-verb-conj (str)
  (ppcre:register-groups-bind (person numer tense1 tense2 conj)
      ((format
        nil
        "(~a)? ?(~a)? ?(~a)?(?: and )?(~a)? ?([-A-z]+)$"
        *person-regex* *numeral-regex* *tense-regex* *tense-regex*)
       str)
    (list conj (parse-person person) (parse-numer numer) (parse-tense tense1) (parse-tense tense2))))

(defun parse-person (person)
  (cond ((string= "3rd" person)
         'third)
        ((string= "2nd" person)
         'second)
        ((string= "1st" person)
         'first)))

(defun parse-numer (numer)
  (cond ((string= "sing." numer)
         'singular)
        ((string= "pl." numer)
         'plural)))

(defun parse-tense (tense)
  (cond ((string= "past" tense)
         'past)
        ((or (string= "pres" tense) (string= "present" tense))
         'present)
        ((string= "pres. part." tense)
         'pres-part)
        ((string= "past part." tense)
         'past-part)))

(defun apply-conj (wrd conj)
  "Applies the conjugate, which may come in the form of -<word end><suffix>"
  (if (char= #\- (elt conj 0))
      (apply-suffix wrd (subseq conj 1))
      conj))

(defun make-verb-from-conj-info (wrd conj-info)
  (let ((conj-parts (uiop:split-string conj-info :separator '(#\;)))
        (past)
        (present)
        (infin)
        (pres-part)
        (past-part)
        (conj-connector ""))
    (flet ((append-conj (conj person numer tense)
             (cond ((eq tense 'past)
                    (setq past conj))
                   ((eq tense 'present)
                    (setq present (if person
                                      (acons person conj present)
                                      conj)))
                   ((eq tense 'pres-part)
                    (setq pres-part conj))
                   ((eq tense 'past-part)
                    (setq past-part conj))
                   ((eq numer 'plural)
                    (error "plural verbs not supported"))
                   (t
                    (setq infin conj)))))
      (mapc (lambda (str)
              (let* ((str (string-downcase str))
                     (conj-info (read-verb-conj str)))
                (when conj-info
                  (destructuring-bind (conj person numer tense1 tense2) conj-info
                    (when (ends-with conj "-")
                      ;; this defines a conjugating connector (like spot -> spo(tt)ed)
                      ;; and is also going to be an infinitive (I think)
                      (setq conj-connector (remove #\- conj))
                      (setq conj (add-ing wrd conj-connector)))
                    (let ((conj (apply-conj wrd conj)))
                      (append-conj conj person numer tense1)
                      (when tense2
                        (append-conj conj person numer tense2)))))))
            conj-parts))
    (make-instance
     'verb
     :lemma wrd :infin infin :past past :present present
     :past-part past-part :pres-part pres-part
     :connector conj-connector)))


(defun parse-descriptor (type wrd def)
  (let* ((wrd (string-downcase wrd)))
    (or (ppcre::register-groups-bind (comp super)
            ("^\\(([^,]+), (.+?)\\)" def)
          (make-instance type
                         :lemma wrd
                         :comparative (apply-conj wrd comp)
                         :superlative (apply-conj wrd super)))
        (make-instance type :lemma wrd))))

(defun parse-adjective (wrd def)
  (parse-descriptor 'adj wrd def))

(defun parse-adverb (wrd def)
  (parse-descriptor 'adv wrd def))

(defun parse-preposition (wrd def)
  (declare (ignore def))
  (make-instance 'preposition :lemma wrd))
