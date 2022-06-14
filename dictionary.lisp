;;;; dictionary.lisp

(in-package #:say-things)

(defgeneric to-string (word)
  (:documentation "Gets the string from the word."))

(defmethod to-string ((wrd symbol))
  (string-downcase (string wrd)))

(defmethod to-string ((wrd string))
  wrd)

(defclass word ()
  ((lemma :initarg :lemma)
   (freq :initarg :freq :initform 1)))

(defun lemma (wrd)
  (slot-value wrd 'lemma))

(defun freq (wrd)
  (slot-value wrd 'freq))

(defclass noun (word)
  ((plural :initarg :plural)
   (plural? :initform nil)))

(defmethod to-string ((wrd noun))
  (to-string (if (slot-value wrd 'plural?)
                 (slot-value wrd 'plural)
                 (slot-value wrd 'lemma))))

(defmethod initialize-instance :after ((wrd noun) &key plural)
  (if plural
      (setf (slot-value wrd 'plural) plural)
      (let* ((swrd (string (slot-value wrd 'lemma))))
        (setf (slot-value wrd 'plural)
              (make-symbol (add-s swrd))))))

(defclass verb (word)
  ((past-tense :initarg :past-tense)
   (singular :initarg :singular)
   (singular? :initform t :initarg :singular?)
   (tense :initform 'present :initarg :present)))

(defmethod to-string ((wrd verb))
  (to-string
   (cond ((slot-value wrd 'singular?)
          (slot-value wrd 'singular))
         ;; Implement tense conjugation here when implemented
         (t
          (slot-value wrd 'lemma)))))

(defmethod initialize-instance :after ((wrd verb) &key singular)
  (if singular
      (setf (slot-value wrd 'singular) singular)
      (let* ((swrd (string (slot-value wrd 'lemma))))
        (setf (slot-value wrd 'singular)
              (make-symbol (add-s swrd))))))

(defclass adj (word)
  ((comparative :initarg :comparative)
   (superlative :initarg :superlative)
   (type :initarg :type :initform 'normal)))

(defmethod to-string ((wrd adj))
  (to-string
   (case (slot-value wrd 'type)
     ((normal numeral)
      (slot-value wrd 'lemma))
     ((super)
      (slot-value wrd 'superlative))
     ((comp)
      (slot-value wrd 'comparative)))))

(defclass adv (adj) ())

(defvar *add-es*
  '("ch" "sh" "j" "s" "x" "z"))

(defun add-s (str)
  (cond ((some (lambda (end)
                 (ends-with str end))
               *add-es*)
         (concatenate 'string str "es"))
        ((string= (last-n str 1) "y")
         (concatenate 'string (subseq str 0 (- (length str) 1)) "ies"))
        (t
         (concatenate 'string str "s"))))

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
  ((freq-total :initform 0)
   (words :initform (make-array 0 :element-type 'word :adjustable t :fill-pointer t))))

(defmethod choose-random ((object dictionary))
  (choose-random (slot-value object 'words)))

(defun add-to-dict (dict wrd)
  (let ((freq (slot-value wrd 'freq)))
    (vector-push-extend wrd (slot-value dict 'words))
    (incf (slot-value dict 'freq-total) freq)))

(defun find-word (dict wrd)
  (find wrd (slot-value dict 'words) :key #'lemma :test #'string=))

(defun incr-freq (dict wrd n)
  (let ((target (find-word dict wrd)))
    (when target
      (incf (slot-value target 'freq) n)
      (incf (slot-value dict 'freq-total) n))))
