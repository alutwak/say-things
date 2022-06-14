;;;; say-things.lisp

(in-package #:say-things)

(defvar *voices*
  '("Alex" "Daniel" "Fred" "Karen"
    "Moira" "Rishi" "Samantha"
    "Tessa" "Veena" "Victoria"))

(defvar *voice* "Alex")

(defun say (words)
  (let ((args (mapcar #'to-string words)))
    (uiop:run-program (append `("say" "-v" ,*voice*) args))
    args))

(defgeneric choose-random (object)
  (:documentation "Chooses a random word from the object"))

(defmethod choose-random ((object sequence))
  (elt object (random (length object))))

(defun say-clause ()
  "Says a traditional subject-object clause. Future versions might generate more complex or varied
   structures
  "
  (let* ((verb (say-verb))
         (subject (say-subject verb))
         (object (say-object verb subject)))
    (append subject (list verb) object)))

;;(defvar *verbs* '(eats likes has wants feels sees))

(defun say-verb ()
  "Chooses a random verb from the dictionary. Right now, a verb is just a single symbol,
   but in the future it might also come with metadata.
  "
  (choose-random *verbs*))

;;(defvar *nouns* '(cheese penguin tree agent bastard belt pickle toy butt penis hand elephant engine baby))

(defun say-noun (verb)
  (declare (ignore verb))
  (choose-random *nouns*))

(defun say-subject (verb)
  (say-noun-phrase verb))

(defun say-object (verb subject)
  (declare (ignore verb subject))
  (say-noun-phrase nil))

(defun say-noun-phrase (verb)
  "Says a noun-phrase that agrees with the given verb. Right now, all verbs are infinitive, and all
   noun subject objects will agree with the verb options, so the verb argument will be ignored"
  (let* ((noun (say-noun verb))
         (modifiers (modify-noun noun))
         (article (choose-article modifiers noun)))
    (append (list article) modifiers (list noun))))

(defun modify-noun (noun)
  (say-adjective-list noun))

;; (defvar *adjectives* '(blue wrong weird ugly small distinct sharp moldy lost old large shiny sweet blonde noisy sexy yellow))

(defun say-adjective-list (noun &optional adjectives)
  (let ((test (random (+ (length adjectives) 2))))
    (if (> 1 test)
        (say-adjective-list noun (cons (choose-random *adjectives*) adjectives))
        adjectives)))

(defvar *articles* '((a . an) (the . the) (that . that) (this . this)))

(defun choose-article (modifiers noun)
  (let ((first-word (or (car modifiers) noun))
        (article (choose-random *articles*)))
    (if (is-vowel (elt (to-string first-word) 0))
        (cdr article)
        (car article))))

(defun is-vowel (char)
  (member char '(#\a #\e #\i #\o #\u)))
