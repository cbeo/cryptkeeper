;;;; cryptkeeper.lisp

(defpackage #:cryptkeeper 
  (:use #:cl)
  (:local-nicknames (#:irc #:trivial-irc))
  (:local-nicknames (#:a #:alexandria)))

(in-package #:cryptkeeper)

(defclass movie (bknr.datastore:store-object)
  ((title :accessor movie-title
          :initarg :title
          :initform nil
          :index-type bknr.indices:string-unique-index 
          :index-reader movie-by-title
          :index-values all-movies)
   (year :accessor movie-year
         :initarg :year
         :initform 0
         :index-type bknr.indices:hash-index
         :index-reader movies-by-year)
   (link :accessor movie-link
         :initarg :link
         :initform nil))
  (:metaclass bknr.datastore:persistent-class))

(defclass review (bknr.datastore:store-object)
  ((user :accessor review-user
         :initarg :user
         :initform nil
         :index-type bknr.indices:hash-index
         :index-initargs (:test 'equal)
         :index-reader reviews-by-user)
   (movie :accessor review-movie
          :initarg :movie
          :initform nil
          :index-type bknr.indices:hash-index
          :index-reader reviews-for-movie)
   (rating :accessor review-rating
           :initarg :rating
           :initform 1)
   (highlights :accessor review-highlights
               :initarg :hightlights
               :initform ""))
  (:metaclass bknr.datastore:persistent-class))


(defclass cryptkeeper (irc:client) ())

(defvar *cryptkeeper*)
(defparameter +nick+ "cryptkeeper")
(defparameter +server+ "localhost")
(defparameter +port+ 6667)
(defparameter +log+ "cryptkeeper-log.txt")

(defparameter +joinlist+ '("#top-100-horror-challenge"))

(defparameter +data-store+ #P"./cryptkeeper-datastore/")

(defun init-data-store ()
  (ensure-directories-exist +data-store+)
  (make-instance 'bknr.datastore:mp-store
                 :directory +data-store+
                 :subsystems (list (make-instance 'bknr.datastore:store-object-subsystem))))

(defun start ()
  (let ((*cryptkeeper*
          (make-instance 'cryptkeeper
                         :server +server+
                         :port +port+
                         :nickname +nick+)))
    (init-data-store)
    (irc:connect *cryptkeeper*)
    (loop
      (handler-case 
          (irc:receive-message *cryptkeeper*)
        (error (e)
          (format t "error: ~a~%" e))))))

(irc:define-handler (:rpl_welcome (ck cryptkeeper) prefix arguments)
  (dolist (chan +joinlist+)
    (irc:send-join ck chan)))


(defun tokenize (str)
  (let ((eof (gensym)))
    (labels ((rec (start toks)
               (multiple-value-bind
                     (val position) (let ((*package* (find-package :cryptkeeper)))
                                      (read-from-string str nil eof :start start))
                 (if (eq val eof) (reverse toks)
                     (rec position (cons val toks))))))
      (rec 0 nil))))


(defun handle-message (sender recipient tokens)
  (let ((ms
          (process-command sender tokens))
        (victim
          (if (string-equal recipient (irc:nickname *cryptkeeper*))
              sender recipient)))
    (dolist (m ms) (irc:send-privmsg *cryptkeeper* victim m))))


(defun get-movie (title)
  (movie-by-title title))

(defun add-movie (&key title year link)
  (bknr.datastore:with-transaction () 
    (make-instance 'movie :title title :year year :link link)))

(defun add-movie-op (&key title year link)
  (list 
   (cond ((null title)
          "It is vital that you give a :title Ha Ha Ha Haa.")
         ((get-movie title)
          "You should be in a padded cell, that title was already added well.")
         (t (add-movie :title title :year year :link link)
            "Its all pretty groovy, I've added that movie, Ha Ha Ha Haa."))))

(defun get-review (nick title)
  (find-if
   (lambda (rev) (string-equal title (movie-title (review-movie rev))))
   (reviews-by-user nick)))

(defun add-review (nick &key movie rating comments)
  (bknr.datastore:with-transaction ()
    (make-instance 'review :hightlights comments :rating rating :movie movie :user nick)))

(defun add-review-op (nick &key title rating comments)
  (list 
   (cond ((null title)
          "It is vital that you give a :title Ha Ha Ha Haa.")
         ((null rating)
          "With no :rating you'll keep me waiting, Ha Ha ha ha.")
         ((not (get-movie title))
          "You must be a philosopher, reviewing something that doesn't exist. Ha ha Ha ha.")
         ((get-review nick title)
          "Voting twice isn't nice.")
         (t
          (add-review nick :movie (get-movie title) :rating rating :comments comments)
          "Hating, baiting, or grating, I've added your rating. Hahahaha."))))

(defun rename-movie-op (&key old new)
  (list 
   (a:if-let (movie (and old new (get-movie old)))
     (progn
       (bknr.datastore:with-transaction ()
         (setf (movie-title movie) new))
       "Like a dog with the mange the movie's title underwent a change. Hahahaha!")
     "How can you rename what never was?")))

(defun edit-movie-op (&key title year link)
  (list 
   (a:if-let (movie (get-movie title))
     (progn
       (bknr.datastore:with-transaction ()
         (when year (setf (movie-year movie ) year))
         (when link (setf (movie-link movie ) link)))
       "By my rotting pate the movie is up to date! Hahahahaha!")
     "It is vital that you give a :title Hahahaha")))


(defun edit-review-op (nick &key title rating comments)
  (list 
   (a:if-let (review (and title (get-review nick title)))
     (progn
       (bknr.datastore:with-transaction ()
         (when rating (setf (review-rating review) rating))
         (when comments (setf (review-highlights review) comments)))
       "Unless I faultered your review was altered! Mwahahaha!")
     "Naughty Naughty. You can't edit that.")))

(defun present-movie (m)
  (with-slots (title year link) m
    (let* ((reviews (reviews-for-movie m))
           (average-rating
             (if (zerop (length  reviews)) 0 
                 (/ (reduce #'+ reviews :key #'review-rating  :initial-value 0)
                    (length reviews)))))
      (format nil "~a stars | ~a | ~a | ~a"
              average-rating year title
              (if link link "")))))


(defun get-watched-list (nick &key by)
  (let ((user (if by by nick)))
    (list* (format nil "Movies watched by ~a include: " user)
           (loop :for rev :in (reviews-by-user user)
                 :collect (present-movie (review-movie rev))))))

(defun get-unwatched-list (nick &key by)
  (let* ((user (if by by nick)))
    (list* (format nil "~a is yet to watch: ~%" user)
           (loop :for mov :in (all-movies)
                 :unless (get-review user (movie-title mov))
                   :collect (present-movie mov)))))

(defparameter +help-menu+
  '("COMMANDS:"
    "add movie :title <quoted string> [:year <number>] [:link <quoted url>]"
    "add review :title <quoted string> :rating <number> [:comments <quoted string>]"
    "rename movie :old <quoted string> :new <quoted string>"
    "edit movie :title <quoted string> [:year <number>] [:link <quoted url>]"
    "edit review :title <quoted string> :rating <number> [:comments <quoted string>]"
    "unwatched [:by <quoted string>]"
    "watched [:by <quoted string>]"
    "help")
  )

(defun confused-cryptkeeper (&optional err)
  (list  (if err
             (format nil "AAAAAHHHHHHHh! Either my eyeballs have finally fallen out or else ~s" err)
             "I'm a ghoul not a fool!")))

(defun front (n list)
  (when (< n (length list)) (subseq list 0 n)))

(defun process-command (sender tokens)
  (handler-case 
      (cond
        ((equal '(add movie) (front 2 tokens))
         (apply #'add-movie-op (nthcdr 2 tokens)))

        ((equal '(add review) (front 2 tokens))
         (apply #'add-review-op sender (nthcdr 2 tokens)))

        ((equal '(rename movie) (front 2 tokens))
         (apply #'rename-movie-op (nthcdr 2 tokens)))

        ((equal '(edit movie) (front 2 tokens))
         (apply #'edit-movie-op (nthcdr 2 tokens)))

        ((equal '(edit review) (front 2 tokens))
         (apply #'edit-review-op  sender (nthcdr 2 tokens)))

        ((equal 'unwatched (first tokens))
         (apply #'get-unwatched-list sender (cdr tokens) ))

        ((equal 'watched (first tokens))
         (apply #'get-watched-list sender (cdr tokens)))

        ((equal 'help (first tokens))
         +help-menu+)

        (t (confused-cryptkeeper)))
    (error (e) (confused-cryptkeeper e))))

(irc:define-handler (:privmsg  (ck cryptkeeper) prefix arguments)
  (let* ((sender (irc:prefix-nickname prefix))
         (recipient (first arguments))
         (botnick (irc:nickname ck)))
    (when (and (< (length botnick) (length (second arguments)))
               (string-equal botnick
                             (second arguments)
                             :end2 (length botnick)))
      (handle-message sender
                      recipient
                      (rest (tokenize (second arguments)))))))


(defun build ()
  (sb-ext:save-lisp-and-die #p"cryptkeeper" :toplevel #'start :executable t))
