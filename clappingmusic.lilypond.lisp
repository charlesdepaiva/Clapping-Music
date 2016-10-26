;imported function (lisp cookbook)

(defun split-by-newline (string)
  (loop
     for i = 0 then (1+ j)
     as j = (position #\Newline string :start i)
     collect (subseq string i j)
     while j))


;;; general functions

(defun lily-header (title composer subtitle)
  (format t "~{~a~}"(list
		     (format nil "\\header \{~%")
		     (format nil "~T title = ~a~%" title)
		     (format nil "~T composer = ~a~%" composer)
		     (format nil "~T subtitle = ~a~%" subtitle)
		     (format nil "~T subsubtitle = \\date~%\}"))))

(defun lily-block (expression &optional (command "score") (parameter nil))
  (if (find #\Newline expression) (setq expression (split-by-newline expression)))
  (format t "\\~a ~@[~a ~]\{~{~%~T ~a~}~%\}" command parameter expression))


(defun lily-variable (name expression)
  (format t "~a = \{~%~T ~a~%\}" name expression))

(defun lily-repeat (m-expression &optional (times 2) (repeat-type "volta"))
  (if (find #\Newline m-expression) (setq m-expression (split-by-newline m-expression)))
  (format nil "\\repeat ~a ~a \{~{~%~T ~a~}~%\}"
	  repeat-type times m-expression))

(defun lily-date-message (&optional (message "automatically generated in"))
  (format nil "date = #(format \"~a ~~a\" (strftime \"\%c\" (localtime (current-time))))" message))


(defun lily-staff (m-expression &key type spacing name tempo omit xtra)
  (if (find #\Newline m-expression) (setq m-expression (split-by-newline m-expression))
      (when (not (listp m-expression)) (setq m-expression (list m-expression))))
  (when (not (listp name)) (setq name (list name)))
  (let ((staff) (op-with))
    (push (format nil "\}") staff)
    (push (format nil "~{~t ~a~}" m-expression) staff)
    (when xtra (push (format nil "~{~t ~a~}" xtra) staff))
    (when omit (push (format nil "~t \\omit ~a" omit) staff))
    (when tempo (push (format nil "~t \\tempo ~a = ~a" (first tempo) (second tempo)) staff))
    (push (format nil "\{") staff)
    (when (or name spacing) (push (format nil "\}") staff)
	  (when spacing (setf staff (append (indent spacing 2) staff)))
	  (when name (push (format nil "~t midiInstrument = #~s" (first name)) staff)
		(if (second name) (push (format nil "~t shortInstrumentName = #~s" (second name)) staff))
		(push (format nil "~t instrumentName = #~s" (first name)) staff))
	  (setq op-with "\\with \{"))
    (push (format nil "\\new ~@[~a~]Staff ~@[~a~]" type op-with) staff)
    staff))	

(defun expand-spacing (&key (grob "VerticalAxisGroup") basic-dist min-dist padding stretch)
  (let ((spacing "staff-staff-space")
	(pair))
    (when stretch (push (list "stretchability" stretch) pair))
    (when padding (push (list "padding" padding) pair))
    (when min-dist (push (list "minimum-distance" min-dist) pair))
    (when basic-dist (push (list "basic-distance" basic-dist) pair))
    (if (= (length pair) 1)
	(let ((x (car pair)))
	     (list (format nil "\\override ~a.~a.~a = #~a" grob spacing (first x) (second x))))
	(cons (format nil "\\override ~a.~a = " grob spacing)
	      (let ((fim (mapcar #'(lambda (x) (format nil "~5t(~a . ~a)" (first x) (second x))) pair)))
		(append (list (format nil "~2t#'(~a" (subseq (car fim) 5)))
			(butlast (cdr fim))
			(list (format nil "~a)" (car (last fim))))))))))

(defun staff-block (staves &key type xtra)
  (let ((temp
    (cond
      ((equal type "group") "\\new StaffGroup ")
      ((equal type "choir") "\\new ChoirStaff ")
      ((equal type "piano") "\\new PianoStaff ")
      ((equal type "grand") "\\new GrandStaff ")
      (t nil))))
    (append (list (format nil "~@[~a~]<<" temp))
	    (when xtra (indent xtra 2))
	    (indent staves 2)
	    '(">>"))))

(defun indent (strings n)
  (mapcar #'(lambda (x) (format nil (format nil "~~~at~~a" n) x)) strings))

;------------------------------------

(defun print-event (figure &key (pitch "c") dynamic articulation)
  (format nil "~a~a~@[~a~]~@[~a~]" pitch (/ 1 figure) dynamic articulation))
  
(defstruct note
  pitch
  figure
  dynamic
  articulation)

(defun note2lily (note)
  (print-event (note-figure note)
	       :pitch (note-pitch note)
	       :dynamic (note-dynamic note)
	       :articulation (note-articulation note)))
(defstruct pause
  figure)

(defun pause2lily (pause)
  (print-event (pause-figure pause)))

(defstruct measure
  tempo
  time-signature
  events)

(defun events2lily (events)
  (format nil "~{~a ~}" 
	  (mapcar #'(lambda (x)
		      (cond ((note-p x) (note2lily x))
			    ((pause-p x) (pause2lily x))))
		  events)))

(defun measure2lily (measure  &key tempo? sig?)
  (let ((tempo (measure-tempo measure))
	(time-signature (measure-time-signature measure))
	(events (measure-events measure)))
    (list (when tempo? (format nil "\\tempo ~a = ~a" (first tempo) (second tempo)))
	  (when sig? (format nil "\\time ~a" time-signature))
	  (events2lily events))))

(defun teste ()
  (let* ((tt (mapcar #'(lambda (x) (make-pause :figure x)) '(1/4 1/4 1/4 1/4)))
	(xx (make-measure :tempo '(8 120) :time-signature 4/4 :events tt)))
    (measure2lily xx)))


(defun measures2lily (measures)
  (let ((ant-tempo -1) (ant-sig -1)) ;ant-tempo e ant-sig guardam os valores do tempo e compasso encontrados no penultimo estagio do loop
      
    (loop for item in measures
       collect (measure2lily item
			     (not (equal (measure-tempo item) ant-tempo)) ;imprime tempo caso tempo atual seja diferente do tempo anterior
			     (not (equal (measure-time-signature item) ant-sig))) ;imprime compasso caso compasso atual seja diferente do anterior
	 (setq (ant-tempo (measure-tempo item)) ;atualiza variaveis que guardam o tempo e o compasso anteriores
	       (ant-sig (measure-time-signature item))))))
		

(defstruct repeats
  measures
  type
  n)

(defun repeats2lily (repeats)
  (lily-repeat (measures2lily (repeats-measures repeats))
	       (repeats-n repeats)
	       (repeats-type repeats)))

(defun staff2lily ())

(defstruct staff-system
  staffs)

(defun staff-system2lily ())

(defun figures2measure (figures)
  (make-measure :events (mapcar #'figure2note figures)))

(defun measures2staff (measures)
  (make-staff :measures (mapcar #'figures2measure measures)))

(defun staff2staff-system (staves)
  (make-staff-system :staffs (mapcar #'measures2staff staves)))

(defun figure2note (figure &optional (pitch "c"))
  (declare (ratio figure) (string pitch))
  (if (< figure 0)
      (make-pause :figure figure)
      (make-note :figure figure
		 :pitch pitch)))



;;; specific functions

(defun save-clapping-ly ()
)

;(mapcar #'(lambda (x) (format nil "c~a" x)) figuras)
