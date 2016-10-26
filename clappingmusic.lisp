(defpackage :clapping
  (:use :common-lisp))

(in-package :clapping)

(declaim (optimize (debug 1) (safety 3) (speed 1)))

;imported functions

(defmethod permut-circn ((list list) &optional (nth 1))
  (when list
    (let ((length (length list))
	  n-1thcdr)
      (setq nth (mod nth length))
      (if (zerop nth) list
	  (prog1
	      (cdr (nconc (setq n-1thcdr (nthcdr (1- nth) list)) list))
	    (rplacd n-1thcdr ()))))))


(defmethod rotate ((list list) &optional (nth 1))
    (permut-circn (copy-list list) nth))

;utils

(defun downward-symmetry (m n)
  (declare (number m n))
  (+ m (abs (- m n))))

(defun calculate-timesignature (list-of-figures)
  (declare (cons list-of-figures) (values number))
  (apply #'+ list-of-figures))

(defun figure->sec (list-of-figures bpm)
  (loop
     with semibreve = (/ 60 bpm)
     for figure in list-of-figures
     collect (float (* semibreve figure))))


(defun seq->ons-durs (sequence)
  (loop
     with durations = (cons 0 (butlast sequence))
     for figure in sequence
     for dur in durations
     sum (abs dur) into z
     when (> figure 0) collect (list z figure)))


;;; main functions

(defun beats-and-rest (figure number-of-beats rest-factor)
  (declare (ratio figure) (integer number-of-beats) (number rest-factor) (values cons))
  (append (make-list number-of-beats :initial-element figure)
	  (list (* -1 (* figure rest-factor)))))

(defun beats-and-rest-sequence (figure max min step rest-factor)
  (declare (integer max min) (number step) (values cons))
  (loop
     repeat (- (* 2 max) 2)
     for i = max then (- i step) 
     append (beats-and-rest figure (round (downward-symmetry min i)) rest-factor)))


(defun clapping-lines (figure max min step rest-factor)
  (loop
     with rhythm = (beats-and-rest-sequence figure max min step rest-factor)
     with repetitions = (length rhythm)
     for rotation from 0 upto repetitions
     collect rhythm into line-one
     collect (rotate rhythm rotation) into line-two
     finally (return (list line-one line-two))))


(defun clapping-ritornello (lines)
  (labels ((ritornello (line)
	     (loop
		with repetitions = (1- (length line))
		for measure in line
		append (loop repeat repetitions collect measure))))
    (loop
       for line in lines
       collect (ritornello line))))

;;; organizing data for output

(defun add-info-to-line (line line-number)
  (let ((data))
    (loop 
       for measure in line
       do (loop
	     for figure in measure
	     count figure into pos
	     do (push (list figure pos line-number)
		      data)))
    (nreverse data)))

(defun clapping-with-info (list-of-lines)
  (loop
     for line in list-of-lines
     count line into line-number
     collect (add-info-to-line line line-number)))

(defun line-in-secs (line bpm)
  (let ((transposed-line (apply #'mapcar #'list line)))
    (setf (car transposed-line) (seq->ons-durs (figure->sec (car transposed-line) bpm)))
    (apply #'mapcar #'(lambda (x y z) (append x (list y) (list z))) transposed-line)))

(defun clapping-in-secs (list-of-lines bpm)
  (loop
     for line in list-of-lines
     append (line-in-secs line bpm)))

(defun clapping-data (list-of-lines &optional (bpm 40))
  (sort (clapping-in-secs (clapping-with-info list-of-lines) bpm)
	#'<
	:key #'car))

;;;output

(defun save-clapping-data (data &optional (filename "clappingmusic"))
  (with-open-file
      (outdata (make-pathname :name filename :type "dat" :defaults *default-pathname-defaults*)
	       :direction :output
	       :if-exists :supersede
	       :if-does-not-exist :create)
    (format outdata "~:{~A ~A ~A ~A ~%~}" (append (list '(onset duration pos voice-number)) data))
    (namestring outdata)))


(defun save-clapping-gpl (datafile &optional (term "wxt"))
  (with-open-file
      (gpl (make-pathname :name (pathname-name datafile) :type "gpl" :defaults *default-pathname-defaults*)
	   :direction :output
	   :if-exists :supersede
	   :if-does-not-exist :create)
    (format gpl "~{~a~}" (list
			  (format nil "set term ~a " term)
			  (format nil "size 1000, 400~%")
			  (format nil "unset key~%")
			  (format nil "set title 'Clapping Music for two performers (1972) by Steve Reich'~%")
			  (format nil "set xtics out~%")
			  (format nil "set xlabel 'Onsets (s)'~%")
			  (format nil "set ytics ('Voice no.1 (Shifting)' 1, 'Voice no. 2 (No shift)' 2) out~%")
			  (format nil "plot ")
			  (format nil "[-1:10] [0:2] ")
			  (format nil "~S " datafile)
			  (format nil "using 1:($3==1?$3:1/0) ") 
			  (format nil "with impulses lt 10 lw 4,~%")
			  (format nil "~S " datafile)
			  (format nil "using 1:($3==2?$3:1/0) ") 
			  (format nil "with impulses lt 7 lw 4,~%")))
    (namestring gpl)))


      

