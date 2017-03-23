(c-declare
"
#include \"glue.c\"
#include \"joystick.h\"
struct js_event input_event;
")


(define *VERSION* "1.0.1")

;;;; Global Constants
(define *JS-EVENT-AXIS* #x02)
(define *JS-EVENT-BUTTON* #x01)
(define *BUTTON-PRESSED* #x01)
(define *BUTTON-RELEASED* #x00)
(define *AXIS-REAL-MIN* -32767)
(define *AXIS-REAL-MAX* 32767)
(define *AXIS-POS-MAX* #xFFFF)
(define *AXIS-POS-MIN* #x0000)
(define *MIDI-VAL-MAX* #x7F)
(define *MIDI-VAL-MIN* #x00)
(define *MIDI-VAL-CENTER* #x40)
(define *MIDI-PITCH-BEND-MAX* #x3FFF)
(define *MIDI-PITCH-BEND-CENTER* #x2000)

;;;; Misc Helper Functions

(define deadzone (make-parameter 0))

(define midi-spline-knots
  (lambda () `((,*AXIS-REAL-MIN* . ,*MIDI-VAL-MIN*)
	       (,(- (deadzone)) . ,*MIDI-VAL-CENTER*)
	       (,(deadzone) . ,*MIDI-VAL-CENTER*)
	       (,*AXIS-REAL-MAX* . ,*MIDI-VAL-MAX*))))

(define midi-pitch-bend-spline-knots
  (lambda () `((,*AXIS-REAL-MIN* . ,*MIDI-VAL-MIN*)
	       (,(- (deadzone)) . ,*MIDI-PITCH-BEND-CENTER*)
	       (,(deadzone) . ,*MIDI-PITCH-BEND-CENTER*)
	       (,*AXIS-REAL-MAX* . ,*MIDI-PITCH-BEND-MAX*))))

(define deadzone-knots
  (lambda () `((,*AXIS-REAL-MIN* . ,*AXIS-REAL-MIN*)
	       (,(- (deadzone)) . 0)
	       (,(deadzone) . 0)
	       (,*AXIS-REAL-MAX* . ,*AXIS-REAL-MAX*))))

(define (find-segment val spline-knots)
  (let loop ((lower-bound (car (spline-knots)))
	     (bounds (cdr (spline-knots))))
    (cond ((null? bounds) '())
	  (else
	   (let ((upper-bound (car bounds)))
	     (if (and (<= (car lower-bound) val)
		      (<= val (car upper-bound)))
		 (cons lower-bound  upper-bound)
		 (loop upper-bound (cdr bounds))))))))

(define (transpose val segment)
  (let* ((lower-bound (car segment))
	 (upper-bound (cdr segment))
	 (scaled-val (- val (car lower-bound)))
	 (steps (+ 0.0 (- (car upper-bound) (car lower-bound))))
	 (ramp (/ (- (cdr upper-bound) (cdr lower-bound)) steps)))
    (inexact->exact (round (+ (cdr lower-bound) (* ramp scaled-val))))))


(define (transpose-to-deadzone val)
  (transpose val (find-segment val deadzone-knots)))

(define (transpose-to-midi val)
  (transpose val (find-segment val midi-spline-knots)))

(define (transpose-to-pitch-bend val)
  (transpose val (find-segment val midi-pitch-bend-spline-knots)))


;;; rotate list left: (rotate-left '(1 2 4)) -> '(4 1 2)
(define (rotate-left lst)
  (append (cdr lst) (list (car lst))))
  ;`(,@(cdr lst) ,@(cons (car lst) '())))


;;; returns a hash (ev-id #x40 #30) -> #x4030
(define (ev-id t i)
  (bitwise-and
   (bitwise-ior (bitwise-and (arithmetic-shift t 8) #xFF00) (bitwise-and i #xFF))
   #xFFFF))



;;;; C Interface

;;; wrapper for c struct js_event
(define-structure js-event ev-id value type number valid is-button? is-axis?)

;;; Functions to interact with static allocated struct js_event
(define _js-event-value (c-lambda () int16 "___return(input_event.value);"))
(define _js-event-type (c-lambda () unsigned-int8 "___return(input_event.type);"))
(define _js-event-number (c-lambda () unsigned-int8 "___return(input_event.number);"))
(define _update-js-event (c-lambda (int) int "int res = get_joystick_event(___arg1, &input_event); ___return(res);"))
(define debug-print-js-event (c-lambda () void "debug_print_joystick_event(&input_event);"))
(define (print-js-event event)
  (let ((value (js-event-value event))
	(type (js-event-type event))
	(number (js-event-number event))
	(valid (js-event-valid event))
	(is-button (js-event-is-button? event))
	(is-axis (js-event-is-axis? event)))
    (print "event val: " value " type: " type " number: " number " valid: " valid " is-button: " is-button " is-axis: " is-axis "\n")))

(define (get-js-event-function fd)
  (define (get-js-event)
    (let ((valid (if (equal? 0 (_update-js-event fd)) #t #f))
	  (event-type (_js-event-type)))
      (make-js-event (ev-id (_js-event-type) (_js-event-number))
		     (_js-event-value)
		     event-type
		     (_js-event-number)
		     valid
		     ((lambda () (if (equal? event-type *JS-EVENT-BUTTON*) #t #f)))
		     ((lambda () (if (equal? event-type *JS-EVENT-AXIS*) #t #f))))))
  get-js-event)


;;; Functions to interact with JackAudioServer
(define setup-jack (c-lambda () int "_setup_jack"))
(define send-midi (c-lambda (scheme-object int) int "int res = SEND_MIDI(___arg1, ___arg2);  ___return(res);"))


;;; Functions to interact with joystick device file
(define open-joystick (c-lambda (nonnull-char-string) int "open_joystick" ))
(define close-joystick (c-lambda (int) int "close_joystick"))



;;;; Configfile Parsing

;;; data structure to hold a commandbinding
(define-structure midi cmd ch param func last-value)

;;; returns a table(k,v) where: k := (ev-id t i); v = '(<instance[s] of midi> ... )
(define (parse-config path)


  ;; returns a (lambda (joystick-input-val) (...)) function based on MIDI Command found in midi-conf (which is a midi structure)
  (define (midi-func-dispatch midi-conf)

    ;; constant values needed by this function


    (define *MIDI-CMD-Note-Off* #x80)
    (define *MIDI-CMD-Note-On* #x90)
    (define *MIDI-CMD-Continus-Controller* #xB0)
    (define *MIDI-CMD-Patch-Change* #xC0)
    (define *MIDI-CMD-Channel-Pressure* #xD0)
    (define *MIDI-CMD-Pitch-Bend* #xE0)
    (define *MIDI-CMD-SYSEX* #xF0)
    


    ;; lambda function creators
    
    (define (build-midi-cmd-byte midi-conf)
      (cond ((equal? (midi-cmd midi-conf) #xF0) #xF0)
	    (else (bitwise-ior (midi-cmd midi-conf) (midi-ch midi-conf)))))

    (define (build-msg-input-last midi-conf)
      (lambda (input-val) `(,(build-midi-cmd-byte midi-conf) ,@(midi-param midi-conf) ,(transpose-to-midi input-val))))

    (define (build-msg-input-first midi-conf)
      (lambda (input-val) `(,(build-midi-cmd-byte midi-conf) ,(transpose-to-midi input-val) ,@(midi-param midi-conf))))

    (define (build-msg-no-input midi-conf)
      (lambda (input-val) `(,(build-midi-cmd-byte midi-conf) ,@(midi-param midi-conf))))

    (define (build-msg-no-param midi-conf)
      (lambda (input-val) `(,(build-midi-cmd-byte midi-conf) ,(transpose-to-midi input-val))))

    (define (build-msg-note-on midi-conf)
      (lambda (input-val) `(,(build-midi-cmd-byte midi-conf) ,(+ (if (null? (midi-param midi-conf)) 0 (car (midi-param midi-conf)))
								 (transpose-to-midi input-val)) #x7F)))

    (define (build-msg-note-off midi-conf)
      (lambda (input-val) `(,(build-midi-cmd-byte midi-conf) ,(+ (if (null? (midi-param midi-conf)) 0 (car (midi-param midi-conf)))
								 (transpose-to-midi (midi-last-value midi-conf))) #x7F)))

    
    (define (build-msg-pitch-bend midi-conf)
      (let ((split-input-val (lambda (input-val)
			       (cons (bitwise-and input-val *MIDI-VAL-MAX*)
				     (cons (bitwise-and (arithmetic-shift input-val -7) *MIDI-VAL-MAX*) '())))))
	(lambda (input-val) `(,(build-midi-cmd-byte midi-conf)
			      ,@(split-input-val (transpose-to-pitch-bend  input-val))))))

    ;; dispatcher
    (let ((cmd (midi-cmd midi-conf)))
      (cond ((equal? cmd *MIDI-CMD-Continus-Controller*) (build-msg-input-last midi-conf))
	    ((equal? cmd *MIDI-CMD-Patch-Change*) (build-msg-no-input midi-conf))
	    ((equal? cmd *MIDI-CMD-Channel-Pressure*) (build-msg-no-param midi-conf))
	    ((equal? cmd *MIDI-CMD-Pitch-Bend*) (build-msg-pitch-bend midi-conf))
	    ((equal? cmd *MIDI-CMD-SYSEX*) (build-msg-no-input midi-conf))
	    ((equal? cmd *MIDI-CMD-Note-On*) (build-msg-note-on midi-conf))
	    ((equal? cmd *MIDI-CMD-Note-Off*) (build-msg-note-off midi-conf))
	    (else (lambda (input-val) '() )))))



  ;; short helper to create an instance of midi structure based on commandbinding
  (define (create-midi-struct command-binding)
    (let* ((cmd-ch (car command-binding))
	   (midi (make-midi (car cmd-ch)
			    (if (pair? (cdr cmd-ch)) (cadr cmd-ch))
			    (cdr command-binding)
			    (lambda (input-val) '())
			    0)))
      (midi-func-set! midi (midi-func-dispatch midi))
      midi))

  ;; traverse a list of commandbindings and returns a list of midi instances
  (define (create-list-of-midi-structs command-bindings)
    (if (null? command-bindings) command-bindings
	(cons (create-midi-struct (car command-bindings)) (create-list-of-midi-structs (cdr command-bindings)))))

  
  (let ((config-list (call-with-input-file path read))
	(config-table (make-table)))
    ;; creates a table entry [k:(ev-id t i), v:'(<instance[s] of midi>) based on an configuration entry
    (define (make-entry entry)
      (let* ((id (car entry))
	     (command-bindings (cdr entry))
	     (t (case (car id)
		  ((a A axis Axis) *JS-EVENT-AXIS* )
		  ((b B button Button) *JS-EVENT-BUTTON*)
		  (else #f)))
	     (i (cadr id)))
	;; (display command-bindings) (newline)
	(if (equal? t #f) (begin (display  "failed to parse line: ") (display entry) (newline)) 
	    (table-set! config-table (ev-id t i) (create-list-of-midi-structs command-bindings)))))

    ;; loop traversing config entries
    (let loop ((config-list config-list))
      (make-entry (car  config-list))
      (if (not (null? (cdr config-list)))
	  (loop (cdr config-list))))
    config-table))


;;; returns a list of complete midi messages
(define (get-midi-msgs config-table current-event)

  ;; predicate if combination of type and value equals a button-pressed event
  (define (button-pressed? current-event)
    (if (and (js-event-is-button? current-event) (equal? (js-event-value current-event) *BUTTON-PRESSED*)) #t #f))

  ;; returns single element list of '(lambda (input-val) (...)), rotates stored midi instances in config-table left by one
  (define (build-button-command-list)
    (let*  ((event-id (js-event-ev-id current-event))
	    (midi-lst (table-ref config-table event-id (list (make-midi #x00 #x00 '() (lambda (x) '()) #f)))))
      (table-set! config-table event-id (rotate-left midi-lst)) ; this is a sideeffect
      (cons ((midi-func (car midi-lst))
	     (if (button-pressed? current-event)
		 #x7F
		 (js-event-value current-event)))
	    (list '()))))

  ;; returns list of midi messages where the deadzone is applied to event-value and sets the current event-value as last-value after that
  (define (build-axis-command-list)
    (letrec ((event-id (js-event-ev-id current-event))
	     (build (lambda (lst) (if (null? lst) lst
				      (cons ((midi-func (car lst)) (transpose-to-deadzone (js-event-value current-event)))
					    (build (cdr lst))))))
	     (midi-instances (table-ref config-table event-id (list (make-midi #x00 #x00 '() (lambda (x) '()) #f))))
	     (res (build midi-instances)) ; save result of midi messages
	     (update-last-value (lambda (midi-instance) (midi-last-value-set! midi-instance (transpose-to-deadzone (js-event-value current-event))))))
      ;; apply the current event-value as last-value, this is a sideeffect
      (let update-last-values-loop ((midi-instances midi-instances))
	(update-last-value (car midi-instances)) 
	(if (not (null? (cdr midi-instances)))
	    (update-last-values-loop (cdr midi-instances))))
      res))


  
  (cond ((js-event-is-axis? current-event) (build-axis-command-list))
	((button-pressed? current-event) (build-button-command-list))
	(else (list'()))))



;;;; MAIN

;;; helper which prints the usage
(define (print-usage name)
  (print "Usage: " name " [options]\n")
  (print "Version: " *VERSION* "\n")
  (print "Options:\n")
  (print "  -h     \tprint this information\n")
  (print "  -j path\tjoystick device\n")
  (print "  -c path\tconfiguration file\n")
  (print "  -d uint\tdeadzone radius for axes\n")
  (print "  -v     \tprint midi messages\n"))


;;; the main thingy
(let* ((args (command-line))
       (js-file-flag (member "-j" args))
       (conf-file-flag (member "-c" args))
       (deadzone-flag (member "-d" args))
       (verbose-flag (member "-v" args)))
  ;; (display args) (newline)

    (if (member "-h" args)
	(print-usage (car args))
	(begin
  
	  (let* ((fd-joy (if (and js-file-flag (> (length js-file-flag) 1))
			    (open-joystick (cadr js-file-flag))
			    (open-joystick "/dev/input/js0")))
		(get-js-event (get-js-event-function fd-joy))
		(config (if (and conf-file-flag (> (length conf-file-flag) 1))
			    (parse-config (cadr conf-file-flag))
			    (parse-config "./input.conf")))

		(last-input-table (make-table))
		(make-dummy-js-event (lambda (ev-id) (make-js-event ev-id 0 0 0 #f #f #f)))
		(compare-js-event (lambda (ev1 ev2) (let ((ev-val1 (transpose-to-deadzone (js-event-value ev1)))
							  (ev-val2 (transpose-to-deadzone (js-event-value ev2))))
						      ;(print ev-val2 " " ev-val1) (newline)
						      (if (and (js-event-valid ev1) (js-event-valid ev2)
							       (equal? ev-val2 ev-val1))
							  #t
							  #f)))))
  
	    (deadzone (if (and deadzone-flag (> (length deadzone-flag) 1))
				    (string->number (cadr deadzone-flag))
				    0)) 
	    (setup-jack)
	    (let mainloop ()
	      (let ((current-event (get-js-event)))
;		(print-js-event current-event)
		(if (equal? (js-event-valid current-event) #t) ; test if we got a valid js_event
		    (let* ((event-id (js-event-ev-id current-event))
			   (last-input (table-ref last-input-table event-id (make-dummy-js-event event-id))))
;		      (display last-input) (newline) (display current-event) (newline) (newline)
		      (if (or (js-event-is-button? current-event) (not (compare-js-event current-event last-input)))
			  (let ((midi-list (get-midi-msgs config current-event)))
			    (let sendloop ((midi-list midi-list))
			      (let* ((msg  (car midi-list))
				     (msg-length (length msg)))
				(if (not (equal? msg-length 0))
				    (begin
				      (if verbose-flag (begin (display msg) (newline)))
				      (send-midi msg (length msg)))))
			      (if (not (null? (cdr midi-list)))
				  (sendloop (cdr midi-list))))
			    (table-set! last-input-table event-id current-event))))
		    
		    (display "failed to get event\n")))
	      (mainloop))

	    (close-joystick fd-joy)))))
