/*
  LispBox LispLibrary - Version 1.1 - Jan 2025
  Hartmut Grawe - github.com/ersatzmoco - Jan 2025

  Some parts based on:
  ErsatzMoco microcontroller framework - github.com/ersatzmoco/ersatzmoco
  	Hartmut Grawe - github.com/ersatzmoco - July 2020
  	Please see there for further explanations on how to use the framework.
  M5Cardputer editor version by hasn0life - Nov 2024 - https://github.com/hasn0life/ulisp-sedit-m5cardputer

  This uLisp version licensed under the MIT license: https://opensource.org/licenses/MIT
*/



    // MOCO-COMMANDS - not implemented as Lisp constants to save memory
    // following commentary sections just for reference - just use the decimal values and write code name into comment
    
    
    // EM_ACK = 0                        #!< Acknowledge received command

    // EM_REC_PING = 1                   #!< Wait for ping signal
    // EM_SEND_PING = 2                  #!< Send ping signal

    // EM_ACT_DCM_SPEED = 3              #!< DC motor: Set speed
    // EM_ACT_DCM_STOP = 4               #!< DC motor: Stop motor
    // EM_ACT_DCM_BRAKE = 5              #!< DC motor: force stop
    // EM_ACT_DCM_CW = 6                 #!< DC motor: set direction to clockwise
    // EM_ACT_DCM_CCW = 7                #!< DC motor: set direction to counterclockwise

    // EM_ACT_DOUT_HI = 8                #!< Set the indicated digital output pin to high state
    // EM_ACT_DOUT_LO = 9                #!< Set the indicated digital output pin to low state
    // EM_ACT_DOUT_INPUT = 10            #!< Set the indicated digital pin to input state
    // EM_ACT_DOUT_OUTPUT = 11           #!< Set the indicated digital pin to output state

    // EM_ACT_AOUT_VAL = 12              #!< Set the indicated analog pin to a value - see descendants of base type Actuator how to use this command

    // EM_ACT_PWM_VAL = 13               #!< Set the indicated analog pin to a pwm value - see descendants of base type Actuator how to use this command

    // EM_ACT_AUDIO_TRACK = 14           #!< Audio module: Select track to play
    // EM_ACT_AUDIO_PLAY = 15            #!< Audio module: Play selected track
    // EM_ACT_AUDIO_STOP = 16            #!< Audio module: Set volume
    // EM_ACT_AUDIO_VOL = 17             #!< Audio module: Stop playback

    // EM_ACT_SERVO_ANGLE = 18           #!< Servo: Set angle

    // EM_ACT_SERVOMOTOR_SPEED = 19      #!< Continuous servo: Set speed

    // EM_DISP_MSG = 20                  #!< Display a message string
    // EM_DISP_CHAR = 21                 #!< Display an ascii character indicated by an int number

    // EM_ACT_STM_STEP = 22              #!< Stepper motor: Execute one step
    // EM_ACT_STM_CW = 23                #!< Stepper motor: Set direction to clockwise
    // EM_ACT_STM_CCW = 24               #!< Stepper motor: Set direction to counterclockwise
    // EM_ACT_STM_SET_DIST = 25          #!< Stepper motor: Set distance to move the object driven by the stepper
    // EM_ACT_STM_SET_TIME = 26          #!< Stepper motor: Set time frame for covering the currently set distance
    // EM_ACT_STM_START = 27             #!< Stepper motor: Start motion from current position to distance in selected time frame
    // EM_ACT_STM_STOP = 28              #!< Stepper motor: Stop the motor within the deceleration period set in the StepperUnit header
    // EM_ACT_STM_EMERGENCY_STOP = 29    #!< Stepper motor: Stop motor immediately. CAUTION: This may damage the motor!
    // EM_ACT_STM_ENABLE = 30            #!< Stepper motor: Set driver to "enabled", i.e. connect power and force motor to maintain current position
    // EM_ACT_STM_DISABLE = 31           #!< Stepper motor: Set driver to "disbled", i.e. disconnect power. Motor may be moved by external forces.

    // EM_ACT_RELAY_OFF = 32             #!< Relay: Open contact
    // EM_ACT_RELAY_ON = 33              #!< Relay: Close contact

    // EM_RMT_SIGNAL = 40                #!< RemoteAction: Send Signal and require acknowledge
    // EM_RMT_SIGNAL_CANCEL = 41         #!< RemoteAction: Cancel sent signal
    // EM_RMT_REMOTE_TOGGLE = 42         #!< RemoteAction: Switch on remote function
    // EM_RMT_REMOTE_PULSE = 43          #!< RemoteAction: Switch off remote function
    // EM_RMT_SIGNAL_ACK = 44            #!< RemoteAction: Acknowledge signal


const char LispLibrary[] PROGMEM = R"lisplibrary(

#| SET THE FOLLOWING VARIABLE TO NIL TO DEACTIVATE LISPY LITTLE HELPER |#

(defvar se:help-active t)

;
; Extended ULOS functions
;
;
; Define a class
(defun class (&optional parent slots constructor)
	#.(format nil "(class [parent] [slotlist] [constructor t/nil])~%ULOS function to define a new class or instance of a class.~%To define a new class, pass it to a variable, adding a list of property/method slots according to ULOS mechanism, optionally passing a parent class to inherit from.~%To define an instance, pass a parent class (= the prototype), omit the slots and set constructor to t.~%That way, the slots of the parent class are automatically copied into the instance.~%Setting the slots of the parent is equivalent to setting class variables, then.")
	(let ((obj (when parent (list (cons 'parent parent))))) 	
	 	(when (and constructor parent)
  		(when (symbolp parent) (setq parent (eval parent)))
  		(loop
	     (when (null parent) (return parent))
	     (unless (or (equal (search "_" (string (first parent))) 1) (search "parent" (string (first parent))) )
	      (push (cons (car (first parent)) (cdr (first parent))) obj))
	     (setq parent (cdr parent)))
  	)
    (loop
     (when (null slots) (return obj))
     (push (cons (first slots) (second slots)) obj)
     (setq slots (cddr slots)))
  )
)

; Get the value of a property slot in an instance/class or its parents
(defun gtv (obj slot)
	#.(format nil "(gtv obj 'slot)~%ULOS function to get value of property slot in obj.")
  (when (symbolp obj) (setq obj (eval obj)))
  (let ((pair (assoc slot obj)))
    (if pair (cdr pair)
           (let ((p (cdr (assoc 'parent obj))))
             (and p (gtv p slot))))
  )
)

; Update a property in an instance/class
(defun stv (obj slot value)
	#.(format nil "(stv obj 'slot value)~%ULOS function to set value of property slot in obj.")
  (when (symbolp obj) (setq obj (eval obj)))
  (let ((pair (assoc slot obj)))
    (when pair (setf (cdr pair) value))
  )
)

; Add property and method slots
(defun adp (obj slots)
	#.(format nil "(adp 'obj slotlist)~%ULOS function to add new property slots to obj, similar to JavaScript mechanism.")
	(let (newlist) 
    (loop
     (when (null slots) (return))
     (push (cons (first slots) (second slots)) newlist)
     (setq slots (cddr slots)))
    (set obj (append (eval obj) newlist)) 
  )
)

; Call a method in an object instance
;
(defun cmt (obj method &rest arguments) 
	(apply (eval (gtv obj method)) (append (list obj) arguments))
)


;
; RGB helper function
;
;
(defun rgb (r g b)
  (logior (ash (logand r #xf8) 8) (ash (logand g #xfc) 3) (ash b -3))
)


;
; LispBox screen editor
;
;
(defun se:init (sk)
	(case sk 
		(t 
		 (defvar se:code_col (rgb 255 255 255))
		 (defvar se:line_col (rgb 90 90 90))
		 (defvar se:border_col (rgb 63 40 0))
		 (defvar se:bg_col (rgb 0 0 0))
		 (defvar se:cursor_col (rgb 160 60 0))
		 (defvar se:emph_col (rgb 0 128 0))
		 (defvar se:alert_col (rgb 255 0 0))
		 (defvar se:input_col (rgb 255 255 255))

		 (defvar se:help_col (rgb 255 127 0))
		)
	)

	(defvar se:origin (cons 34 18))
	(defvar se:txtpos (cons 0 0))
	(defvar se:txtmax (cons 94 26))
	(defvar se:offset (cons 0 0))
	(defvar se:scrpos (cons 0 0))
	(defvar se:lastc nil)
	(defvar se:funcname nil)
	(defvar se:filename nil)
	(defvar se:suffix nil)
	(defvar se:buffer ())
	(defvar se:curline "")
	(defvar se:tscale 1)
	(defvar se:leading (* 16 se:tscale))
	(defvar se:cwidth (* 8 se:tscale))
	(defvar se:openings ())
	(defvar se:closings ())
	(defvar se:lastmatch ())
	(defvar se:match nil)
	(defvar se:exit nil)

	(fill-screen)
	(set-cursor 0 0)
	(set-text-color se:code_col se:bg_col)

	(tft1-graphics-mode)
	(tft1-set-scroll-win 0 0 800 480)
	(tft1-set-scrollX 0)
	(tft1-set-scrollY 0)
	(tft1-fill-screen)
	(tft1-draw-line 33 17 33 451 se:border_col)
	(tft1-draw-line 0 451 799 451 se:border_col)
	(tft1-text-mode)
	#| (tft1-text-enlarge (1- se:tscale)) |#
	(tft1-set-cursor 0 0)
	(tft1-set-text-color se:bg_col se:cursor_col)
	(if se:help-active
		(tft1-write-text "F1/F2 chk () | F3 help | F5 bind |                          | F9 del | F10 save | F11 load | F12 dir")
		(tft1-write-text "F1/F2 chk () | F5 bind |                                    | F9 del | F10 save | F11 load | F12 dir")
	)
)

(defun se:cleanup ()
	(makunbound 'se:buffer)
	(makunbound 'se:openings)
	(makunbound 'se:closings)
	(makunbound 'se:curline)
	(gc)
)

(defun se:hide-cursor ()
	(when se:lastmatch
		(let ((spos nil) (bpos (car se:lastmatch)) (br (cdr se:lastmatch)))
			(setf spos (se:calc-scrpos bpos))
			(tft1-set-cursor (car spos) (cdr spos)) 
			(tft1-set-text-color se:code_col se:bg_col)
			(tft1-write-text (string br))
			(setf se:lastmatch nil)
		)
	)
	(when se:lastc 
		(tft1-set-cursor (car se:scrpos) (cdr se:scrpos)) 
		(tft1-set-text-color se:code_col se:bg_col)
		(tft1-write-text (string se:lastc))
		(tft1-set-cursor 0 (cdr se:scrpos))
		(tft1-set-text-color se:line_col)
		(tft1-write-text (string (1+ (cdr se:txtpos))))
	)
)
		
(defun se:show-cursor (&optional forceb)
	(let ((x (car se:txtpos))
		  (y (cdr se:txtpos))
		  (ox (car se:offset))
		  (oy (cdr se:offset))
		  (padx (car se:origin))
		  (pady (cdr se:origin))
		  (myc (code-char 32)))
		(setf se:curline (nth y se:buffer))
		(setf se:scrpos (se:calc-scrpos se:txtpos))
		(tft1-set-cursor (car se:scrpos) (cdr se:scrpos))
		(tft1-set-text-color se:code_col se:cursor_col)
		#| check if cursor is within line string or behind last char |#
		(when (< x (length se:curline)) (setf myc (char se:curline x)))
		(setf se:lastc myc)
		(if forceb
			(progn
				(setf se:match t)
				(se:write-char (char-code myc))
				(setf se:match nil)
			)
			(se:write-char (char-code myc))
		)
		(tft1-set-cursor 0 (cdr se:scrpos))
		(tft1-set-text-color se:cursor_col)
		(tft1-write-text (string (1+ y)))
	)
)

(defun se:toggle-match ()
	(keyboard-flush)
	(if se:match
		(progn
			(setf se:match nil) (setf se:openings ()) (setf se:closings ())
			(tft1-set-cursor 0 0)
			(tft1-set-text-color se:bg_col se:cursor_col)
			(tft1-write-text "F1")
			(keyboard-flush)
		)
		(progn
			(setf se:match t)
			(tft1-set-cursor 0 0)
			(tft1-set-text-color se:bg_col se:emph_col)
			(tft1-write-text "F1")
			(se:hide-cursor)
			(se:map-brackets)
			(keyboard-flush)
			(se:show-cursor)
		)
	)
)

(defun se:checkbr ()
	(keyboard-flush)
	(se:hide-cursor)
	(se:map-brackets t)
	(se:show-cursor t)
	(setf se:openings ())
	(setf se:closings ())
	(setf se:match nil)
	(tft1-set-cursor 0 0)
	(tft1-set-text-color se:bg_col se:cursor_col)
	(tft1-write-text "F1")
	(keyboard-flush)
)

(defun se:map-brackets (&optional forcemp)
	(when (or se:match forcemp)
		(let ((myline "") (keys ()) (octr 0) (bl (length se:buffer)))
			(dotimes (y bl)
				(setf myline (nth y se:buffer))
				(dotimes (x (length myline))
					(when (equal (char-code (char myline x)) 40) (push (cons (cons x y) octr) se:openings) (push octr keys) (incf octr))
					(when (equal (char-code (char myline x)) 41) (push (cons (cons x y) (if keys (pop keys) nil)) se:closings))
				)
			)
		)
	)
)

(defun se:find-closing-bracket ()
	(let ((opentry (assoc* se:txtpos se:openings #'equal)) (clentry nil))
		(if opentry
			(progn
				(setf clentry (reverse-assoc* (cdr opentry) se:closings #'equal))
				(if clentry
					clentry
					nil
				)
			)
			nil
		)
	)
)

(defun se:find-opening-bracket ()
	(let ((clentry (assoc* se:txtpos se:closings #'equal)) (opentry nil))
		(if clentry
			(progn
				(setf opentry (reverse-assoc* (cdr clentry) se:openings #'equal))
				(if opentry
					opentry
					nil
				)
			)
			nil
		)
	)
)

(defun se:in-window (pos)
	(if (and (>= (car pos) (car se:offset)) (<= (car pos) (+ (car se:offset) (car se:txtmax))))
		(if (and (>= (cdr pos) (cdr se:offset)) (<= (cdr pos) (+ (cdr se:offset) (cdr se:txtmax))))
			t
			nil
		)
		nil
	)
)

(defun se:move-window (&optional forceshow)
	(let ((x (car se:txtpos))
		  (y (cdr se:txtpos))
		  (ox (car se:offset))
		  (oy (cdr se:offset))
		  (xm (car se:txtmax))
		  (ym (cdr se:txtmax)))
		(when (> x (+ ox xm))
			(setf (car se:offset) (+ (car se:offset) (- x (+ ox xm))))
			(setf forceshow t)
		)
		(when (> y (+ oy ym))
		    (setf (cdr se:offset) (+ (cdr se:offset) (- y (+ oy ym))))
			(setf forceshow t)
		)
		(when (< x ox)
			(setf (car se:offset) (- (car se:offset) (- ox x)))
			(setf forceshow t)
		)
		(when (< y oy)
			(setf (cdr se:offset) (- (cdr se:offset) (- oy y)))
			(setf forceshow t)
		)
		(when forceshow
			(se:show-text)
		)
	)
)

(defun se:calc-scrpos (tpos)
	(let ((sx 0) (sy 0) (ox (car se:offset)) (oy (cdr se:offset)) (padx (car se:origin)) (pady (cdr se:origin)))
		(setf sx (+ (* (- (car tpos) ox) se:cwidth) padx))
		(setf sy (+ (* (- (cdr tpos) oy) se:leading) pady))
		(cons sx sy)
	)
)

(defun se:calc-msgpos (tpos)
	(let ((sx 0) (sy 0) (padx (car se:origin)) (pady (cdr se:origin)))
		(setf sx (+ (* (car tpos) se:cwidth) padx))
		(setf sy (+ (* (cdr tpos) se:leading) pady))
		(cons sx sy)
	)
)

(defun se:write-char (cc)
	(let ((bpos nil) (spos nil))
		(cond
			((and (= cc 40) se:match)
				(setf bpos (se:find-closing-bracket))
				(when bpos
					(when (se:in-window bpos)
						(setf spos (se:calc-scrpos bpos))
						(tft1-set-cursor (car spos) (cdr spos))
						(tft1-set-text-color se:code_col se:emph_col)
						(tft1-write-text ")")
						(setf se:lastmatch (cons bpos ")"))
					)
					(tft1-set-text-color se:code_col se:emph_col)
				)
				(tft1-set-cursor (car se:scrpos) (cdr se:scrpos))
				(tft1-write-text (string (code-char cc)))
				(tft1-set-text-color se:code_col se:bg_col)
			)
			((and (= cc 41) se:match)
				(setf bpos (se:find-opening-bracket))
				(when bpos
					(when (se:in-window bpos)
						(setf spos (se:calc-scrpos bpos))
						(tft1-set-cursor (car spos) (cdr spos))
						(tft1-set-text-color se:code_col se:emph_col)
						(tft1-write-text "(")
						(setf se:lastmatch (cons bpos "("))
					)
					(tft1-set-text-color se:code_col se:emph_col)
				)
				(tft1-set-cursor (car se:scrpos) (cdr se:scrpos))
				(tft1-write-text (string (code-char cc)))
				(tft1-set-text-color se:code_col se:bg_col)
			)
			(t (tft1-write-text (string (code-char cc))))
		)
	)
)

(defun se:disp-line (y)
	(let ((ypos (+ (cdr se:origin) (* (- y (cdr se:offset)) se:leading))) (myl " "))
		(when (nth y se:buffer) (setf myl (concatenate 'string (nth y se:buffer) myl)))
		(tft1-set-text-color se:line_col)
		(tft1-set-cursor 0 ypos)
		(tft1-write-text (string (1+ y)))

		(tft1-set-cursor (car se:origin) ypos)
		(tft1-set-text-color se:code_col se:bg_col)
		(when (> (length myl) (car se:offset))
			(tft1-write-text (subseq myl (car se:offset) (min (length myl) (+ (car se:txtmax) (car se:offset) 1))))
		)
	)
)

(defun se:show-text ()
	(tft1-fill-rect 34 18 763 432 se:bg_col)
	(tft1-fill-rect 0 18 33 432 se:bg_col)
	(let ((i 0) (ymax (min (cdr se:txtmax) (- (length se:buffer) (cdr se:offset) 1))))
		(loop
			(se:disp-line (+ i (cdr se:offset)))
			(when (= i ymax) (return))
			(incf i)
		)
	)
)

(defun se:show-dir ()
	(keyboard-flush)
	(se:hide-cursor)
	(tft1-fill-rect 34 18 763 432 se:bg_col)
	(tft1-fill-rect 0 18 33 432 se:bg_col)
	(tft1-set-text-color se:line_col)
	(let ((spos (se:calc-scrpos (cons 0 0))))
		(tft1-set-cursor (car spos) (cdr spos))
		(tft1-write-text "/")
	)
	(let ((dirbuf (sd-card-dir 2)))
		(se:compile-dir dirbuf (cons 3 1))
	)
	(loop
		(when (keyboard-get-key t) (return))
	)
	(keyboard-flush)
	(se:show-text)
	(se:show-cursor)
)

(defun se:compile-dir (mydir dpos)
	(let ((spos nil))
		(loop
			(when (null mydir) (return))
			(let ((entry (pop mydir)))
				(if (listp entry)
					(se:compile-dir entry (cons (+ 3 (car dpos)) (cdr dpos)))
					(unless (or (> (car dpos) (- (car se:txtmax) (length entry))) (> (cdr dpos) (cdr se:txtmax)))
						(setf spos (se:calc-scrpos dpos))
						(if (search "/" entry)
							(tft1-set-text-color se:line_col)
							(tft1-set-text-color se:code_col)
						)
						(tft1-set-cursor (car spos) (cdr spos))
						(tft1-write-text entry)
						(incf (cdr dpos))
					)
				)
			)
		)
	)
)

(defun se:flush-buffer ()
	(keyboard-flush)
	(when (se:alert "Flush buffer")
		(se:hide-cursor)
		(setq se:buffer (list ""))
		(setf se:txtpos (cons 0 0))
		(setf se:offset (cons 0 0))
		(se:show-text)
		(se:show-cursor)
		(keyboard-flush))
)

(defun se:flush-line ()
	(keyboard-flush)
	(se:hide-cursor)
	(let* ((x (car se:txtpos))
	   (y (cdr se:txtpos))
	   (myl se:curline)
	   (firsthalf ""))
		(progn
			(setf firsthalf (subseq myl 0 x))
			(setf (nth y se:buffer) firsthalf)
			(setf se:curline (concatenate 'string (nth y se:buffer) " "))
			(setf se:lastc nil)
			(se:disp-line y)
		)
	)
	(se:map-brackets)
	(se:show-text)
	(se:show-cursor)
	(keyboard-flush)
)

(defun se:insert (newc)
	(se:hide-cursor)
	(let* ((x (car se:txtpos))
		   (y (cdr se:txtpos))
		   (myl se:curline)
		   (firsthalf "")
		   (scdhalf ""))
		(setf firsthalf (subseq myl 0 x))
		(setf scdhalf (subseq myl x (length myl)))
		(setf (nth y se:buffer) (concatenate 'string firsthalf (string newc) scdhalf))
		(incf (car se:txtpos))
		(setf se:curline (nth y se:buffer))
		(setf se:lastc nil)
		(if (> (car se:txtpos) (car se:txtmax)) (se:move-window) (se:disp-line y))
	)
	(se:map-brackets)
	(se:show-cursor)
)

(defun se:tab ()
	(keyboard-flush)
		(se:insert #\032)
		(se:insert #\032)
		(se:insert #\032)
		(se:insert #\032)
	(keyboard-flush)
)

(defun se:enter ()
	(se:hide-cursor)
	(let* ((x (car se:txtpos))
		   (y (cdr se:txtpos))
		   (myl se:curline)
		   (firsthalf "")
		   (scdhalf "")
		   (newl () ))
		(setf firsthalf (subseq myl 0 x))
		(setf scdhalf (subseq myl x (length myl)))
		(dotimes (i y)
			(push (nth i se:buffer) newl)
		)
		(push firsthalf newl)
		(push scdhalf newl)
		(dotimes (i (- (length se:buffer) (1+ y)))
			(push (nth (+ i (1+ y)) se:buffer) newl)
		)
		(setf se:buffer (reverse newl))
		(setf (car se:txtpos) 0)
		(incf (cdr se:txtpos))
		(setf se:curline (nth (1+ y) se:buffer))
		(setf se:lastc nil)
		(setf (car se:offset) 0)
		(se:move-window t)
	)
	(se:map-brackets)
	(se:show-cursor)
)

(defun se:delete ()
	(se:hide-cursor)
	(let* ((x (car se:txtpos))
		   (y (cdr se:txtpos))
		   (myl se:curline)
		   (firsthalf "")
		   (scdhalf ""))
		(if (> x 0)
			(progn
				(setf firsthalf (subseq myl 0 (1- x)))
				(setf scdhalf (subseq myl x (length myl)))
				(setf (nth y se:buffer) (concatenate 'string firsthalf scdhalf))
				(setf se:curline (concatenate 'string (nth y se:buffer) " "))
				(decf (car se:txtpos))
				(setf se:lastc nil)
				(se:disp-line y)
			)
			(when (> y 0)
				(setf scdhalf se:curline)
				(setf se:buffer (remove y se:buffer))
				(decf (cdr se:txtpos))
				(decf y)
				(setf se:curline (nth y se:buffer))
				(setf firsthalf se:curline)
				(if firsthalf
					(progn
						(setf (nth y se:buffer) (concatenate 'string firsthalf scdhalf))
						(setf (car se:txtpos) (length firsthalf))
					)
					(progn
						(setf (nth y se:buffer) scdhalf)
						(setf (car se:txtpos) 0)
					)
				)
				(se:move-window t)
			)
		)
	)
	(se:map-brackets)
	(se:show-cursor)
)

(defun se:left ()
	(se:hide-cursor)
	(cond
		#| xpos > 0 |#
		((> (car se:txtpos) 0) 
			(decf (car se:txtpos))
			(se:move-window)
		)
		#| xpos == 0, but ypos > 0 |#
		((> (cdr se:txtpos) 0)
			(decf (cdr se:txtpos))
			(setf (car se:txtpos) (length (nth (cdr se:txtpos) se:buffer)))
			(se:move-window)
		)
	)
	(se:show-cursor)
)

(defun se:right ()
	(se:hide-cursor)
	(cond
		#| xpos < eol |#
		((< (car se:txtpos) (length se:curline)) 
			(incf (car se:txtpos))
			(se:move-window)
		)
		#| xpos == eol, but ypos < end of se:buffer |#
		((< (cdr se:txtpos) (1- (length se:buffer)))
			(incf (cdr se:txtpos))
			(setf (car se:txtpos) 0)
			(se:move-window)
		)
	)
	(se:show-cursor) 
)

(defun se:up ()
	(se:hide-cursor)
	(cond
		#| ypos > 0 |#
		((> (cdr se:txtpos) 0) 
			(decf (cdr se:txtpos))
			(setf se:curline (nth (cdr se:txtpos) se:buffer))
			(when (> (car se:txtpos) (length se:curline)) 
				(setf (car se:txtpos) (length se:curline))
			)
			(se:move-window)
		)
	)
	(se:show-cursor)
)

(defun se:down ()
	(se:hide-cursor)
	(cond
		#| ypos < length of se:buffer |#
		((< (cdr se:txtpos) (1- (length se:buffer))) 
			(incf (cdr se:txtpos))
			(setf se:curline (nth (cdr se:txtpos) se:buffer))
			(when (> (car se:txtpos) (1+ (length se:curline))) (setf (car se:txtpos) (length se:curline)))
			(se:move-window)
		)
	)
	(se:show-cursor)
)

(defun se:linestart ()
	(se:hide-cursor)
	(setf (car se:txtpos) 0)
	(se:move-window)
	(se:show-cursor)
)

(defun se:lineend ()
	(se:hide-cursor)
	(setf (car se:txtpos) (length se:curline))
	(se:move-window)
	(se:show-cursor)
)

(defun se:nextpage ()
	(se:hide-cursor)
	(setf (cdr se:txtpos) (min (length se:buffer) (+ (cdr se:txtpos) (cdr se:txtmax) 1)))
	(se:move-window)
	(se:show-cursor)
)

(defun se:prevpage ()
	(se:hide-cursor)
	(setf (cdr se:txtpos) (max 0 (- (cdr se:txtpos) (cdr se:txtmax) 1)))
	(se:move-window)
	(se:show-cursor)
)

(defun se:docstart ()
	(se:hide-cursor)
	(setf se:txtpos (cons 0 0))
	(se:move-window)
	(se:show-cursor)
)

(defun se:run ()
	(let ((body "") (fname (se:input "Symbol name: " se:funcname 60)))
		(mapc (lambda (x) (setf body (concatenate 'string body x))) se:buffer)
		(if fname
			(when (se:alert (concatenate 'string "Bind code to symbol " fname " "))
				(eval (read-from-string (concatenate 'string (format nil "(defvar ~a" fname) (format nil " '~a)" body))))
				(se:msg "Done! Returning to REPL")
				(delay 2000)
				(se:clr-msg)
				(se:cleanup)
				(setf se:exit t)
			)
			(eval (read-from-string body))
		)
	)
)

(defun se:remove ()
	(let ((fname (se:input "DELETE file name (UPPERCASE only): " nil 8 t)) (suffix (se:input "Suffix: ." "CL" 3 t)))
		(if (sd-file-exists (concatenate 'string fname "." suffix))
			(when (se:alert (concatenate 'string "Delete file " fname "." suffix))
				(sd-file-remove (concatenate 'string fname "." suffix))
				(se:msg "Done! Returning to editor.")
				(delay 1000)
				(se:clr-msg)
			)
		)
	)

)

(defun se:save ()
	(unless se:suffix (setf se:suffix "CL"))
	(let ((fname (se:input "SAVE file name (UPPERCASE only): " se:filename 8 t)) (suffix (se:input "Suffix: ." se:suffix 3 t)) (overwrite t))
		(if (sd-file-exists (concatenate 'string fname "." suffix))
			(unless (se:alert "File exists. Overwrite")
				(setf overwrite nil)
			)
		)
		(unless (or (< (length fname) 1) (< (length suffix) 1) (not overwrite))
			(with-sd-card (strm (concatenate 'string fname "." suffix) 2)
				(dolist (line se:buffer)
					(princ line strm)
					(princ (code-char 10) strm)
				)
			)
			(tft1-set-cursor (* 36 se:cwidth) 0)
			(tft1-set-text-color se:code_col se:cursor_col)
			(setf se:filename fname)
			(setf se:suffix suffix)
			(tft1-write-text (concatenate 'string "FILE: " fname "." suffix "       "))
			(se:msg "Done! Returning to editor.")
			(delay 1000)
			(se:clr-msg)
		)
	)
)

(defun se:load ()
	(when (se:alert "Discard buffer and load from SD")
		(let ((fname (se:input "LOAD file name (UPPERCASE only): " nil 8 t)) (suffix (se:input "Suffix: ." "CL" 3 t)) (line ""))
			(unless (or (< (length fname) 1) (< (length suffix) 1) (not (sd-file-exists (concatenate 'string fname "." suffix))))
				(setq se:buffer ())
				(with-sd-card (strm (concatenate 'string fname "." suffix) 0)
					(loop
						(setf line (read-line strm))
						(if line 
							(push line se:buffer)
							(return)
						)
					)
				)
				(setf se:buffer (reverse se:buffer))
				(se:hide-cursor)
				(se:map-brackets t)
				(tft1-set-cursor (* 36 se:cwidth) 0)
				(tft1-set-text-color se:code_col se:cursor_col)
				(setf se:filename fname)
				(setf se:suffix suffix)
				(tft1-write-text (concatenate 'string "FILE: " fname "." suffix "       "))
				(setf se:txtpos (cons 0 0))
				(setf se:offset (cons 0 0))
				(se:show-text)
				(se:show-cursor)
			)
		)
	)
)

(defun se:alert (mymsg)
	(keyboard-flush)
	(se:msg (concatenate 'string mymsg " y/n ?") t)
	(let ((lk nil))
		(loop
			(when lk (return))
			(setf lk (keyboard-get-key t))
		)
		(se:clr-msg)
		(keyboard-flush)
		(if (or (= lk 121) (= lk 89))
			t
			nil
		)
	)
)

(defun se:msg (mymsg &optional alert cursor)
	(if alert
		(tft1-set-text-color se:alert_col)
		(tft1-set-text-color se:emph_col)
	)
	(let ((spos (se:calc-msgpos (cons 0 (1+ (cdr se:txtmax))))))
		(tft1-set-cursor (+ (car spos) 2) (+ (cdr spos) 2))
		(tft1-write-text mymsg)
	)
	(when cursor
		(setf cursor (max 0 cursor))
		(let ((spos (se:calc-msgpos (cons cursor (1+ (cdr se:txtmax))))))
			(tft1-set-text-color se:code_col se:emph_col)
			(tft1-set-cursor (+ (car spos) 2) (+ (cdr spos) 2))
			(tft1-write-text (subseq mymsg cursor (1+ cursor)))
		)
	)
)

(defun se:input (mymsg default maxlen &optional chkanum)
	(keyboard-flush)
	(let* ((ibuf "") (lastkey nil) (newkey nil) (lkd nil) (lku nil)
		(lpos (1+ (cdr se:txtmax)))
		(istart (length mymsg))
		(iend (min maxlen (- (car se:txtmax) istart 1)))
		(ipos 0))

		(when default (setf ibuf default))
		(se:msg (concatenate 'string mymsg ibuf " ") nil istart)
		(loop
			(when (not lastkey)
				(setf lkd (keyboard-get-key t))
				(when lkd (setf lastkey lkd) (setf newkey t))
			)
			(when (and lastkey newkey)
				(setf newkey nil)
				(case lastkey
					(216 (when (> ipos 0) (decf ipos)))
					(215 (when (< ipos (length ibuf)) (incf ipos)))
					(10 (se:clr-msg) (keyboard-flush) (return ibuf))
					(127 (if (> ipos 0)
							(progn
								(decf ipos)
								(setf ibuf (concatenate 'string (subseq ibuf 0 ipos) (subseq ibuf (1+ ipos))))
							)
							(setf ibuf "")
						 ) 
					)
					(t
						(if chkanum 
							(when (and (< (length ibuf) maxlen) (se:alphnum lastkey))
								(setf ibuf (concatenate 'string (subseq ibuf 0 ipos) (string (code-char lastkey)) (subseq ibuf ipos (length ibuf))))
								(incf ipos)
							)
							(when (< (length ibuf) maxlen)
								(setf ibuf (concatenate 'string (subseq ibuf 0 ipos) (string (code-char lastkey)) (subseq ibuf ipos (length ibuf))))
								(incf ipos)
							)
						)
					)
				)
				(se:clr-msg)
				(se:msg (concatenate 'string mymsg ibuf " ") nil (+ istart ipos))
			)
			(setf lku (keyboard-get-key))
			(when lku (setf newkey nil) (setf lastkey nil))
		)
	)
)

(defun se:alphnum (chr)
	(if (or (= chr 31) (= chr 45) (= chr 95) (and (> chr 64) (< chr 91)) (and (> chr 47) (< chr 58)))
		t
		nil 
	)
)

(defun se:clr-msg ()
	(tft1-fill-rect 34 452 763 27 se:bg_col)
)

(defun se:sedit (&optional myform myskin)
	(se:init myskin)
	(let* ((lkd nil)
		   (lku nil)
		   (lastkey nil)
		   (k_pr nil)
		   (numline 0)
		   (kdelay 500)
		   (repcnt nil)
		   (newkey nil)
		   (krepeat nil))
			(if myform
				(progn
					(setf se:funcname (prin1-to-string myform)) 
					(setq se:buffer (cdr (split-string-to-list (string #\Newline) (string (with-output-to-string (str) (pprint (eval myform) str))))))
					(tft1-set-cursor (* 36 se:cwidth) 0)
					(tft1-set-text-color se:code_col se:cursor_col)
					(if (> (length se:funcname) 13)
						(tft1-write-text (concatenate 'string "SYM: " (subseq se:funcname 0 10) "..."))
						(tft1-write-text (concatenate 'string "SYM: " se:funcname))
					)
				)
				(setq se:buffer (list ""))
			)
			(se:map-brackets)
			(se:show-text)
			(se:show-cursor)
			(loop
				(setf lkd (keyboard-get-key t))
				(when (and lkd (not (equal lkd lastkey))) (setf lastkey lkd) (setf repcnt (millis)) (setf newkey t) (setf krepeat nil))
				(when lastkey
					(when (or newkey krepeat)
						(case lastkey
							((or 1 210) (se:linestart))
							((or 5 213) (se:lineend))
							((or 3 17) (when (se:alert "Exit") (se:cleanup) (setf se:exit t)) (keyboard-flush) (setf lastkey nil))
							((or 24 14 2) (se:flush-buffer) (setf lastkey nil))
							((or 11 12) (se:flush-line) (setf lastkey nil))
							(94 (se:docstart))
							(211 (se:prevpage))
							(214 (se:nextpage))
							(194 (se:toggle-match) (setf lastkey nil))
							(195 (se:checkbr) (setf lastkey nil))
							(196 (se:help) (setf lastkey nil))
							(198 (se:run) (setf lastkey nil))
							(202 (se:remove) (setf lastkey nil))
							(203 (se:save) (setf lastkey nil))
							(204 (se:load) (setf lastkey nil))
							(205 (se:show-dir) (setf lastkey nil))
							(216 (se:left))
							(215 (se:right))
							(218 (se:up))
							(217 (se:down))
							(10 (se:enter))
							(9 (se:tab) (setf lastkey nil))
							(127 (se:delete))
							(t (se:insert (code-char lastkey)))
						)
						(setf newkey nil)
						(when krepeat (delay 100))
					)
					(setf lku (keyboard-get-key))
					(when lku (setf lastkey nil) (setf lkd nil) (setf lku nil) (setf krepeat nil))
					(when (and lastkey (not krepeat))
							(when (> (millis) (+ repcnt kdelay)) (setf krepeat t) (setf repcnt nil))
					)
				)
				(when se:exit (tft1-fill-screen) (return t))
			)
	)
	(keyboard-flush)
	nil
)


;
; LispBox editor help screen
;
;
(defun se:ref (chr)
		(mapcan 
			(lambda (x)
				(let ((entry ""))
					(when x
						(setf entry (search chr (string x)))
						(when entry
							(when (and (< entry 1) (> (length (string x)) 0)) (list x))
						)
					)
				)
			) 
			(apropos-list chr)
		)
)


(defun se:build-ref (chr)
	(let ((reflist nil)
		  (ele nil))
		(setf reflist (sort (mapcar princ-to-string (se:ref chr)) string<))
		(if reflist
			(progn
				(setf se:ref-array (make-array (length reflist)))
				(dotimes (i (length reflist))
					(setf ele (pop reflist))
					(when (> (length ele) 0) (setf (aref se:ref-array i) ele))
				)
			)
			(progn
				(setf se:ref-array (make-array 1))
				(setf (aref se:ref-array 0) "No entry")
			)
		)
	)
)


(defun se:build-doc (ctr)
	(when (> ctr (1- (length se:ref-array))) (setf ctr (1- (length se:ref-array))))
	(when (< ctr 0) (setf ctr 0))
	(let ((docstring (documentation (read-from-string (aref se:ref-array ctr))))
		  (numlines 0)
		  (prlist nil)
		  (splitlist nil)
		  (linelist nil)
		  (text "")
		  (line "")
		  (headerlines 1))

		(if (< (length docstring) 1)
			(progn 
				(setf se:docline-array (make-array '(2 1)))
				(setf (aref se:docline-array 0 0) "No doc")
				(setf (aref se:docline-array 1 0) t)
			)
			(progn
				(setf prlist (split-string-to-list (string #\Newline) docstring))
				(if (<= (length (first prlist)) se:maxchar)
					(setf linelist (list (pop prlist)))
					(let ((header (pop prlist))) 
						(setf linelist (list (subseq header 0 se:maxchar)))
						(setf header (subseq header se:maxchar (length header)))
						(loop
							(setf headerlines (1+ headerlines))
							(if (<= (length header) se:maxchar)
								(progn 
									(setf linelist (append linelist (list header)))
									(return)
								)
								(progn 
									(setf linelist (append linelist (list (subseq header 0 (1+ se:maxchar)))))
									(setf header (subseq header se:maxchar (length header)))
								)
							)
						)
					)
				)
				
				(loop
					(unless prlist (return))
					(setf text (pop prlist))
					(setf splitlist (split-string-to-list " " text))
					(setf text "")
					(setf line (pop splitlist))
					(loop
						(unless splitlist (return))
						(setf text (pop splitlist))
						(if (<= (+ (length line) (length text) 1) se:maxchar)
							(setf line (concatenate 'string line " " text))
							(progn
								(setf linelist (append linelist (list line)))
								(setf line text)
							)
						)
					)
					(setf linelist (append linelist (list line)))
				)
				(setf numlines (length linelist))
				(setf se:docline-array (make-array (list 2 numlines)))

				(dotimes (i numlines)
					(setf (aref se:docline-array 0 i) (pop linelist))
					(if (< i headerlines)
						(setf (aref se:docline-array 1 i) t)
						(setf (aref se:docline-array 1 i) nil)
					)
				)
			)
		)
	)
)


(defun se:print-ref (&optional new cctr)
	(se:hide-tft-cursor)
	(when new (fill-screen) (setf se:rctr 0))
	(let ((reflength (length se:ref-array))
		 (refstart 0)
		 (offset (floor (/ se:maxlines 2)))
		 (refstr "")
		 (prstr "")
		 (ctr se:rctr))

		(when cctr (setf ctr cctr))
		(setf ctr (constrain ctr 0 (- reflength 1)))
		(setf se:rctr ctr)

		(if (and (not new) (or (and (< ctr offset) (> ctr 0)) (> ctr (- reflength offset))))
			(progn 
				(setf refstr (aref se:ref-array ctr))
				(setf prstr (subseq refstr 0 (min se:maxchar (length refstr))))
				(setf se:crlin prstr)
				(if (<= ctr offset)
					(setf se:tftcrs ctr)
					(setf se:tftcrs (- (min se:maxlines reflength) (- reflength ctr)))
				)
			)
			(progn
				(fill-screen)
				(setf refstart (max (- ctr offset) 0))
				(setf refstart (max (min (- reflength se:maxlines) refstart) 0))
				(dotimes (i (min se:maxlines reflength))
					(setf refstr (aref se:ref-array (+ refstart i)))
					(setf prstr (subseq refstr 0 (min se:maxchar (length refstr))))
					(if (= (+ refstart i) ctr)
						(progn 
							(setf se:crlin prstr)
							(setf se:tftcrs i)
						)
						(progn 
							(set-cursor 0 (* i se:cysize))
							(with-gfx (strm)
								(princ prstr strm)
							)
						)
					)
				)
			)
		)
	)
	(se:show-tft-cursor)
)


(defun se:print-doc (&optional new cctr)
	(fill-screen)
	(set-cursor 0 0)
	(let ((dalength (second (array-dimensions se:docline-array)))
		 (prstr "")
		 (ctr se:dctr))
		
		(when cctr (setf ctr cctr))
		(setf ctr (constrain ctr 0 (min (- dalength se:maxlines) dalength)))
		(setf se:dctr ctr)

		(dotimes (i (min se:maxlines dalength))
			(setf prstr (aref se:docline-array 0 (+ ctr i)))
			(if (aref se:docline-array 1 (+ ctr i))
				(set-text-color se:bg_col se:help_col)
				(set-text-color se:code_col se:bg_col)
			)
			(set-cursor 0 (* i se:cysize))
			(with-gfx (strm)
				(princ prstr strm)
			)
		)
	)
)


(defun se:wait-encoder ()
	(keyboard-flush)
	(let ((sw-event nil)
				(rot-event 0)
			  (lkd nil))
		(loop
			(when (or (eq sw-event t) (= rot-event 2) lkd) (return))
			(unless (eq (digitalread se:SW) se:sw-state)
				(delay 2)
				(setf se:sw-state (digitalread se:SW))
				(setf sw-event t)
			)

			(unless (eq (digitalread se:CLK) se:clk-state)
        (delay 1)
				(setf se:clk-state (digitalread se:CLK))
				(if (eq (digitalread se:CLK) (digitalread se:DT))
					(setf se:direction "CCW")
					(setf se:direction "CW")
				)
				(setf rot-event (1+ rot-event))
			)

			(setf lkd (keyboard-get-key t))
		)
		(list sw-event rot-event lkd)
	)
)


(defun se:eval-encoder (cstart cmax rot-fun sw-fun &optional key-fun)

	(let ((sw-event nil)
		  (rot-event 0)
		  (key-event nil)
		  (wait-time 0)
		  (result nil)
		  (ctr cstart))

		(loop
			(setf wait-time (for-millis () (setf result (se:wait-encoder))))
			(setf sw-event (first result))
			(setf rot-event (second result))
			(setf key-event (third result))

			(when key-event
				(when key-fun (key-fun key-event))
				(keyboard-flush)
				(return key-event)
			)

			(when (and sw-event se:sw-state)
				(sw-fun ctr)
				(return nil)
			)
			(when (= rot-event 2)
				(when (and (< wait-time 10) (not (equal se:direction se:previous-direction)))
					(setf se:direction se:previous-direction))
				(if (equal se:direction "CCW")
					(setf ctr (min cmax (1+ ctr)))
					(setf ctr (max 0 (1- ctr)))
				)
				(rot-fun nil ctr)
				(setf rot-event 0)
				(setf se:previous-direction se:direction)
			)
		)
		key-event
	)
)

(defun se:insert-fun (ctr)
	(let ((dalength (second (array-dimensions se:docline-array)))
		 (prstr "")
		 (ctr 0))
		(unless (equal (aref se:docline-array 0 0) "No doc")
			(loop
				(setf prstr (aref se:docline-array 0 ctr))
				(if (aref se:docline-array 1 ctr)
					(progn 
						(when (boundp 'se:buffer)
							(dotimes (i (length prstr))
								(se:insert (char prstr i))
							)
						)
						(setf se:last-cmd (concatenate 'string se:last-cmd prstr))
						(setf ctr (1+ ctr))
					)
					(return)
				)
			)
		)
	)
)


(defun se:hide-tft-cursor ()
	(when (> (length se:crlin) 0)
		(fill-rect 0 (* se:tftcrs se:cysize) se:xsize se:cysize 0)
		(set-cursor 0 (* se:tftcrs se:cysize))
		(set-text-color se:code_col se:bg_col)
		(with-gfx (strm)
			(princ se:crlin strm)
		)	
	)
)


(defun se:show-tft-cursor ()
	(when (> (length se:crlin) 0)
		(set-cursor 0 (* se:tftcrs se:cysize))
		(set-text-color se:bg_col se:help_col)
		(with-gfx (strm)
			(princ se:crlin strm)
		)	
	)
)


(defun se:help ()

	(when se:help-active
		(defvar se:code_col (rgb 255 255 255))
		(defvar se:line_col (rgb 90 90 90))
		(defvar se:border_col (rgb 63 40 0))
		(defvar se:bg_col (rgb 0 0 0))
		(defvar se:cursor_col (rgb 160 60 0))
		(defvar se:emph_col (rgb 0 128 0))
		(defvar se:alert_col (rgb 255 0 0))
		(defvar se:input_col (rgb 255 255 255))

		(defvar se:help_col (rgb 255 127 0))

		(defvar se:CLK 16)
		(defvar se:DT 14)
		(defvar se:SW 20)

		(pinmode se:CLK nil)
		(pinmode se:DT nil)
		(pinmode se:SW nil)

		(defvar se:sw-state t)
		(defvar se:clk-state (digitalread se:CLK))
		(defvar se:direction nil)
		(defvar se:previous-direction nil)

		(defvar se:rctr 0)
		(defvar se:dctr 0)
		(defvar se:tftcrs 0)
		(defvar se:crlin "")

		(defvar se:ref-array nil)
		(defvar se:docline-array nil)
		(defvar se:last-cmd "")
		(defvar se:xsize 160)
		(defvar se:ysize 128)
		(defvar se:cxsize 6)
		(defvar se:cysize 8)
		(defvar se:maxlines (floor (/ se:ysize se:cysize)))
		(defvar se:maxchar (floor (/ se:xsize se:cxsize)))

		(let ((result nil)
			  (esc nil)
			  (dalength 0))

			(fill-screen)
			(set-cursor 0 0)
			(set-text-color se:code_col se:bg_col)
			(with-gfx (strm)
				(princ (concatenate 'string "Press key or encoder!" (string #\Newline) "F3 to exit.") strm)
			)
			(setf result (se:wait-encoder))

			(if (third result)
				(progn
					(if (and (> (third result) 32) (< (third result) 127))
						(se:build-ref (string (code-char (third result))))
						(se:build-ref "")
					)
					(if (= (third result) 196)
						(progn 
							(fill-screen)
							(set-cursor 0 0)
							(setf esc t)
						)
					)
				)
				(se:build-ref "")
			)
			(unless esc
				(defvar se:sw-state t)
				(se:print-ref t)
				(loop
					(setf result (se:eval-encoder se:rctr (1- (length se:ref-array)) se:print-ref se:build-doc))
					(when result
						(when (= result 196)
							(return)
						)
						(if (and (> result 32) (< result 127))
							(se:build-ref (string (code-char result)))
							(se:build-ref "")
						)
						(setf se:dctr 0)
						(se:print-ref t)
					)
					(unless result
						(fill-screen)
						(se:print-doc)
						(setf dalength (second (array-dimensions se:docline-array)))
						(setf result (se:eval-encoder se:dctr (constrain dalength 0 (min (- dalength se:maxlines) dalength)) se:print-doc se:insert-fun))
						(when result
							(when (= result 196)
								(return)
							)
							(if (and (> result 32) (< result 127))
								(se:build-ref (string (code-char result)))
								(se:build-ref "")
							)
							(setf se:rctr 0)
						)
						(se:print-ref t se:rctr)
					)
				)
				(fill-screen)
			)
			(makunbound 'se:ref-array)
			(makunbound 'se:docline-array)
		)
	)

	(if se:help-active
		se:last-cmd
		"Deactivated. To activate LLH set se:help-active to t."
	)
)


;
; Helper functions
;
;
(defun constrain (value mini maxi)
  (max (min value maxi) mini)
)

; Fn adopted from M5Cardputer editor version by hasn0life
; recursion eliminated, requires search-str from extensions
(defun split-string-to-list (delim str)
	(unless (or (eq str nil) (not (stringp str))) 
		(let* ((start 0)
          (end (search-str delim str))
          (lst nil))
			(loop
        (if (eq end nil) 
          (return (append lst (list (subseq str start))))
				  (setq lst (append lst (list (subseq str start end)))))
        (setq start (1+ end))
        (setq end (search-str delim str start))
      )
    )
  )
)

(defun char-list-to-string (clist)
	(let ((str "")) 
		(dolist (c clist) (setq str (concatenate 'string str c))) 
		str
	)
)

(defun to-hex-char (i)
	(unless (equal i nil)
		(let ((i (abs i))) 
			(code-char (+ i (if (<= i 9) 48 55)) )
		)
	)
)

(defun byte-to-hexstr (i)
	(unless (equal i nil)
		(let ((i (abs i))) 
		 (concatenate 'string (string (to-hex-char (ash i -4))) (string (to-hex-char (logand i 15))) ) 
		)
	)
)

(defun word-to-hexstr (i)
	(unless (equal i nil)
		(let ((i (abs i))) 
		 (concatenate 'string (byte-to-hexstr (ash i -8)) (byte-to-hexstr (logand i 255))) 
		)
	)
)

(defun hexstr-to-int (s)
	(read-from-string (concatenate 'string "#x" s))
)

(defun remove-if (fn lis)
  (mapcan #'(lambda (item) (unless (funcall fn item) (list item))) lis)
 )

(defun remove-if-not (fn lis)
  (mapcan #'(lambda (item) (when (funcall fn item) (list item))) lis)
)

(defun remove_item (ele lis)
	(remove-if (lambda (item) (equal ele item)) lis)
)

(defun remove (place lis)
	(let ((newlis ()))
		(dotimes (i place)
			(push (pop lis) newlis)
		)
		(pop lis)
		(dotimes (i (length lis))
			(push (pop lis) newlis)
		)
		(reverse newlis)
	)
)

(defun assoc* (a alist test)
  (cond
   ((null alist) nil)
   ((funcall test a (caar alist)) (car alist))
   (t (assoc* a (cdr alist) test))
  )
)

(defun reverse-assoc* (a alist test)
  (cond
   ((null alist) nil)
   ((funcall test a (cdar alist)) (caar alist))
   (t (reverse-assoc* a (cdr alist) test))
  )
)

(defun get-obj (aname anum)
	(read-from-string (eval (concatenate 'string aname (string anum))))
)



;
; ErsatzMoco framework functions
; using extended ULOS 
;
;
; 
; class moco-message
;
(defvar moco-message 
	(class nil '(
			command 0
			id "0000"
			require_ack nil
			data ""

			timeout 10000
			resend_ctr 0
			error_code 0

			_build-from-string #'(lambda (self data) 
				(let ((parts (split-string-to-list ":" data)))
					(cond 
						((> (length data) 36) (stv self 'error_code 93))
						((/= (length parts) 6) (stv self 'error_code 93))
						((> (length (nth 4 parts)) 18) (stv self 'error_code 94))
						((not (equal (nth 0 parts) "@EM")) (stv self 'error_code 98))
						((not (equal (nth 5 parts) "ME@")) (stv self 'error_code 99))
						((or (> (length (nth 1 parts)) 2) (< (length (nth 1 parts)) 1)) (stv self 'error_code 96))
						((or (> (length (nth 2 parts)) 4) (< (length (nth 2 parts)) 1)) (stv self 'error_code 97))
						((or (> (length (nth 3 parts)) 1) (< (length (nth 3 parts)) 1) (> (read-from-string (nth 3 parts)) 1) (< (read-from-string (nth 3 parts)) 0)) (stv self 'error_code 95))
					)
					(unless (/= (gtv self 'error_code) 0)
						(stv self 'command (read-from-string (nth 1 parts)))
						(stv self 'id (nth 2 parts))
						(if (= (read-from-string(nth 3 parts)) 1) (stv self 'require_ack t) (stv self 'require_ack nil))
						(stv self 'data (nth 4 parts))
						t
					) 
				)
			)

			_build-from-scratch #'(lambda (self cmd ack mydata rc tmo)
				(stv self 'id (word-to-hexstr (logand (abs (millis)) #xFFFF)))
				(stv self 'timeout tmo)
				(stv self 'resend_ctr rc)
				(stv self 'command cmd)
				(stv self 'require_ack (if ack t nil))
				(stv self 'data (string mydata))
				(when (cmt self '_msg-valid) t)
			)

			_build-acknowledge #'(lambda (self myid)
				(when (symbolp myid) (setq myid (eval myid)))
				(stv self 'timeout 10000)
				(stv self 'resend_ctr 0)
				(stv self 'command (gtv moco-command myid))
				(stv self 'require_ack nil)
				(stv self 'id myid)
				(stv self 'data "0")
			)

			_msg-valid #'(lambda (self)
				(stv self 'error_code 0)
				(cond 
					((> (length (gtv self 'data)) 18) (stv self 'error_code 94))
					((or (> (gtv self 'command) 99) (< (gtv self 'command) 0)) (stv self 'error_code 96))
					((numberp (gtv self 'require_ack)) (stv self 'error_code 95))
				)
				(let ((ec (gtv self 'error_code)))
					(if (/= ec 0) nil t)
				)
			)

			_to-string #'(lambda (self)
				(concatenate 'string
					"@EM:" (string (gtv self 'command)) ":"
					(gtv self 'id) ":"
					(if (gtv self 'require_ack) "1" "0") ":"
					(string (gtv self 'data)) ":ME@"
				)
			)

			_decrease-resend-ctr #'(lambda (self)
				(unless (< (gtv self 'resend_ctr) 1)
					(stv self 'resend_ctr (1- (gtv self 'resend_ctr)))
				)
			)
		)
	)
)


; 
; class com-module
;
;
;
;		Com module types
;
; 	UNDEFINED = 0
; 	RFM69 		= 1
; 	XBEE 			= 2
; 	SERCOM 		= 3
;
(defvar com-module
	(class nil '(

			recbuffer_size 10
			ackbuffer_size 10
			ack_timeout 200

			receive_buffer ()
			expect_ack_buffer ()
			type 0

			last_sent ""
			last_received ()

			alert_callback nil 

			last_ack_check 0
			last_expect_check 0

			_flush-expect-ack-buffer #'(lambda (self) 
				(stv self 'expect_ack_buffer () )
			)

			_flush-receive-buffer #'(lambda (self) 
				(stv self 'receive_buffer () )
			)

			_send-mmobject #'(lambda (self mmo)
				(cmt self '_send (cmt mmo '_to-string))
			)

			_build-and-send #'(lambda (self cmd ack data rc tmo)
				(let ((mm (class 'moco-message nil t)))
					(cmt mm '_build_from_scratch cmd ack data rc tmo)
					(when (and ack (< (length (gtv self 'expect_ack_buffer)) (gtv com-module '_ackbuffer_size)))
						(stv self 'expect_ack_buffer (append (gtv self 'expect_ack_buffer) (list mm)))
					)
					(cmt self '_send (cmt mm '_to-string))
				)
			)

			_resend #'(lambda (self mmo)
				(cmt self '_send (cmt mmo '_to-string))
				(cmt mmo '_decrease-resend-ctr)			
			)

			_handle-alert #'(lambda (self amm)
				(if (gtv self 'alert_callback)
					(cmt self 'alert_callback amm)
				)
			)

			_receive #'(lambda (self child)
				(let ((mm (class 'moco-message nil t))) 
					(cmt mm '_build-from-string (char-list-to-string (gtv child 'last_received)))
					
					(cond 
						((> (gtv mm 'error_code) 0) nil)
						((= (gtv mm 'command) 0)
							(when (> (length (gtv child 'expect_ack_buffer)) 0)
								(let* ((eab (gtv child 'expect_ack_buffer)))
									(stv child 'expect_ack_buffer 
										(mapcan (lambda (am) (unless (equal (gtv am 'id) (gtv mm 'id))
											(list am)))
											eab)
									)
								)
							)
							nil
						)
						((> (length (gtv child 'receive_buffer)) 0)
							(if (not	
									(dolist (known (gtv child 'receive_buffer))
										(when (equal (gtv known 'id) (gtv mm 'id))
											(return t)
										)
									)
								)
								(progn
									(when (>= (length(gtv child 'receive_buffer)) (gtv child '_recbuffer_size))
										(pop (gtv child 'receive_buffer))
									)
									(when (gtv mm 'ack_required)
										(stv mm 'resend_ctr 3)
										(stv mm 'timeout (gtv child '_ack_timeout))
									)
									(append (gtv child 'receive_buffer) (list mm))
									t
								)
								nil
							)
						)
					)
				)

			)

			_update #'(lambda (self)
				(defvar curmil (millis))

				(let ((eab (gtv self 'expect_ack_buffer)))
					(when (> (length eab) 0)
						(stv self 'expect_ack_buffer
							(mapcan (lambda (mm) (when (> (gtv mm 'resend_ctr) 0) (progn
									(print (concatenate "ACK never received " (string (gtv mm 'id))))
									(list mm)))) 
								eab)
						)
					)
				)
				(let ((eab (gtv self 'expect_ack_buffer)))
					(when (> (length eab) 0)
						(dolist (mm eab)
							(if (> curmil (+ (gtv mm 'timeout) (gtv self 'last_expect_check))) 
								(progn
									(print (concatenate "Resend! " (gtv mm 'data)))
									(cmt self '_resend mm)
								)
								(progn
									(print "Alert!")
									(cmt self '_handle-alert mm)
								)
							)
							(stv self 'last_expect_check curmil)
						)
					)
				)
				(let ((rb (gtv self 'receive_buffer)))
					(when (> (length rb) 0)
						(dolist (mm rb)
							(when (and (gtv mm 'require_ack) (> curmil (+ (gtv mm 'timeout) (gtv self 'last_ack_check))))
								(when (< (gtv mm 'resend_ctr) 0)
									(let ((ackmm (class 'moco-message nil t)))
										(cmt ackmm '_build-acknowledge (gtv mm 'id))
										(cmt self '_send-mmobject ackmm)
										(cmt mm '_decrease-resend-ctr)
									)
								)
								(stv self 'last_ack_check curmil)
							)
						)
					)
				)
				(let ((rb (gtv self 'receive_buffer)))
					(if (> (length rb) 0)
						(stv self 'receive_buffer
							(mapcan (lambda (mm) (when (and (gtv mm 'ack_required) (< (gtv mm 'resend_ctr) 1))
									(list mm)))
								rb)
						)
					)
				)
			)
			
		)
	)
)



(defvar sercom-module
	(class 'com-module '(
			
			baud 9600
			port 0
			reading_message nil
			message_complete nil 
			char_counter 0
			intro_counter 0
			outro_counter 0

			intro "@EM:"
			outro ":ME@"

			alert_callback nil 
			last_sent ()
			last_received ()
			last_ack_check (millis)
			last_expect_check (millis)


			_init #'(lambda (self bd pt cb)
				(stv self 'type 3)
				(stv self 'baud bd)
				(stv self 'port pt)
				(stv self 'alert_callback cb)
			)

			_send #'(lambda (self packetstr) 
				(with-serial (strm (gtv self 'port) (gtv self 'baud))
					(write-string packetstr strm)
				)
			)

			_receive #'(lambda (self) 
				(with-serial (strm (gtv self 'port) (gtv self 'baud))
					(let* ((nbyte (read-byte strm))
								 (nextbyte (if (nbyte) (string (code-char nbyte)) nil)))
						(when (nextbyte)
							(if (not (gtv self 'reading_message))
								(let* ((ic (gtv self 'intro_counter))) 
									(if (equal nextbyte (string (char (gtv self 'intro) ic)))
										(stv self 'intro_counter (1+ ic))
										(stv self 'intro_counter 0)
									)
									(when (= ic 4)
										(stv self 'reading_message t)
										(stv self 'intro_counter 0)
										(stv self 'char_counter 0)
										(stv self 'last_received (gtv self 'intro))
									)
								)
								(if (< (gtv self 'char_counter) 39)
									(let* ((oc (gtv self 'outro_counter)) (cc (gtv self 'char_counter)))
										(stv self 'last_received (push nextbyte (gtv self 'last_received)))
										(if (equal nextbyte (string (char (gtv self 'outro) oc)))
											(stv self 'outro_counter (1+ oc))
											(stv self 'outro_counter 0)
										)
										(when (= oc 4)
											(stv self 'reading_message nil)
											(stv self 'message_complete t)
											(stv self 'outro_counter 0)
											(stv self 'char_counter 0)
										)
										(stv self 'char_counter (1+ cc))
									)
									(progn
										(stv self 'reading_message nil)
										(stv self 'message_complete nil)
										(stv self 'outro_counter 0)
										(stv self 'char_counter 0)
									)
								)
							)
						)
						(if (or (not nextbyte) (not (gtv self 'message_complete)))
							nil 

							(progn
								(stv self 'message_complete nil)
								(cmt (gtv self 'parent) '_receive self)
							)
						)
					)
				)
			)
		) 
		t
	)
)


(defvar rfm69-module
	(class 'com-module '(
			
			node_id 1
			net_id 1

			alert_callback nil 
			last_sent ()
			last_received ()
			last_ack_check (millis)
			last_expect_check (millis)


			_init #'(lambda (self noid neid cb)
				(stv self 'type 1) 
				(stv self 'node_id noid)
				(stv self 'net_id neid)
				(stv self 'alert_callback cb)
				(rfm69-begin noid neid)
			)

			_send #'(lambda (self rec packetstr ack) 
				(rfm69-send rec packetstr ack)
			)

			_receive #'(lambda (self)
				(let* ((rec (rfm69-receive)))
					(if (not rec)
						nil
						(progn
							(stv self 'last_received rec)
							(cmt (gtv self 'parent) '_receive self)
						)
					)
				)
			)

			_get-rssi #'(lambda (self)
				(rfm69-get-rssi)
			)
		) 
		t
	)
)


(defvar shape-size
	(class nil '(
			width 0
			height 0

			_has-size #'(lambda (self)
				(if (and (> (gtv self 'width) 0) (> (gtv self 'height) 0))
					t 
					nil 
				)	
			)

			_size-one #'(lambda (self)
				(stv self 'width 1)
				(stv self 'height 1)
			)

			_set-size #'(lambda (self w h)
				(stv self 'width w)
				(stv self 'height h)
			)
		)
	)
)


(defvar pos
	(class nil '(
			x 0
			y 0
		)
	)
)


; 
; class color
;
(defvar color
	(class nil '(
			red 0
			green 0
			blue 0

			_copy-color #'(lambda (self acol)
				(stv self 'red (gtv acol 'r))
				(stv self 'green (gtv acol 'g))
				(stv self 'blue (gtv acol 'b))
			)

			_set-rgb #'(lambda (self r g b)
				(stv self 'red r)
				(stv self 'green g)
				(stv self 'blue b)
			)

			_set-rgb-list #'(lambda (self rgb)
				(stv self 'red (first rgb))
				(stv self 'green (second rgb))
				(stv self 'blue (third rgb))
			)

			_darken-by #'(lambda (self darken)
				(let* ((darken (max 0 darken)) (r 0) (g 0) (b 0))
					(setq r (max 0 (- (gtv self 'red) darken)))
					(setq g (max 0 (- (gtv self 'green) darken)))
					(setq b (max 0 (- (gtv self 'blue) darken)))

					(list r g b)
				)
			)

			_brighten-by #'(lambda (self brighten)
				(let* ((brighten (max 0 brighten)) (r 0) (g 0) (b 0))
					(setq r (min 255 (+ (gtv self 'red) brighten)))
					(setq g (min 255 (+ (gtv self 'green) brighten)))
					(setq b (min 255 (+ (gtv self 'blue) brighten)))

					(list r g b)
				)
			)

			_to-grayscale #'(lambda (self)
				(min 255 (max 0 (floor (+ (* 0.21 (gtv self 'red)) (* 0.72 (gtv self 'green)) (* 0.07 (gtv self 'blue))))))
			)

			_to-bw #'(lambda (self &optional (trs 128))
				(if (> (cmt self '_to-grayscale) trs)
					1
					0 
				)
			)

			_to-hex-color #'(lambda (self)
				(logior (ash (gtv self 'red) 16) (ash (gtv self 'green) 8) (gtv self 'red))
			)

			_to-hex-string  #'(lambda (self)
				(concatenate 'string (byte-to-hexstr (gtv self 'red)) (byte-to-hexstr (gtv self 'green)) (byte-to-hexstr (gtv self 'blue))) 
			)

			_to-16bit #'(lambda (self)
				(let* ((r (gtv self 'red)) (g (gtv self 'green)) (b (gtv self 'blue)))
					(logior (ash (logand r #xf8) 8) (ash (logand g #xfc) 3) (ash b -3))
				)
			)
		)
	)
)


; 
; class display
;
(defvar display
	(class nil '(
			
			type 0
			mono nil

			grid_space nil
			grid_min_x 0
			grid_max_x 0
			grid_min_y 0
			grid_max_y 0

			pix_max_x 0
			pix_max_y 0

			buffer ()

			_init #'(lambda (self) 
				(stv self 'grid_space (class 'shape-size nil t))
			)

			_grid-to-pix #'(lambda (self gridpos) 
				(stv gridpos 'x (constrain (gtv gridpos 'x) (gtv self 'grid_min_x) (gtv self 'grid_max_x)))
				(stv gridpos 'y (constrain (gtv gridpos 'y) (gtv self 'grid_min_y) (gtv self 'grid_max_y)))

				(let* ((px 0) (py 0))
					(setq px (+ (* (gtv self 'grid_min_x) (gtv (gtv self 'grid_space) 'width)) (* (gtv gridpos 'x) (gtv (gtv self 'grid_space) 'width))))
					(setq py (+ (* (gtv self 'grid_min_y) (gtv (gtv self 'grid_space) 'height)) (* (gtv gridpos 'y) (gtv (gtv self 'grid_space) 'height))))
					(class 'pos (list 'x px 'y py))
				)
			)

			_get-grid-topleft #'(lambda (self)
				(class 'pos (list 'x (gtv self 'grid_min_x) 'y (gtv self 'grid_min_y)))
			)

			_get-grid-bottomright #'(lambda (self)
				(class 'pos (list 'x (gtv self 'grid_max_x) 'y (gtv self 'grid_max_y)))
			)

			_get-pix-bottomright #'(lambda (self)
				(class 'pos (list 'x (gtv self 'pix_max_x) 'y (gtv self 'pix_max_y)))
			)

			_set-grid-space #'(lambda (self gs)
				(let* ((grsp (gtv self 'grid_space)))
					(stv grsp 'width (min (gtv gs 'width) (gtv self 'pix_max_x)))
					(stv grsp 'height (min (gtv gs 'height) (gtv self 'pix_max_y)))

					(stv self 'grid_max_x (floor (/ (gtv self 'pix_max_x) (gtv grsp 'width))))
					(stv self 'grid_max_y (floor (/ (gtv self 'pix_max_y) (gtv grsp 'height))))
				)
			)

			_set-grid-min-x #'(lambda (self gmix)
				(stv self 'grid_min_x (max 0 gmix))
			)

			_set-grid-min-y #'(lambda (self gmiy)
				(stv self 'grid_min_y (max 0 gmiy))
			)

			_set-grid-max-x #'(lambda (self gmax)
				(let* ((grsp (gtv self 'grid_space)))
					(stv self 'grid_max_x (min gmax (floor (/ (gtv self 'pix_max_x) (gtv grsp 'width)))))
				)
			)

			_set-grid-max-y #'(lambda (self gmay)
				(let* ((grsp (gtv self 'grid_space)))
					(stv self 'grid_max_y (min gmay (floor (/ (gtv self 'pix_max_y) (gtv grsp 'height)))))
				)
			)

			_set-grid-min #'(lambda (self gmin)
				(cmt self '_set-grid-min-x (gtv gmin 'x))
				(cmt self '_set-grid-min-y (gtv gmin 'y))
			)

			_set-grid-max #'(lambda (self gmax)
				(cmt self '_set-grid-max-x (gtv gmax 'x))
				(cmt self '_set-grid-max-y (gtv gmax 'y))
			)

			_set-grid-borders #'(lambda (self tleft bright)
				(cmt self '_set-grid-min tleft)
				(cmt self '_set-grid-max bright)
			)

			_add-element #'(lambda (self de)
				(stv self 'buffer (cons de (reverse (gtv self 'buffer))))
			)

			_remove-element #'(lambda (self de)
				(stv self 'buffer
					(mapcan (lambda (me) (unless (equal me de)
						(list me)))
						(gtv self 'buffer))
				)
			)

			_get-element-by-num #'(lambda (self num)
				(let* ((bf (gtv self 'buffer)))
					(when (< num (length bf))
						(nth num bf)
					)
				)
			)
		)
	)
)

; 
;		 Display element types
;
;		 UNDEFINED		= 0
;    LABEL				= 1
;    BUTTON				= 2
;    IMAGE 				= 3
;    IMAGEBUTTON	= 4
;
(defvar display-element
	(class nil '(
			
			type 0
			name ""

			pixel_pos nil
			grid_pos nil
			pixel_size nil
			grid_size nil

			fg_color nil 
			bg_color nil 
			border_color nil 

			grid t 
			redraw t 

			current_display nil

			_init #'(lambda (self)
				(stv self 'pixel_pos (class 'pos nil t))
				(stv self 'pixel_size (class 'shape-size nil t))

				(stv self 'grid_pos (class 'pos nil t)) 
				(stv self 'grid_size (class 'shape-size nil t))

				(stv self 'fg_color (class 'color nil t))
				(stv self 'bg_color (class 'color nil t))
				(stv self 'border_color (class 'color nil t))

				(cmt (gtv self 'pixel_size) '_size-one)
				(cmt (gtv self 'grid_size) '_size-one)
			)

			_set-pixel-coords #'(lambda (self myx myy) 
				(let* ((cd (gtv self 'current_display)) (pp (gtv self 'pixel_pos)))
					(unless (null cd)
						(stv pp 'x (constrain myx 0 (gtv cd 'pix_max_x)))
						(stv pp 'y (constrain myy 0 (gtv cd 'pix_max_y)))

						(stv self 'grid nil)
						(stv self 'redraw t)
						t
					)
				)
			)

			_set-pixel-pos #'(lambda (self mypos)
				(cmt self '_set-pixel-coords (gtv mypos 'x) (gtv mypos 'y))
			)

			_get-pixel-pos #'(lambda (self)
				(gtv self 'pixel_pos)
			)

			_set-grid-coords #'(lambda (self myx myy)
				(let* ((cd (gtv self 'current_display)) (gp (gtv self 'grid_pos)))
					(unless (null cd)
						(stv gp 'x (constrain myx (gtv cd 'grid_min_x) (gtv cd 'grid_max_x)))
						(stv gp 'y (constrain myy (gtv cd 'grid_min_y) (gtv cd 'grid_max_y)))

						(stv self 'grid t)
						(cmt self '_calculate-pixel-pos)
						(stv self 'redraw t)
						t
					)
				)
			)

			_set-grid-pos #'(lambda (self mypos)
				(cmt self '_set-grid-coords (gtv mypos 'x) (gtv mypos 'y))
			)

			_get-grid-pos #'(lambda (self)
				(gtv self 'grid_pos)
			)

			_set-size #'(lambda (self width height)
				(stv self 'grid nil)
				(cmt (gtv self 'pixel_size) '_set-size width height)
				(stv self 'redraw t)
			)

			_set-grid-size #'(lambda (self width height)
				(let* ((cd (gtv self 'current_display)) (gs (gtv self 'grid_size)))
					(unless (or (null cd) (< width 1) (< height 1))
						(stv gs 'width (min width (+ (- (gtv cd 'grid_max_x) (gtv cd 'grid_min_x)) 1)))
						(stv gs 'height (min height (+ (- (gtv cd 'grid_max_y) (gtv cd 'grid_min_y)) 1)))

						(cmt self '_calculate-pixel-size)
						(stv self 'redraw t)
						t
					)
				)
			)

			_set-grid-shape #'(lambda (self mysize)
				(cmt self '_set-grid-size (gtv mysize 'width) (gtv mysize 'height))
			)

			_calculate-pixel-pos #'(lambda (self)
				(let* ((cd (gtv self 'current_display)))
					(unless (null cd)
						(stv self 'pixel_pos (cmt cd '_grid-to-pix (gtv self 'grid_pos)))
						t
					)
				)
			)

			_calculate-pixel-size #'(lambda (self)
				(let* ((cd (gtv self 'current_display)))
					(unless (null cd)
						(let* ((gsp (gtv cd 'grid_space)) (ps (gtv self 'pixel_size)) (gsz (gtv self 'grid_size)))
							(stv ps 'width (constrain (* (gtv gsz 'width) (gtv gsp 'width)) 1 (gtv cd 'pix_max_x)))
							(stv ps 'height (constrain (* (gtv gsz 'height) (gtv gsp 'height)) 1 (gtv cd 'pix_max_y)))
						)
						t
					)
				)
			)

			_coords-within #'(lambda (self coords)
				(let* ((pp (gtv self 'pixel_pos))
							 (ps (gtv self 'pixel_size))
							 (xmi (gtv pp 'x))
							 (ymi (gtv pp 'y))
							 (xma (+ xmi (gtv ps 'width) -1))
							 (yma (+ ymi (gtv ps 'height) -1)))

					(if (and (>= (first coords) xmi) (<= (first coords) xma))
						(if (and (>= (second coords) ymi) (<= (second coords) yma))
							t 
							nil
						)
						nil
					)
				)
			)

			_set-display #'(lambda (self myd &optional (rfr nil))
				(unless (null (gtv self 'current_display))
					(cmt (gtv self 'current_display) '_remove-element self)
				)
				(stv self 'current_display myd)
				(cmt myd '_add-element (read-from-string (gtv self 'name)))
				(when rfr
					(cmt self '_reframe-grid-pos)
				)
			)

			_set-colors #'(lambda (self fg bg bd)
				(stv self 'fg_color fg)
				(stv self 'bg_color bg)
				(stv self 'border_color bd)
				(stv self 'redraw t)
			)

			_use-grid #'(lambda (self ongrid)
				(stv self 'grid ongrid)
				(stv self 'redraw t)
			)

			_uses-grid #'(lambda (self)
				(gtv self 'grid)
			)

			_reframe-grid-pos #'(lambda (self)
				(unless (null (gtv self 'current_display))

					(let* ((dp (gtv self 'current_display)) 
								(gp (gtv self 'grid_pos))
								(gs (gtv self 'grid_size))
								(cdmix (gtv dp 'grid_min_x))
								(cdmax (gtv dp 'grid_max_x))
								(cdmiy (gtv dp 'grid_min_y))
								(cdmay (gtv dp 'grid_max_y)))
				    (stv gp 'x (constrain (gtv gp 'x) cdmix cdmax))
				    (stv gp 'y (constrain (gtv gp 'y) cdmiy cdmay))

				    (stv gs 'width (min (gtv gs 'width) (- cdmax cdmix)))
				    (stv gs 'height (min (gtv gs 'height) (- cdmay cdmiy)))
			    )
			    (cmt self '_calculate-pixel-pos)
		    	(stv self 'redraw t)
		    )
			)
		)
	)
)


(defvar label
	(class 'display-element '(

			font ""
			text_pos nil
			text_size 1
			text ""
			stroke 1
			
			_init #'(lambda (self &optional (aname "empty") (atext ""))
				(stv self 'type 1)
				(stv self 'text_pos (class 'pos nil t))
				(stv self 'name aname)
				(stv self 'text atext)
				(stv self 'type 1)
				(stv self 'pixel_size (class 'shape-size nil t))
				(stv self 'grid nil)
				(stv self 'current_display nil)
				(stv self 'pixel_pos (class 'pos nil t))

				(stv self 'grid_pos (class 'pos nil t)) 
				(stv self 'grid_size (class 'shape-size nil t))

				(stv self 'fg_color (class 'color nil t))
				(stv self 'bg_color (class 'color nil t))
				(stv self 'border_color (class 'color nil t))

				(cmt (gtv self 'pixel_size) '_set-size 16 16)
				(cmt (gtv self 'grid_size) '_size-one)
			)

			_set-text-size #'(lambda (self scale)
				(stv self 'text_size scale)
				(stv self 'redraw t)
			)

			_set-text #'(lambda (self atext)
				(stv self 'text (string atext))
				(stv self 'redraw t)
			)

			_set-stroke #'(lambda (self st)
				(let* ((dp (gtv self 'current_display)))
					(unless (null dp)
						(stv self 'stroke (constrain st 1 (min (gtv dp 'pix_max_x) (gtv dp 'pix_max_y))))
						(stv self 'redraw t)
					)
				)	
			)

			_draw #'(lambda (self)
				(unless (null (gtv self 'current_display))
					(when (gtv self 'grid)
						(cmt self '_reframe-grid-pos)
					)
					(let* ((pp (gtv self 'pixel_pos)) (ps (gtv self 'pixel_size)) (tp (gtv self 'text_pos)) (st (gtv self 'stroke)) (minus 0))
						(fill-rect (gtv pp 'x) (gtv pp 'y) (gtv ps 'width) (gtv ps 'height) (cmt (gtv self 'bg_color) '_to-16bit))
						(dotimes (plus st)
							(setq minus (* plus 2))
							(draw-rect (+ (gtv pp 'x) plus) (+ (gtv pp 'y) plus) (- (gtv ps 'width) minus) (- (gtv ps 'height) minus) (cmt (gtv self 'border_color) '_to-16bit))
						)
					
						(let* ((ts (gtv self 'text_size)) (tl (length (gtv self 'text))) (ix 0) (iy 0) )
							(when (> tl 0)
								(set-text-size ts)
								(setq ix (max 0 (round (/ (+ (- (gtv ps 'width) (* 6 tl ts)) ts) 2))))
								(setq iy (max 0 (round (/ (- (gtv ps 'height) (* 7 ts)) 2))))
								(set-cursor (+ (gtv pp 'x) ix) (+ (gtv pp 'y) iy))
								(set-text-color (cmt (gtv self 'fg_color) '_to-16bit))
								(set-text-size (gtv self 'text_size))
								(with-gfx (str)
									(princ (gtv self 'text) str)
								)
							)
						)
					)
					(stv self 'redraw nil)
				)
			)
		) 
		t
	)
)


(defvar button
	(class 'label '(

			downtext ""
			isdown nil
			
			_init #'(lambda (self &optional (aname "empty") (atext "") (adtext ""))
				(stv self 'type 2)
				(stv self 'text_pos (class 'pos nil t))
				(stv self 'name aname)
				(stv self 'text atext)
				(stv self 'downtext adtext)
				(stv self 'type 1)
				(stv self 'pixel_size (class 'shape-size nil t))
				(stv self 'grid nil)
				(stv self 'current_display nil)
				(stv self 'pixel_pos (class 'pos nil t))

				(stv self 'grid_pos (class 'pos nil t)) 
				(stv self 'grid_size (class 'shape-size nil t))

				(stv self 'fg_color (class 'color nil t))
				(stv self 'bg_color (class 'color nil t))
				(stv self 'border_color (class 'color nil t))

				(cmt (gtv self 'pixel_size) '_set-size 16 16)
				(cmt (gtv self 'grid_size) '_size-one)
			)

			_is-down #'(lambda (self)
				(gtv self 'isdown)
			)

			_down #'(lambda (self)
				(stv self 'isdown t)
				(stv self 'redraw t)
			)

			_up #'(lambda (self)
				(stv self 'isdown nil)
				(stv self 'redraw t)
			)

			_toggle #'(lambda (self)
				(stv self 'isdown (not (gtv self 'isdown)))
				(stv self 'redraw t)
			)

			_set-down-text #'(lambda (self atext)
				(stv self 'downtext (string atext))
				(stv self 'redraw t)
			)

			_draw #'(lambda (self)
				(unless (null (gtv self 'current_display))
					(when (gtv self 'grid)
						(cmt self '_reframe-grid-pos)
					)

					(let* ((pp (gtv self 'pixel_pos))
								 (ps (gtv self 'pixel_size))
								 (tp (gtv self 'text_pos))
								 (st (gtv self 'stroke))
								 (dtext (class 'color nil t))
								 (ishad (class 'color nil t))
								 (bshad (class 'color nil t))
								 (bgc (class 'color nil t))
								 (fgc (class 'color '(red 255 green 255 blue 255)))
								 (minus 0)
								 (isd (gtv self 'isdown))
								 (mono (gtv (gtv self 'current_display) 'mono)))

						(cmt dtext '_set-rgb-list (cmt (gtv self 'fg_color) '_darken-by 80))
						(cmt ishad '_set-rgb-list (cmt (gtv self 'bg_color) '_darken-by 80))
						(cmt bshad '_set-rgb-list (cmt (gtv self 'border_color) '_darken-by 80))
						
						(if isd 
							(if mono 
								(fill-rect (gtv pp 'x) (gtv pp 'y) (gtv ps 'width) (gtv ps 'height) (cmt bgc '_to-16bit))
									(fill-rect (gtv pp 'x) (gtv pp 'y) (gtv ps 'width) (gtv ps 'height) (cmt ishad '_to-16bit))
							)
								(if mono 
									(fill-rect (gtv pp 'x) (gtv pp 'y) (gtv ps 'width) (gtv ps 'height) (cmt bgc '_to-16bit))
										(fill-rect (gtv pp 'x) (gtv pp 'y) (gtv ps 'width) (gtv ps 'height) (cmt (gtv self 'bg_color) '_to-16bit))
								)
						)

						(let*  ((ulcol (if isd bshad (gtv self 'border_color)))
										(drcol (if isd (gtv self 'border_color) bshad)))
							(when mono 
								(progn
									(setq ulcol (if isd fgc bgc))
									(setq drcol (if isd bgc fgc))
								)
							)
							(dotimes (plus st)
								(draw-line (+ (gtv pp 'x) plus) (+ (gtv pp 'y) plus) (+ (gtv pp 'x) plus) (+ (gtv pp 'y) (- (gtv ps 'height) plus)) (cmt ulcol '_to-16bit))
								(draw-line (+ (gtv pp 'x) plus) (+ (gtv pp 'y) (- (gtv ps 'height) plus)) (+ (gtv pp 'x) (- (gtv ps 'width) plus)) (+ (gtv pp 'y) (- (gtv ps 'height) plus)) (cmt drcol '_to-16bit))
								(draw-line (+ (gtv pp 'x) (- (gtv ps 'width) plus)) (+ (gtv pp 'y) (- (gtv ps 'height) plus)) (+ (gtv pp 'x) (- (gtv ps 'width) plus)) (+ (gtv pp 'y) plus) (cmt drcol '_to-16bit))
								(draw-line (+ (gtv pp 'x) (- (gtv ps 'width) plus)) (+ (gtv pp 'y) plus) (+ (gtv pp 'x) plus) (+ (gtv pp 'y) plus) (cmt ulcol '_to-16bit))
							)
						)

						(let* ((ts (gtv self 'text_size))
									 (myt (if isd (gtv self 'downtext) (gtv self 'text)))
									 (tl (length myt)) 
									 (ix 0)
									 (iy 0))
							(when (> tl 0)
								(set-text-size ts)
								(setq ix (max 0 (round (/ (+ (- (gtv ps 'width) (* 6 tl ts)) ts) 2))))
								(setq iy (max 0 (round (/ (- (gtv ps 'height) (* 7 ts)) 2))))
								(set-cursor (+ (gtv pp 'x) ix) (+ (gtv pp 'y) iy))
								(if mono
									(set-text-color (cmt fgc '_to-16bit))
										(if isd
											(set-text-color (cmt dtext '_to-16bit))
											(set-text-color (cmt (gtv self 'fg_color) '_to-16bit))
										)
								)
								(set-text-size (gtv self 'text_size))
								(with-gfx (str)
									(if isd
										(princ (gtv self 'downtext) str)
											(princ (gtv self 'text) str)
									)
								)
							)
						)
					)
					(stv self 'redraw nil)
				)
			)
		)
		t 
	)
)

)lisplibrary";
