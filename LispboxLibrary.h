/*
  LispBox LispLibrary - Version 1.2 - Mar 2025
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

#| BACKLIGHT OF SECONDARY DISPLAY DISABLED BY DEFAULT TO SAVE ENERGY |#

(set-backlight 0)

(backtrace t)

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
	#.(format nil "(adp 'obj slotlist)~%ULOS function to add new property/method slots to obj, similar to JavaScript mechanism.")
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
	#.(format nil "(cmt obj 'method [arguments])~%ULOS function to call method of obj, providing a list of arguments.") 
	(apply (eval (gtv obj method)) (append (list obj) arguments))
)


;
; RGB helper function
;
;
(defun rgb (r g b)
	#.(format nil "(rgb r g b)~%Builds 16 bit color value from red, green and blue components provided as 8-bit integers.")
  (logior (ash (logand r #xf8) 8) (ash (logand g #xfc) 3) (ash b -3))
)


;
; LispBox screen editor
; Functions without built-in help to save memory.
;
;
(defun se:init (sk)
	(case sk 
		(t 
		 (defvar se:code_col (rgb 255 155 55))
		 (defvar se:line_col (rgb 90 60 30))
		 (defvar se:border_col (rgb 63 40 0))
		 (defvar se:bg_col (rgb 10 5 0))
		 (defvar se:cursor_col (rgb 160 60 0))
		 (defvar se:emph_col (rgb 0 128 0))
		 (defvar se:msg_col (rgb 0 190 0))
		 (defvar se:alert_col (rgb 255 0 0))
		 (defvar se:input_col (rgb 255 255 255))
		 (defvar se:mark_col (rgb 90 40 0))

		 (defvar se:help_col (rgb 255 127 0))
		)
	)

	(defvar se:origin (cons 34 18))
	(defvar se:txtpos (cons 0 0))
	(defvar se:lasttxtpos (cons 0 0))
	(defvar se:mark (cons nil nil))
	(defvar se:txtmax (cons 94 26))
	(defvar se:offset (cons 0 0))
	(defvar se:scrpos (cons 0 0))
	(defvar se:lastc nil)
	(defvar se:funcname nil)
	(defvar se:symname nil)
	(defvar se:filename nil)
	(defvar se:suffix nil)
	(defvar se:buffer ())
	(defvar se:bufbak ())
	(defvar se:editable t)
	(defvar se:curline "")
	(defvar se:copyline "")
	(defvar se:copybuf ())
	(defvar se:sniplist '("\"\"" "()" "(lambda ())" "(setf )" "(defvar )" "(let (()) )" "(when )" "(unless )" "(dotimes (i ) )" "#| |#"))
	(defvar se:tscale 1)
	(defvar se:leading (* 16 se:tscale))
	(defvar se:cwidth (* 8 se:tscale))
	(defvar se:openings ())
	(defvar se:closings ())
	(defvar se:lastmatch ())
	(defvar se:match nil)
	(defvar se:exit nil)
	(defvar se:numtabs 2)
	(defvar se:lastsrch nil)
	(defvar se:markset nil)
	
	(when se:help-active
		(defvar se:CLK 16)
		(defvar se:DT 14)
		(defvar se:SW 20)
		(pinmode se:CLK nil)
		(pinmode se:DT nil)
		(pinmode se:SW nil)

		(defvar se:sw-state t)
		(defvar se:clk-state (digitalread se:CLK))
	)

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

	(se:info-line)
)

(defun se:info-line ()
	(tft1-graphics-mode)
	(tft1-fill-rect 0 0 800 17 0)
	(tft1-text-mode)
	(tft1-set-cursor 0 0)
	(tft1-set-text-color se:bg_col se:cursor_col)
	(if se:help-active
		(tft1-write-text "F1/F2 chk () | F3 help | F5 bind |                          | F9 del | F10 save | F11 load | F12 dir")
		(tft1-write-text "F1/F2 chk () | F5 bind |                                    | F9 del | F10 save | F11 load | F12 dir")
	)
)

(defun se:cleanup ()
	(makunbound 'se:buffer)
	(makunbound 'se:bufbak)
	(makunbound 'se:openings)
	(makunbound 'se:closings)
	(makunbound 'se:curline)
	(makunbound 'se:copyline)
	(makunbound 'se:sniplist)
	(makunbound 'se:copybuf)
	(gc)
)

(defun se:hide-cursor ()
	(when se:lastmatch
		(let ((spos nil) (bpos (car se:lastmatch)) (br (cdr se:lastmatch)))
			(setf spos (se:calc-scrpos bpos))
			(tft1-set-cursor (car spos) (cdr spos)) 
			(if (se:in-mark (cdr bpos))
				(tft1-set-text-color se:code_col se:mark_col)
				(tft1-set-text-color se:code_col se:bg_col)
			) 
			(tft1-write-text (string br))
			(setf se:lastmatch nil)
		)
	)
	(when se:lastc 
		(tft1-set-cursor (car se:scrpos) (cdr se:scrpos)) 
		(if (se:in-mark (cdr se:txtpos))
			(tft1-set-text-color se:code_col se:mark_col)
			(tft1-set-text-color se:code_col se:bg_col)
		) 
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
	(let ((ypos (+ (cdr se:origin) (* (- y (cdr se:offset)) se:leading))) (myl " ") (myy (nth y se:buffer)))
		(when myy (setf myl (concatenate 'string myy myl)))
		(tft1-set-text-color se:line_col)
		(tft1-set-cursor 0 ypos)
		(tft1-write-text (string (1+ y)))

		(tft1-set-cursor (car se:origin) ypos)
		(if (se:in-mark y)
			(tft1-set-text-color se:code_col se:mark_col)
			(tft1-set-text-color se:code_col se:bg_col)
		)
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
	(if se:editable
		(progn
			(setf se:editable nil)
			(se:save-buffer)
			(let ((dirbuf (sd-card-dir 2)))
				(se:compile-dir dirbuf 0)
				(setq se:buffer (reverse se:buffer))
			)
			(se:map-brackets)
			(se:move-window t)

			(tft1-set-cursor (* 36 se:cwidth) 0)
			(tft1-set-text-color se:alert_col se:line_col)
			(tft1-write-text "SD DIRECTORY")

		)
		(se:restore-buffer)
	)
	(se:show-cursor)
	(keyboard-flush)
)

(defun se:compile-dir (mydir tab)
	(let ((tabstr ""))
		(dotimes (i tab)
			(setf tabstr (concatenate 'string tabstr " "))
		)
		(dolist (entry mydir)
			(if (listp entry)
				(se:compile-dir entry (+ tab 3))
				(push (concatenate 'string tabstr entry) se:buffer)
			)
		)
	)
)

(defun se:flush-buffer ()
	(when se:editable
		(keyboard-flush)
		(when (se:alert "Flush buffer")
			(se:hide-cursor)
			(setq se:buffer (list ""))
			(setf se:txtpos (cons 0 0))
			(setf se:offset (cons 0 0))
			(setf se:filename nil)
			(setf se:funcname nil)
			(se:info-line)
			(se:show-text)
			(se:show-cursor)
			(keyboard-flush)
		)
	)
)

(defun se:flush-line ()
	(when se:editable
		(keyboard-flush)
		(se:hide-cursor)
		(if se:markset
			(let* ((start (car se:mark)) (end (cdr se:mark)) (numl (- (1+ end) start)) )
				(setf se:copybuf ())
				(dotimes (ln numl)
					(push (nth (- end ln) se:buffer) se:copybuf)
					(setf (nth (- end ln) se:buffer) "")
				)
				(se:unmark)
			)
			(let* ((x (car se:txtpos))
			   (y (cdr se:txtpos))
			   (myl se:curline)
			   (firsthalf ""))
			  (setf se:copybuf ())
				(setf firsthalf (subseq myl 0 x))
				(setf se:copyline (subseq myl x (length myl)))
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
)

(defun se:insert (newc)
	(when se:editable
		(se:hide-cursor)
		(let* ((x (car se:txtpos))
			   (y (cdr se:txtpos))
			   (myl se:curline)
			   (firsthalf "")
			   (scdhalf ""))
			(setf firsthalf (subseq myl 0 x))
			(setf scdhalf (subseq myl x))
			(setf (nth y se:buffer) (concatenate 'string firsthalf (string newc) scdhalf))
			(incf (car se:txtpos))
			(setf se:curline (nth y se:buffer))
			(setf se:lastc nil)
			(if (> (car se:txtpos) (car se:txtmax)) (se:move-window t) (se:disp-line y))
		)
		(se:map-brackets)
		(se:show-cursor)
	)
)

(defun se:tab (numtabs)
	(keyboard-flush)
		(dotimes (i numtabs)
			(se:insert #\032)
		)
	(keyboard-flush)
)

(defun se:enter ()
	(when se:editable
		(se:hide-cursor)
		(let* ((x (car se:txtpos))
			   (y (cdr se:txtpos))
			   (myl se:curline)
			   (firsthalf "")
			   (scdhalf "")
			   (newl () ))
			(setf firsthalf (subseq myl 0 x))
			(setf scdhalf (subseq myl x))
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
)

(defun se:delete ()
	(when se:editable
		(se:hide-cursor)
		(let* ((x (car se:txtpos))
			   (y (cdr se:txtpos))
			   (myl se:curline)
			   (firsthalf "")
			   (scdhalf ""))
			(if (> x 0)
				(progn
					(setf firsthalf (subseq myl 0 (1- x)))
					(setf scdhalf (subseq myl x))
					(setf (nth y se:buffer) (concatenate 'string firsthalf scdhalf))
					(setf se:curline (concatenate 'string (nth y se:buffer) " "))
					(decf (car se:txtpos))
					(setf se:lastc nil)
					(if (> (car se:offset) 0)
						(se:move-window t)
						(se:disp-line y)
					)
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
)

(defun se:copy ()
	(keyboard-flush)
	(if se:markset
		(let* ((start (car se:mark)) (end (cdr se:mark)) (numl (- (1+ end) start)) )
			(setf se:copybuf ())
			(dotimes (ln numl)
				(push (nth (- end ln) se:buffer) se:copybuf)
			)
			(se:unmark)
		)
		(progn
			(setf se:copybuf ())
			(setf se:copyline se:curline)
		)
	)
	(keyboard-flush)
)

(defun se:paste ()
	(when se:editable
		(keyboard-flush)
		(if se:copybuf
			(let ((firstline t) (y (cdr se:txtpos)))
				(dolist (ln se:copybuf)
					(if firstline
						(progn
							(dotimes (i (length ln))
								(se:insert (char ln i))
							)
							(setf firstline nil)
						)
						(progn
							(se:enter)
							(incf y)
							(setf (nth y se:buffer) ln)
							(setf (car se:txtpos) (length ln))
							(setf se:curline ln)
							(setf se:lastc nil)
						)
					)
				)
				(se:move-window t)
				(se:map-brackets)
				(se:show-cursor)
			)
			(when se:copyline
				(dotimes (i (length se:copyline))
					(se:insert (char se:copyline i))
				)
			)	
		)
		(keyboard-flush)
	)
)

(defun se:snippet (lk)
	(keyboard-flush)
	(let ((sn (nth (- lk 240) se:sniplist)))
		(when (> (length sn) 0)
				(dotimes (i (length sn))
					(se:insert (char sn i))
				)
		)
	)
	(keyboard-flush)
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

(defun se:search ()
	(keyboard-flush)
	(se:hide-cursor)
	(let* ((srchstr (se:input "SEARCH for: " se:lastsrch 40)) 
		(found nil) (fx nil) (fy (cdr se:txtpos)) 
		(srchcell se:buffer))
		(setf se:lastsrch srchstr)
		(when srchstr
			(dotimes (i fy) (setf srchcell (cdr srchcell)))
			(loop
				(setf fx (search srchstr (car srchcell)))
				(when fx
					(return)
				)
				(incf fy)
				(setf srchcell (cdr srchcell))
				(unless srchcell (return))
			)
		)
		(if fx
			(progn
				(setf se:txtpos (cons fx fy))
				(se:move-window t)
			)
			(progn
				(se:msg "No match.")
				(delay 2000)
			)
		)
	)
	(se:clr-msg)
	(se:show-cursor)
	(keyboard-flush)
)


(defun se:mark-in ()
	(se:hide-cursor)
	(setf (car se:mark) (cdr se:txtpos))
	(when (se:checkmark) (se:move-window t)	)
	(se:show-cursor)
)

(defun se:mark-out ()
	(se:hide-cursor)
	(setf (cdr se:mark) (cdr se:txtpos))
	(when (se:checkmark) (se:move-window t)	)
	(se:show-cursor)
)

(defun se:unmark ()
	(when se:markset
		(se:hide-cursor)
		(setf se:mark (cons nil nil))
		(se:checkmark)
		(se:move-window t)
		(se:show-cursor)
	)
)

(defun se:checkmark ()
	(if (and (car se:mark) (cdr se:mark))
		(setf se:markset t)
		(setf se:markset nil)
	)
	se:markset
)

(defun se:in-mark (y)
	(if se:markset
		(if (and (>= y (car se:mark)) (<= y (cdr se:mark)))
			t
			nil
		)
		nil
	)
)

(defun se:run ()
	(when se:editable
		(let ((body "") (fname (se:input "Symbol name: " se:funcname 60)))
			(mapc (lambda (x) (setf body (concatenate 'string body x))) se:buffer)
			(if (> (length fname) 0)
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
)

(defun se:execute ()
	(if se:markset
		(let* ((eline "(progn ") (start (car se:mark)) (end (cdr se:mark)) 
				(numl (- (1+ end) start)))
			(dotimes (ln numl)
				(setf eline (concatenate 'string eline (nth (+ start ln) se:buffer)))
			)
			(setf eline (concatenate 'string eline ")" ))
			(eval (read-from-string eline))
		)
		(eval (read-from-string (nth (cdr se:txtpos) se:buffer)))
	)
)

(defun se:remove ()
	(let ((fname (se:input "DELETE file name: " nil 17 t)) (suffix (se:input "Suffix: ." "CL" 3 t)))
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
	(let ((fname (se:input "SAVE file name: " se:filename 17 t)) (suffix (se:input "Suffix: ." se:suffix 3 t)) (overwrite t) (fullname ""))
		(setf fullname (concatenate 'string fname "." suffix))		
		(if (sd-file-exists fullname)
			(unless (se:alert "File exists. Overwrite")
				(setf overwrite nil)
			)
		)
		(unless (or (< (length fname) 1) (< (length suffix) 1) (not overwrite))
			(when overwrite (sd-file-remove fullname))
			(with-sd-card (strm fullname 2)
				(dolist (line se:buffer)
					(princ line strm)
					(princ (code-char 10) strm)
				)
			)
			(se:info-line)
			(tft1-set-cursor (* 36 se:cwidth) 0)
			(tft1-set-text-color se:code_col se:cursor_col)
			(setf se:filename fname)
			(setf se:suffix suffix)
			(tft1-write-text (concatenate 'string "FILE: " fname "." suffix))
			(se:msg "Done! Returning to editor.")
			(delay 1000)
			(se:clr-msg)
		)
	)
)

(defun se:quicksave ()
	(when se:editable
		(keyboard-flush)
		(sd-file-remove "lisp/BACKUP.CL")
		(with-sd-card (strm "lisp/BACKUP.CL" 2)
			(dolist (line se:buffer)
				(princ line strm)
				(princ (code-char 10) strm)
			)
		)
		(se:msg "SAVED to BACKUP.CL")
		(delay 500)
		(keyboard-flush)
		(se:clr-msg)
	)
)

(defun se:load ()
	(when se:editable
		(when (se:alert "Discard buffer and load from SD")
			(let ((fname (se:input "LOAD file name: " nil 17 t)) (suffix (se:input "Suffix: ." "CL" 3 t)) (line ""))
				(unless (or (< (length fname) 1) (< (length suffix) 1) (not (sd-file-exists (concatenate 'string fname "." suffix))))
					(se:unmark)
					(setq se:buffer ())
					(setf se:funcname nil)
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
					(setf se:filename fname)
					(setf se:suffix suffix)
					(se:info-line)
					(setf fname (concatenate 'string fname "." suffix))
					(tft1-set-cursor (* 36 se:cwidth) 0)
					(tft1-set-text-color se:code_col se:cursor_col)
					(if (> (length fname) 13)
						(tft1-write-text (concatenate 'string "FILE: " (subseq fname 0 10) "..."))
						(tft1-write-text (concatenate 'string "FILE: " fname))
					)
					(setf se:txtpos (cons 0 0))
					(setf se:offset (cons 0 0))
					(se:show-text)
					(se:show-cursor)
				)
			)
		)
	)
)

(defun se:save-buffer ()
	(se:unmark)
	(setq se:bufbak (copy-list se:buffer))
	(setf se:lasttxtpos (cons (car se:txtpos) (cdr se:txtpos)))
	(setf se:txtpos (cons 0 0))
	(setq se:buffer ())
	(se:info-line)
		(when se:match
		(tft1-set-cursor 0 0)
		(tft1-set-text-color se:bg_col se:emph_col)
		(tft1-write-text "F1")
	)
)

(defun se:restore-buffer ()
	(se:unmark)
	(setq se:buffer (copy-list se:bufbak))
	(makunbound 'se:bufbak)
	(defvar se:bufbak nil)
	(se:map-brackets)
	(se:info-line)
	(if (or se:funcname se:filename)
		(let ((myname "") (mytype ""))
			(if se:funcname
				(progn (setf myname se:funcname) (setf mytype "SYM: "))
				(progn (setf myname se:filename) (setf mytype "FILE: "))
			)
			(tft1-set-cursor (* 36 se:cwidth) 0)
			(tft1-set-text-color se:code_col se:cursor_col)
			(if (> (length myname) 13)
				(tft1-write-text (concatenate 'string mytype (subseq myname 0 10) "..."))
				(tft1-write-text (concatenate 'string mytype myname))
			)
		)
		(se:info-line)
	)
	(when se:match
		(tft1-set-cursor 0 0)
		(tft1-set-text-color se:bg_col se:emph_col)
		(tft1-write-text "F1")
	)
	(setf se:txtpos (cons (car se:lasttxtpos) (cdr se:lasttxtpos)))
	(se:move-window t)
	(setf se:editable t)
)

(defun se:viewsym ()
	(keyboard-flush)
	(se:hide-cursor)
	(if se:editable
		(let ((symname (se:input "VIEW SYM: " nil 60)))
			(when (boundp (read-from-string symname))
				(setf se:editable nil)
				(se:save-buffer)
				(setq se:buffer (cdr (split-string-to-list (string #\Newline) (string (with-output-to-string (str) (pprint (eval (read-from-string symname)) str))))))
				(se:map-brackets)
				(se:move-window t)

				(tft1-set-cursor (* 36 se:cwidth) 0)
				(tft1-set-text-color se:alert_col se:line_col)
				(if (> (length symname) 13)
					(tft1-write-text (concatenate 'string "SYM: " (subseq symname 0 10) "..."))
					(tft1-write-text (concatenate 'string "SYM: " symname))
				)
			)
		)
		(se:restore-buffer)
	)
	(se:show-cursor)
	(keyboard-flush)
)

(defun se:loadview ()
	(keyboard-flush)
	(se:hide-cursor)
	(if se:editable
		(let ((fname (se:input "VIEW file: " nil 17 t)) (suffix (se:input "Suffix: ." "CL" 3 t)) (line ""))
			(unless (or (< (length fname) 1) (< (length suffix) 1) (not (sd-file-exists (concatenate 'string fname "." suffix))))
				(setf se:editable nil)
				(se:save-buffer)
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
				(se:map-brackets t)
				(se:move-window t)
				(se:info-line)
				(setf fname (concatenate 'string fname "." suffix))
				(tft1-set-cursor (* 36 se:cwidth) 0)
				(tft1-set-text-color se:alert_col se:line_col)
				(if (> (length fname) 13)
					(tft1-write-text (concatenate 'string "FILE: " (subseq fname 0 10) "..."))
					(tft1-write-text (concatenate 'string "FILE: " fname))
				)
			)
		)
		(se:restore-buffer)
	)
	(se:show-cursor)
	(keyboard-flush)
)

(defun se:alert (mymsg)
	(keyboard-flush)
	(se:msg (concatenate 'string mymsg " y/n ?") t)
	(let ((lk nil))
		(loop
			(when lk (return))
			(setf lk (keyboard-get-key))
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
		(tft1-set-text-color se:msg_col)
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
	(let* ((ibuf "") (pressedkey nil)
		(lpos (1+ (cdr se:txtmax)))
		(istart (length mymsg))
		(iend (min maxlen (- (car se:txtmax) istart 1)))
		(ipos 0))

		(when default (setf ibuf default))
		(se:msg (concatenate 'string mymsg ibuf " ") nil istart)
		(loop
			(setf pressedkey (keyboard-get-key))
			(when pressedkey
				(case pressedkey
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
							(when (and (< (length ibuf) maxlen) (se:alphnum pressedkey))
								(setf ibuf (concatenate 'string (subseq ibuf 0 ipos) (string (code-char pressedkey)) (subseq ibuf ipos (length ibuf))))
								(incf ipos)
							)
							(when (< (length ibuf) maxlen)
								(setf ibuf (concatenate 'string (subseq ibuf 0 ipos) (string (code-char pressedkey)) (subseq ibuf ipos (length ibuf))))
								(incf ipos)
							)
						)
					)
				)
				(se:clr-msg)
				(se:msg (concatenate 'string mymsg ibuf " ") nil (+ istart ipos))
			)
		)
	)
)

(defun se:alphnum (chr)
	(if (or (= chr 31) (= chr 45) (= chr 47) (= chr 95) (and (> chr 64) (< chr 91)) (and (> chr 96) (< chr 123)) (and (> chr 47) (< chr 58)))
		t
		nil 
	)
)

(defun se:clr-msg ()
	(tft1-fill-rect 34 452 763 27 se:bg_col)
)

(defun se:sedit (&optional myform myskin)
	#.(format nil "(se:sedit ['symbol])~%Invoke fullscreen editor, optionally providing the name of a bound symbol to be edited.")
	(se:init myskin)
	(let* ((pressedkey nil)
			 (savedkey nil)
		   (numline 0)
		   (kwait nil)
		   (kwaitmax 500)
		   (kwaitstart 0)
		   (krepeat nil)
		   (pollenc nil)
		   (numrot 0))

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
				(when se:help-active
					(setf pollenc (second (se:poll-encoder)))
					(when pollenc
						(if (= numrot 1)
							(progn
								(if (equal pollenc "CW")
									(se:up)
									(se:down)
								)
								(setf numrot 0)
							)
							(setf numrot (1+ numrot))
						)
					)
				)
				
				(if krepeat
					(unless (keyboard-key-pressed) (setf krepeat nil) (setf savedkey nil)) 
					(setf pressedkey (keyboard-get-key))
				)

				(when (and kwait pressedkey)
					(setf kwait nil)
				)

				(when (and (not kwait) (not krepeat))
					(setf kwait t) 
			  	(setf kwaitstart (millis))
			  	(setf savedkey pressedkey)
				)

				(when kwait
					(when (> (abs (- (millis) kwaitstart)) kwaitmax)
						(if (keyboard-key-pressed)
							(progn (setf krepeat t) (setf pressedkey savedkey) (setf kwait nil))
							(setf kwait nil)
						)
					)
				)

				(when pressedkey
					(case pressedkey
						((1 210) (se:linestart))
						((5 213) (se:lineend))
						(9 (se:mark-in))
						(15 (se:mark-out))
						(16 (se:unmark))
						(18 (se:execute))
						(19 (se:search))
						((3 17) (when (se:alert "Exit") (se:cleanup) (setf se:exit t)) (keyboard-flush))
						((24 14 2) (se:flush-buffer))
						((11 12 253) (se:flush-line))
						(94 (se:docstart))
						(211 (se:prevpage))
						(214 (se:nextpage))
						(194 (se:toggle-match))
						(195 (se:checkbr))
						(196 (se:help))
						(197 (se:viewsym))
						(198 (se:run))
						(200 (se:loadview))
						(201 (se:quicksave))
						(202 (se:remove))
						(203 (se:save))
						(204 (se:load))
						(205 (se:show-dir))
						(216 (se:left))
						(215 (se:right))
						(218 (se:up))
						(217 (se:down))
						(10 (se:enter))
						(9 (se:tab se:numtabs))
						(127 (se:delete))
						(251 (se:copy))
						(252 (se:paste))
						((240 241 242 243 244 245 246 247 248 249 250) (se:snippet pressedkey))
						(t (se:insert (code-char pressedkey)))
					)

					(when krepeat (delay 100))
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
				(set-text-color se:hbg_col se:hhelp_col)
				(set-text-color se:hcode_col se:hbg_col)
			)
			(set-cursor 0 (* i se:cysize))
			(with-gfx (strm)
				(princ prstr strm)
			)
		)
	)
)

(defun se:poll-encoder ()
	(let ((sw-event nil)
				(rot-event nil))
			(unless (eq (digitalread se:SW) se:sw-state)
				(delay 2)
				(setf se:sw-state (digitalread se:SW))
				(setf sw-event t)
			)

			(unless (eq (digitalread se:CLK) se:clk-state)
        (delay 1)
				(setf se:clk-state (digitalread se:CLK))
				(if (eq (digitalread se:CLK) (digitalread se:DT))
					(setf rot-event "CCW")
					(setf rot-event "CW")
				)
			)
		(list sw-event rot-event)
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

			(setf lkd (keyboard-get-key))
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
		(set-text-color se:hcode_col se:hbg_col)
		(with-gfx (strm)
			(princ se:crlin strm)
		)	
	)
)


(defun se:show-tft-cursor ()
	(when (> (length se:crlin) 0)
		(set-cursor 0 (* se:tftcrs se:cysize))
		(set-text-color se:hbg_col se:hhelp_col)
		(with-gfx (strm)
			(princ se:crlin strm)
		)	
	)
)


(defun se:help ()
	#.(format nil "(se:help)~%Invoke Lispy Little Helper from REPL. Exit with F3 on USB keyboard.")
	(when se:help-active

		(set-backlight 255)

		(defvar se:hcode_col (rgb 255 255 255))
		(defvar se:hline_col (rgb 90 90 90))
		(defvar se:hborder_col (rgb 63 40 0))
		(defvar se:hbg_col (rgb 0 0 0))
		(defvar se:hcursor_col (rgb 160 60 0))
		(defvar se:hemph_col (rgb 0 128 0))
		(defvar se:halert_col (rgb 255 0 0))
		(defvar se:hinput_col (rgb 255 255 255))

		(defvar se:hhelp_col (rgb 255 127 0))

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
			(set-text-color se:hcode_col se:hbg_col)
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

			(set-backlight 0)
		)
	)

	(if se:help-active
		se:last-cmd
		"Deactivated. To activate LLH set se:help-active to t."
	)
	(keyboard-flush)
)


;
; Helper functions
;
;
(defun constrain (value mini maxi)
	#.(format nil "(constrain value mini maxi)~%Constrain a value to interval between mini and maxi (including both).")
  (max (min value maxi) mini)
)

; Fn adopted from M5Cardputer editor version by hasn0life
; recursion eliminated, requires search-str from extensions
(defun split-string-to-list (delim str)
	#.(format nil "(split-string-to-list delimiter str)~%Split string str into list, cutting it at delimiter.")
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
	#.(format nil "(char-list-to-string clist)~%Build string from a list of characters.")
	(let ((str "")) 
		(dolist (c clist) (setq str (concatenate 'string str c))) 
		str
	)
)

(defun to-hex-char (i)
	#.(format nil "(to-hex-char i)~%Convert number i (<= 15) to hex char.")
	(unless (equal i nil)
		(let ((i (abs i))) 
			(code-char (+ i (if (<= i 9) 48 55)) )
		)
	)
)

(defun byte-to-hexstr (i)
	#.(format nil "(byte-to-hexstr i)~%Convert number i (<= 255) to hex string.")
	(unless (equal i nil)
		(let ((i (abs i))) 
		 (concatenate 'string (string (to-hex-char (ash i -4))) (string (to-hex-char (logand i 15))) ) 
		)
	)
)

(defun word-to-hexstr (i)
	#.(format nil "(word-to-hexstr i)~%Convert number i (<= 65535) to hex string.")
	(unless (equal i nil)
		(let ((i (abs i))) 
		 (concatenate 'string (byte-to-hexstr (ash i -8)) (byte-to-hexstr (logand i 255))) 
		)
	)
)

(defun hexstr-to-int (s)
	#.(format nil "(hexstr-to-int str)~%Convert hex string to int.")
	(read-from-string (concatenate 'string "#x" s))
)

(defun remove-if (fn lis)
	#.(format nil "(remove-if fn lis)~%Remove list items when processing them with function fn evals to t.")
  (mapcan #'(lambda (item) (unless (funcall fn item) (list item))) lis)
 )

(defun remove-if-not (fn lis)
	#.(format nil "(remove-if-not fn lis)~%Remove list items when processing them with function fn evals to nil.")
  (mapcan #'(lambda (item) (when (funcall fn item) (list item))) lis)
)

(defun remove-item (ele lis)
	#.(format nil "(remove-item ele lis)~%Remove item ele from list.")
	(remove-if (lambda (item) (equal ele item)) lis)
)

(defun remove (place lis)
	#.(format nil "(remove n lis)~%Remove nth item from list.")
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
	#.(format nil "(assoc* key list testfn)~%Looks up a key in an association list of (key . value) pairs, and returns *all* matching pairs as a list, or nil if no pair is found.")
  (cond
   ((null alist) nil)
   ((funcall test a (caar alist)) (car alist))
   (t (assoc* a (cdr alist) test))
  )
)

(defun reverse-assoc* (a alist test)
	#.(format nil "(assoc* val list testfn)~%Looks up a value in an association list of (key . value) pairs, and returns *all* matching pairs as a list, or nil if no pair is found.")
  (cond
   ((null alist) nil)
   ((funcall test a (cdar alist)) (caar alist))
   (t (reverse-assoc* a (cdr alist) test))
  )
)

(defun get-obj (aname anum)
	#.(format nil "(get-obj aname index)~%Retrieve numbered object by providing the root name and a number.")
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

#| uLisp Assembler |#
#| ARM Thumb Assembler for uLisp - Version 10 - 18th November 2024 |#
#| see http://www.ulisp.com/show?2YRU |#

(defun regno (sym)
  (case sym (sp 13) (lr 14) (pc 15)
    (t (read-from-string (subseq (string sym) 1)))))

(defun emit (bits &rest args)
  (let ((word 0) (shift -28))
    (mapc #'(lambda (value)
              (let ((width (logand (ash bits shift) #xf)))
                (incf shift 4)
                (unless (zerop (ash value (- width))) (error "Won't fit"))
                (setq word (logior (ash word width) value))))
          args)
    word))

(defun offset (label) (ash (- label *pc* 4) -1))

(defun $word (val)
  (append
   (unless (zerop (mod *pc* 4)) (list ($nop)))
   (list (logand val #xffff) (logand (ash val -16) #xffff))))

(defun lsl-lsr-0 (op argd argm immed5)
  (emit #x41533000 0 op immed5 (regno argm) (regno argd)))

(defun asr-0 (op argd argm immed5)
  (emit #x41533000 1 op immed5 (regno argm) (regno argd)))

(defun add-sub-1 (op argd argn argm)
  (cond
   ((numberp argm)
    (emit #x61333000 #b000111 op argm (regno argn) (regno argd)))
   ((null argm)
    (emit #x61333000 #b000110 op (regno argn) (regno argd) (regno argd)))
   (t
    (emit #x61333000 #b000110 op (regno argm) (regno argn) (regno argd)))))

(defun mov-sub-2-3 (op2 op argd immed8)
  (emit #x41380000 op2 op (regno argd) immed8))

(defun add-mov-4 (op argd argm)
  (let ((rd (regno argd))
        (rm (regno argm)))
    (cond
     ((and (>= rd 8) (>= rm 8))
      (emit #x61333000 #b010001 op #b011 (- rm 8) (- rd 8)))
     ((>= rm 8)
      (emit #x61333000 #b010001 op #b001 (- rm 8) rd))
     ((>= rd 8)
      (emit #x61333000 #b010001 op #b010 rm (- rd 8))))))

(defun reg-reg (op argd argm)
  (emit #xa3300000 op (regno argm) (regno argd)))

(defun bx-blx (op argm)
  (emit #x81430000 #b01000111 op (regno argm) 0))

(defun str-ldr (op argd arg2)
  (cond
   ((numberp arg2)
    (when (= op 0) (error "str not allowed with label"))
    (let ((arg (- (truncate (+ arg2 2) 4) (truncate *pc* 4) 1)))
      (emit #x41380000 4 1 (regno argd) (max 0 arg))))
   ((listp arg2)
    (let ((argn (first arg2))
          (immed (or (eval (second arg2)) 0)))
      (unless (zerop (mod immed 4)) (error "not multiple of 4"))
      (cond
       ((eq (regno argn) 15)
        (when (= op 0) (error "str not allowed with pc"))
        (emit #x41380000 4 1 (regno argd) (truncate immed 4)))
       ((eq (regno argn) 13)
        (emit #x41380000 9 op (regno argd) (truncate immed 4)))
       (t
        (emit #x41533000 6 op (truncate immed 4) (regno argn) (regno argd))))))
   (t (error "illegal argument"))))

(defun str-ldr-5 (op argd arg2)
  (cond
   ((listp arg2)
    (let ((argn (first arg2))
          (argm (second arg2)))
      (emit #x43333000 5 op (regno argm) (regno argn) (regno argd))))
   (t (error "illegal argument"))))

(defun add-10 (op argd immed8)
  (emit #x41380000 #b1010 op (regno argd) (truncate immed8 4)))

(defun add-sub-11 (op immed7)
  (emit #x81700000 #b11010000 op (truncate immed7 4)))

(defun push-pop (op lst)
  (let ((byte 0)
        (r 0))
    (mapc #'(lambda (x) 
              (cond
               ((and (= op 0) (eq x 'lr)) (setq r 1))
               ((and (= op 1) (eq x 'pc)) (setq r 1))
               (t (setq byte (logior byte (ash 1 (regno x))))))) lst)
    (emit #x41218000 11 op 2 r byte)))

(defun b-cond-13 (cnd label)
  (let ((soff8 (logand (offset label) #xff)))
    (emit #x44800000 13 cnd soff8)))

(defun cpside (op aif)
  (emit #xb1130000 #b10110110011 op 0 aif))

(defun $adc (argd argm)
  (reg-reg #b0100000101 argd argm))

(defun $add (argd argn &optional argm)
  (cond
   ((numberp argm)
    (cond
     ((eq (regno argn) 15)
      (add-10 0 argd argm))
     ((eq (regno argn) 13)
      (add-10 1 argd argm))
     (t (add-sub-1 0 argd argn argm))))
   ((and (numberp argn) (null argm))
    (cond
     ((eq (regno argd) 13)
      (add-sub-11 0 argn))
     (t
      (mov-sub-2-3 3 0 argd argn))))
   (t
    (cond
     ((or (>= (regno argd) 8) (>= (regno argn) 8))
      (add-mov-4 0 argd argn))
     (t
      (add-sub-1 0 argd argn argm))))))

(defun $and (argd argm)
  (reg-reg #b0100000000 argd argm))

(defun $asr (argd argm &optional arg2)
  (unless arg2 (setq arg2 argm argm argd))
  (cond
   ((numberp arg2)
    (asr-0 0 argd argm arg2))
   ((eq argd argm)
    (reg-reg #b0100000100 argd arg2))
   (t (error "First 2 registers must be the same"))))

(defun $b (label)
  (emit #x41b00000 #xe 0 (logand (offset label) #x7ff)))

(defun $bcc (label)
  (b-cond-13 3 label))

(defun $bcs (label)
  (b-cond-13 2 label))

(defun $beq (label)
  (b-cond-13 0 label))

(defun $bge (label)
  (b-cond-13 10 label))

(defun $bgt (label)
  (b-cond-13 12 label))

(defun $bhi (label)
  (b-cond-13 8 label))

(defun $bhs (label)
  (b-cond-13 2 label))

(defun $ble (label)
  (b-cond-13 13 label))

(defun $blo (label)
  (b-cond-13 3 label))

(defun $blt (label)
  (b-cond-13 11 label))

(defun $bmi (label)
  (b-cond-13 4 label))

(defun $bne (label)
  (b-cond-13 1 label))

(defun $bpl (label)
  (b-cond-13 5 label))

(defun $bic (argd argm)
  (reg-reg #b0100001110 argd argm))

(defun $bl (label)
  (list
   (emit #x5b000000 #b11110 (logand (ash (offset label) -11) #x7ff))
   (emit #x5b000000 #b11111 (logand (offset label) #x7ff))))

(defun $blx (argm)
  (bx-blx 1 argm))

(defun $bx (argm)
  (bx-blx 0 argm))

(defun $cmn (argd argm)
  (reg-reg #b0100001011 argd argm))

(defun $cmp (argd argm)
  (cond
   ((numberp argm)
    (mov-sub-2-3 2 1 argd argm))
   (t
    (reg-reg #b0100001010 argd argm))))

(defun $cpsid (aif)
  (cpside 1 aif))

(defun $cpsie (aif)
  (cpside 0 aif))
    
(defun $eor (argd argm)
  (reg-reg #b0100000001 argd argm))

(defun $ldr (argd arg2)
  (str-ldr 1 argd arg2))

(defun $ldrb (argd arg2)
  (str-ldr-5 6 argd arg2))

(defun $ldrh (argd arg2)
  (str-ldr-5 5 argd arg2))

(defun $ldrsb (argd arg2)
  (str-ldr-5 3 argd arg2))

(defun $ldrsh (argd arg2)
  (str-ldr-5 7 argd arg2))

(defun $lsl (argd argm &optional arg2)
  (unless arg2 (setq arg2 argm argm argd))
  (cond
   ((numberp arg2)
    (lsl-lsr-0 0 argd argm arg2))
   ((eq argd argm)
    (reg-reg #b0100000010 argd arg2))
   (t (error "First 2 registers must be the same"))))

(defun $lsr (argd argm &optional arg2)
  (unless arg2 (setq arg2 argm argm argd))
  (cond
   ((numberp arg2)
    (lsl-lsr-0 1 argd argm arg2))
   ((eq argd argm)
    (reg-reg #b0100000011 argd arg2))
   (t (error "First 2 registers must be the same"))))

(defun $mov (argd argm)
  (cond
   ((numberp argm)
    (mov-sub-2-3 2 0 argd argm))
   ((or (>= (regno argd) 8) (>= (regno argm) 8))
    (add-mov-4 1 argd argm))
   (t ; Synonym of LSLS Rd, Rm, #0
    (lsl-lsr-0 0 argd argm 0))))

(defun $mul (argd argm)
  (reg-reg #b0100001101 argd argm))

(defun $mvn (argd argm)
  (reg-reg #b0100001111 argd argm))

(defun $neg (argd argm)
  (reg-reg #b0100001001 argd argm))

(defun $nop () ; mov r8,r8
  (add-mov-4 1 'r8 'r8))

(defun $orr (argd argm)
  (reg-reg #b0100001100 argd argm))

(defun $push (lst)
  (push-pop 0 lst))

(defun $pop (lst)
  (push-pop 1 lst))

(defun $rev (argd argm)
  (reg-reg #b1011101000 argd argm))

(defun $rev16 (argd argm)
  (reg-reg #b1011101001 argd argm))

(defun $revsh (argd argm)
  (reg-reg #b1011101010 argd argm))

(defun $ror (argd argm)
  (reg-reg #b0100000111 argd argm))

(defun $sbc (argd argm)
  (reg-reg #b0100000110 argd argm))

(defun $str (argd arg2)
  (str-ldr 0 argd arg2))

(defun $strb (argd arg2)
  (str-ldr-5 2 argd arg2))

(defun $sub (argd argn &optional argm)
  (cond
   ((not (numberp argn))
    (add-sub-1 1 argd argn argm))
   ((eq (regno argd) 13)
      (add-sub-11 1 argn))
   (t
    (mov-sub-2-3 3 1 argd argn))))

(defun $sxtb (argd argm)
  (reg-reg #b1011001001 argd argm))

(defun $sxth (argd argm)
  (reg-reg #b1011001000 argd argm))

(defun $tst (argd argm)
  (reg-reg #b0100001000 argd argm))

(defun $uxtb (argd argm)
  (reg-reg #b1011001011 argd argm))

(defun $uxth (argd argm)
  (reg-reg #b1011001010 argd argm))


#| uLisp Compiler |#
#| Lisp compiler to ARM Thumb Assembler - Version 2a - 23rd August 2024 |#

(defun compile (name)
  (if (eq (car (eval name)) 'lambda)
      (eval (comp (cons 'defun (cons name (cdr (eval name))))))
    (error "Not a Lisp function")))

(defun comp (x &optional env)
  (cond
   ((null x) (type-code :boolean '(($mov 'r0 0))))
   ((eq x t) (type-code :boolean '(($mov 'r0 1))))
   ((symbolp x) (comp-symbol x env))
   ((atom x) (type-code :integer (list (list '$mov ''r0 x))))
   (t (let ((fn (first x)) (args (rest x)))
        (case fn
          (defun (setq *label-num* 0)
                 (setq env (mapcar #'(lambda (x y) (cons x y)) (second args) *locals*))
                 (comp-defun (first args) (second args) (cddr args) env))
          (progn (comp-progn args env))
          (if    (comp-if (first args) (second args) (third args) env))
          (setq  (comp-setq args env))
          (t     (comp-funcall fn args env)))))))

(defun mappend (fn lst)
  (apply #'append (mapcar fn lst)))

(defun type-code (type code) (cons type code))

(defun code-type (type-code) (car type-code))

(defun code (type-code) (cdr type-code))

(defun checktype (fn type check)
  (unless (or (null type) (null check) (eq type check))
    (error "Argument to '~a' must be ~a not ~a" fn check type)))

(defvar *params* '(r0 r1 r2 r3))

(defvar *locals* '(r4 r5 r6 r7))

(defvar *label-num* 0)

(defun gen-label ()
  (read-from-string (format nil "lab~d" (incf *label-num*))))

(defun comp-symbol (x env)
  (let ((reg (cdr (assoc x env))))
    (type-code nil (list (list '$mov ''r0 (list 'quote reg))))))

(defun comp-setq (args env)
  (let ((value (comp (second args) env))
        (reg (cdr (assoc (first args) env))))
    (type-code 
     (code-type value) 
     (append (code value) (list (list '$mov (list 'quote reg) ''r0))))))

(defun comp-defun (name args body env)
  (let ((used (subseq *locals* 0 (length args))))
    (append 
     (list 'defcode name args)
     (list name (list '$push (list 'quote (cons 'lr (reverse used)))))
     (apply #'append 
            (mapcar #'(lambda (x y) (list (list '$mov (list 'quote x) (list 'quote y))))
                    used *params*))
     (code (comp-progn body env))
     (list (list '$pop (list 'quote (append used (list 'pc))))))))

(defun comp-progn (exps env)
  (let* ((len (1- (length exps)))
         (nlast (subseq exps 0 len))
         (last1 (nth len exps))
         (start (mappend #'(lambda (x) (append (code (comp x env)))) nlast))
         (end (comp last1 env)))
    (type-code (code-type end) (append start (code end)))))

(defun comp-if (pred then else env)
  (let ((lab1 (gen-label))
        (lab2 (gen-label))
        (test (comp pred env)))
    (checktype 'if (car test) :boolean)
    (type-code :integer
               (append
                (code test) (list '($cmp 'r0 0) (list '$beq lab1))
                (code (comp then env)) (list (list '$b lab2) lab1)
                (code (comp else env)) (list lab2)))))

(defun comp-funcall (f args env)
  (let ((test (assoc f '((> . $bgt) (>= . $bge) (= . $beq) 
                         (<= . $ble) (< . $blt) (/= . $bne))))
        (logical (assoc f '((and . $and) (or . $orr))))
        (arith1 (assoc f '((1+ . $add) (1- . $sub))))
        (arith+- (assoc f '((+ . $add) (- . $sub))))
        (arith2 (assoc f '((* . $mul) (logand . $and) (logior . $orr) (logxor . $eor)))))
    (cond
     (test
      (let ((label (gen-label)))
        (type-code :boolean
                   (append
                    (comp-args f args 2 :integer env)
                    (list '($pop '(r1)) '($mov 'r2 1) '($cmp 'r1 'r0) 
                          (list (cdr test) label) '($mov 'r2 0) label '($mov 'r0 'r2))))))
     (logical 
      (type-code :boolean
                 (append
                  (comp-args f args 2 :boolean env)
                  (list '($pop '(r1)) (list (cdr logical) ''r0 ''r1)))))
     (arith1
      (type-code :integer 
                 (append
                  (comp-args f args 1 :integer env)
                  (list (list (cdr arith1) ''r0 1)))))
     (arith+-
      (type-code :integer 
                 (append
                  (comp-args f args 2 :integer env)
                  (list '($pop '(r1)) (list (cdr arith+-) ''r0 ''r1 ''r0)))))
     (arith2
      (type-code :integer 
                 (append
                  (comp-args f args 2 :integer env)
                  (list '($pop '(r1)) (list (cdr arith2) ''r0 ''r1)))))
     ((member f '(car cdr))
      (type-code :integer
                 (append
                  (comp-args f args 1 :integer env)
                  (if (eq f 'cdr) (list '($ldr 'r0 '(r0 4)))
                    (list '($ldr 'r0 '(r0 0)) '($ldr 'r0 '(r0 4)))))))
     (t ; function call
      (type-code :integer 
                 (append
                  (comp-args f args nil :integer env)
                  (when (> (length args) 1)
                    (append
                     (list (list '$mov (list 'quote (nth (1- (length args)) *params*)) ''r0))
                     (mappend
                      #'(lambda (x) (list (list '$pop (list 'quote (list x)))))
                      (reverse (subseq *params* 0 (1- (length args)))))))
                  (list (list '$bl f))))))))

(defun comp-args (fn args n type env)
  (unless (or (null n) (= (length args) n))
    (error "Incorrect number of arguments to '~a'" fn))
  (let ((n (length args)))
    (mappend #'(lambda (y)
                 (let ((c (comp y env)))
                   (decf n)
                   (checktype fn type (code-type c))
                   (if (zerop n) (code c) (append (code c) '(($push '(r0)))))))
             args)))


(backtrace)

)lisplibrary";
