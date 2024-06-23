# ulisp-lispbox
Modified version of ulisp-arm (see below) for use with a self-contained uLisp computer based on the Teensy 4.1, a TFT screen with RA8875 controller and an USB keyboard. The REPL works locally and via serial connection simultaneously and prints to the TFT as well.

Contains several modifications to the ulisp-arm version (many based on the uLisp firmware for the LilyGO T-Deck)
as well as a set of suitable uLisp extensions including RFM69 support and a Lisp library providing an extended ULOS system, the most important parts of a uLisp port of ErsatzMoco (github.com/ersatzmoco/ersatzmoco) and a full-screen uLisp and text editor.
The editor features full SD card support, direct binding to uLisp symbols, full bracket matching and some keyboard shortcuts
partially compatible with EMACS:

* CTRL-q / CTRL-c --- quit editor and return to REPL
* CTRL-x / CTRL-b / CTRL-n --- discard current text buffer (i.e. new file)
* CTRL-k / CTRL-l --- delete line starting at cursor position
* CTRL-a / HOME --- move cursor to start of line
* CTRL-e / END --- move cursor to end of line
* ^ --- move cursor to beginning of buffer
* PG UP/PG DOWN --- move one page up or down

* F1 --- toggle bracket matching on/off
* F2 --- check if bracket under the cursor has a matching bracket in the buffer. If so, they are temporarily highlighted. (Use when continuous bracket matching is off.)
* F5 --- bind contents of the text buffer to a symbol of your choice and quit editor
* F9 --- delete a file on the SD card
* F10 --- save text buffer to SD card
* F11 --- load text from SD card into buffer, discarding the present one
* F12 --- show directory of SD card
Note: file names for SD card follow the 8.3 standard and thus must be given in capital letters. 

The editor is written in uLisp. To invoke it type

(se:sedit)  or  (se:sedit 'symbol) where "symbol" can be any symbol name already present in uLisp



Based on ulisp-arm by David Johnson-Davies:
# ulisp-arm
A version of the Lisp programming language for boards based on the ARM processor:

* Arduino Zero and MKRZero.
* Arduino Uno R4 Minima and WiFi.
* Adafruit ItsyBitsy M0, Feather M0, and Gemma M0.
* Adafruit Metro M4, ItsyBitsy M4, Feather M4, and Grand Central M4.
* Adafruit PyBadge and PyGamer.
* Adafruit CLUE and ItsyBitsy nRF52840.
* Raspberry Pi Pico and Raspberry Pi Pico W.
* BBC Micro Bit.
* Maxim MAX32620FTHR.
* Teensy 4.0/4.1.

For more information see: http://www.ulisp.com/
