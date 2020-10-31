;;; Commentary:

;; This file builds upon lilypond-mode to enable output to an ALSA
;; MIDI synthesizer daemon and input from a physical or virtual MIDI
;; controller.

;; Ideas taken from: https://elpa.gnu.org/packages/midi-kbd.html

;; == Dependencies ==

;; * lilypond-mode (usually installed with the lilypond system package)
;; * A MIDI synthesizer daemon (see instructions for qsynth below)
;; * A MIDI controller (physical or virtual)
;; * alsa-utils (for the aseqdump, aconnect, and aplaymidi commands)

;; == Setting up MIDI Synthesizer ==

;; The simplest setup is to use fluidsynth which can be controlled
;; by the qsynth gui.

;; 1. Install the `qsynth` package
;; 2. Run qsynth, and set the following settings under Setup
;;    1. MIDI -> MIDI Driver: alsa_seq
;;    2. Audio -> Audio Driver: pulseaudio
;; 4. Change soundfonts and instruments as desired

;; == Setting up MIDI Controller ==

;; Either plug in your physical MIDI controller or run a virtual
;; MIDI controller (such as VMPK or Virtual MIDI Keyboard). To test
;; your synthesizer and controller, you can connect the controller's
;; output to the synthesizer's input (which create sound when you
;; play the controller):

;; 1. Find the FluidSynth (or other) synthesizer input port by
;;    running `aconnect -i` (e.g. "128:0")
;; 2. Find the controller output port by running `aconnect -o`
;;    (e.g. "24:0")
;; 3. Connect the devices with `aconnect <output-port> <input-port>`
;; 4. Verify the connection with `aconnect -l` and by playing the
;;    controller

;; == Setting up Emacs ==

;; 1. Open a lilypond file
;; 2. Run "M-x midi-connect" and provide the MIDI ports you used
;;    in the previous step. You can leave the input port blank if
;;    you don't want to have MIDI input to Emacs
;; 3. When the sustain pedal is down and your cursor is in a
;;    lilypond-mode buffer, the MIDI controller will enter lilypond
;;    notes
;; 4. You can change the enharmonics entered by the MIDI controller
;;    by running "M-x midi-select-key" and entering a key-name.
;;    The default key is "c"
;; 4. Compile the lilypond PDF and MIDI file with "C-c C-l"
;; 5. Play the MIDI file on your MIDI synthesizer with "C-c <return>"
;; 6. Stop the MIDI output with "C-c C-g" or "M-x midi-stop"
;; 7. Disconnect the MIDI controller input with "M-x midi-disconnect"
;; 8. Set default ports for next time with the customizable variables
;;    below.

;;; Code:

;; Customizable variables

(defcustom default-midi-input-port nil
  "Default port used to receive MIDI input. E.g. 24:0. List
available ports with `aconnect -o`''"
  :group 'LilyPond
  :type 'string)

(defcustom default-midi-output-port "128:0"
   "Default port used to play MIDI files. E.g. 128:0. List available ports
with `aconnect -o`''"
   :group 'LilyPond
   :type 'string)

;; Constants

(defconst key-notes (make-hash-table)
  "A map of (major/minor) keys to the enharmonics for that key.")
;; See chromatic scales:
;; https://www.basicmusictheory.com/g-chromatic-scale
(defconst sharp-notes '("c" "cis" "d" "dis" "e" "f" "fis" "g" "gis" "a" "ais" "b"))
(defconst flat-notes '("c" "des" "d" "ees" "e" "f" "ges" "g" "aes" "a" "bes" "b"))
(puthash 'c   sharp-notes key-notes)
(puthash 'cis sharp-notes key-notes)
(puthash 'des flat-notes key-notes)
(puthash 'd   sharp-notes key-notes)
(puthash 'dis sharp-notes key-notes)
(puthash 'ees flat-notes key-notes)
(puthash 'e   sharp-notes key-notes)
(puthash 'f   flat-notes key-notes)
(puthash 'fis sharp-notes key-notes)
(puthash 'ges flat-notes key-notes)
(puthash 'g   sharp-notes key-notes)
(puthash 'gis sharp-notes key-notes)
(puthash 'aes flat-notes key-notes)
(puthash 'a   sharp-notes key-notes)
(puthash 'ais sharp-notes key-notes)
(puthash 'bes flat-notes key-notes)
(puthash 'b   sharp-notes key-notes)

;; Stateful variables

(defvar midi-record nil
  "Whether the sustain pedal is pressed to start entering MIDI
  input into lilypond files.")

(defvar key 'c
  "The current key for choosing enharmonics of MIDI controller input.")

(defvar midi-input-port default-midi-input-port
  "The MIDI port to receive MIDI events from.")
(defvar midi-output-port default-midi-output-port
  "The MIDI port to play compiled MIDI files on.")

;; MIDI Input Processing

;; Treat the different MIDI keystroke events as mouse-clicks so that
;; they can store payloads of parameters.
(dolist (event '(MIDI_ON MIDI_OFF MIDI_CC))
  (put event 'event-kind 'mouse-click))

(defun trigger-event (event channel val1 val2)
  "Trigger a keystroke event of type 'event' prefixed by channel
and containing numeric parameters val1 and val2."
  (let* ((channel-num (+ 1 (string-to-number channel)))
         (channel-sym (make-symbol (concat "Ch" (number-to-string channel-num))))
         (vals (mapcar 'string-to-number (list val1 val2))))
    (setq unread-command-events
          (append unread-command-events
                  (list (list event (list nil channel-sym vals)))))))

(defun midi-input-filter (proc string)
  "Process an incoming stream of MIDI events from an 'aseqdump'
process and trigger keystroke events."
  (let* ((lines (split-string string "\n")))
    (dolist (line lines)
      (cond ((string-match ".*Note on.*\\([0-9+]\\), note \\([0-9]+\\).*velocity \\([0-9+]\\).*" line)
             (trigger-event 'MIDI_ON (match-string 1 line) (match-string 2 line) (match-string 3 line)))
            ((string-match ".*Note off.*\\([0-9+]\\), note \\([0-9]+\\).*velocity \\([0-9+]\\).*" line)
             (trigger-event 'MIDI_OFF (match-string 1 line) (match-string 2 line) (match-string 3 line)))
            ((string-match ".*Control change.*\\([0-9+]\\), controller \\([0-9]+\\).*value \\([0-9]+\\).*" line)
             (trigger-event 'MIDI_CC (match-string 1 line) (match-string 2 line) (match-string 3 line)))))))

(defun midi-listen ()
  "Start an aseqdump process to capture MIDI events from."
  (start-process "midi-input-listener" nil "aseqdump" "-p" midi-input-port)
  (set-process-filter (get-process "midi-input-listener") 'midi-input-filter))

;; Interactive commands

(defun midi-disconnect ()
  "Stop the aseqdump process listening to MIDI input events."
  (interactive)
  (when (get-process "midi-input-listener")
    (kill-process "midi-input-listener")))

(defun midi-connect (new-midi-input-port new-midi-output-port)
  ""
  (interactive (list (read-from-minibuffer "MIDI input port: "
                                           nil nil nil nil
                                           (or midi-input-port
                                               default-midi-input-port))
                     (read-from-minibuffer "MIDI output port: "
                                           nil nil nil nil
                                           (or midi-output-port
                                               default-midi-output-port))))
  ;; Disconnect from any existing inputs.
  (midi-disconnect)
  (setq midi-input-port new-midi-input-port)
  (when midi-input-port
    (midi-listen))
  (setq midi-output-port new-midi-output-port)
  ;; These commands will play MIDI files through the synthesizer at
  ;; the configured output port.
  (setq LilyPond-all-midi-command (concat "aplaymidi -p " midi-output-port))
  (setq LilyPond-midi-command (concat "aplaymidi -p " midi-output-port)))

(defun midi-select-key (new-key)
  "Change the key for determining MIDI input keystroke enharmonics."
  (interactive (list (read-from-minibuffer "Input key: ")))
  (let* ((new-key-sym (intern new-key)))
    (if (gethash new-key-sym key-notes)
        (setq key new-key-sym)
      (message (concat "Invalid lilypond key: " new-key)))))

(defun midi-stop ()
  "Stop any playing MIDI."
  (interactive)
  ;; Stop the running aplaymidi process.
  (when (get-process "compilation")
    (kill-process "compilation"))
  ;; Send an all-note-off CC message to the midi output port.
  (shell-command (concat "aplaymidi -p " midi-output-port
                         " ~/.emacs.d/personal/common/all-off.midi")))

;; MIDI Event LilyPond Handlers

(defun midi-on (event)
  "Handle incomining MIDI note-on events."
  (interactive "e")
  (when midi-record
    (let* ((payload (car (cdr event)))
           (midi-note (car (nth 2 payload)))
           (index (mod midi-note 12))
           (notes (gethash key key-notes))
           (note (nth index notes)))
      (insert (concat note " ")))))

(defun midi-off (event)
  "Handle incomining MIDI note-off events."
  (interactive "e"))

(defun midi-cc (event)
  "Handle incoming MIDI control-change events."
  (interactive "e")
  (let* ((payload (car (cdr event)))
         (values (nth 2 payload))
         (key (nth 0 values))
         (value (nth 1 values)))
    (when (eq key 64)
      (setq midi-record (>= value 64)))))

;; Keybindings

(if (require 'lilypond-mode)
    (progn
      (define-key LilyPond-mode-map (kbd "<Ch1> <MIDI_ON>") 'midi-on)
      (define-key LilyPond-mode-map (kbd "<Ch1> <MIDI_OFF>") 'midi-off)
      (define-key LilyPond-mode-map (kbd "<Ch1> <MIDI_CC>") 'midi-cc)
      (define-key LilyPond-mode-map (kbd "C-c C-g") 'midi-stop)))
