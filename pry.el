;;; pry.el --- Interaction with the ruby pry command

;; Copyright (C) 2010, 2011, 2012
;;   Geoff Jacobsen

;; Author: Geoff Jacobsen
;; URL: http://http://github.com/jacott/emacs-pry
;; Created: Sep 18 2010
;; Keywords: languages elisp, ruby
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with it.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

(require 'term)

;; override standard term func
;; (defun term-handle-ansi-escape (proc char)
;;   (cond
;;    ((or (eq char ?H)  ;; cursor motion (terminfo: cup,home)
;; 	;; (eq char ?f) ;; xterm seems to handle this sequence too, not
;; 	;; needed for now
;; 	)
;;     (when (<= term-terminal-parameter 0)
;;       (setq term-terminal-parameter 1))
;;     (when (<= term-terminal-previous-parameter 0)
;;       (setq term-terminal-previous-parameter 1))
;;     (when (> term-terminal-previous-parameter term-height)
;;       (setq term-terminal-previous-parameter term-height))
;;     (when (> term-terminal-parameter term-width)
;;       (setq term-terminal-parameter term-width))
;;     (term-goto
;;      (1- term-terminal-previous-parameter)
;;      (1- term-terminal-parameter)))
;;    ;; \E[A - cursor up (terminfo: cuu, cuu1)
;;    ((eq char ?A)
;;     (term-handle-deferred-scroll)
;;     (let ((tcr (term-current-row)))
;;       (term-down
;;        (if (< (- tcr term-terminal-parameter) term-scroll-start)
;; 	   ;; If the amount to move is before scroll start, move
;; 	   ;; to scroll start.
;; 	   (- term-scroll-start tcr)
;; 	 (if (>= term-terminal-parameter tcr)
;; 	     (- tcr)
;; 	   (- (max 1 term-terminal-parameter)))) t)))
;;    ;; \E[B - cursor down (terminfo: cud)
;;    ((eq char ?B)
;;     (let ((tcr (term-current-row)))
;;       (unless (= tcr (1- term-scroll-end))
;; 	(term-down
;; 	 (if (> (+ tcr term-terminal-parameter) term-scroll-end)
;; 	     (- term-scroll-end 1 tcr)
;; 	   (max 1 term-terminal-parameter)) t))))
;;    ;; \E[C - cursor right (terminfo: cuf, cuf1)
;;    ((eq char ?C)
;;     (term-move-columns
;;      (max 1
;; 	  (if (>= (+ term-terminal-parameter (term-current-column)) term-width)
;; 	      (- term-width (term-current-column)  1)
;; 	    term-terminal-parameter))))
;;    ;; \E[D - cursor left (terminfo: cub)
;;    ((eq char ?D)
;;     (term-move-columns (- (max 1 term-terminal-parameter))))
;;    ;; \E[G - pry added not handled by term.el
;;    ((eq char ?G)
;;     (forward-line term-terminal-parameter))
;;    ;; \E[J - clear to end of screen (terminfo: ed, clear)
;;    ((eq char ?J)
;;     (term-erase-in-display term-terminal-parameter))
;;    ;; \E[K - clear to end of line (terminfo: el, el1)
;;    ((eq char ?K)
;;     (term-erase-in-line term-terminal-parameter))
;;    ;; \E[L - insert lines (terminfo: il, il1)
;;    ((eq char ?L)
;;     (term-insert-lines (max 1 term-terminal-parameter)))
;;    ;; \E[M - delete lines (terminfo: dl, dl1)
;;    ((eq char ?M)
;;     (term-delete-lines (max 1 term-terminal-parameter)))
;;    ;; \E[P - delete chars (terminfo: dch, dch1)
;;    ((eq char ?P)
;;     (term-delete-chars (max 1 term-terminal-parameter)))
;;    ;; \E[@ - insert spaces (terminfo: ich)
;;    ((eq char ?@)
;;     (term-insert-spaces (max 1 term-terminal-parameter)))
;;    ;; \E[?h - DEC Private Mode Set
;;    ((eq char ?h)
;;     (cond ((eq term-terminal-parameter 4)  ;; (terminfo: smir)
;; 	   (setq term-insert-mode t))
;; 	  ;; ((eq term-terminal-parameter 47) ;; (terminfo: smcup)
;; 	  ;; (term-switch-to-alternate-sub-buffer t))
;; 	  ))
;;    ;; \E[?l - DEC Private Mode Reset
;;    ((eq char ?l)
;;     (cond ((eq term-terminal-parameter 4)  ;; (terminfo: rmir)
;; 	   (setq term-insert-mode nil))
;; 	  ;; ((eq term-terminal-parameter 47) ;; (terminfo: rmcup)
;; 	  ;; (term-switch-to-alternate-sub-buffer nil))
;; 	  ))

;;    ;; Modified to allow ansi coloring -mm
;;    ;; \E[m - Set/reset modes, set bg/fg
;;    ;;(terminfo: smso,rmso,smul,rmul,rev,bold,sgr0,invis,op,setab,setaf)
;;    ((eq char ?m)
;;     (when (= term-terminal-more-parameters 1)
;;       (when (>= term-terminal-previous-parameter-4 0)
;; 	(term-handle-colors-array term-terminal-previous-parameter-4))
;;       (when (>= term-terminal-previous-parameter-3 0)
;; 	(term-handle-colors-array term-terminal-previous-parameter-3))
;;       (when (>= term-terminal-previous-parameter-2 0)
;; 	(term-handle-colors-array term-terminal-previous-parameter-2))
;;       (term-handle-colors-array term-terminal-previous-parameter))
;;     (term-handle-colors-array term-terminal-parameter))

;;    ;; \E[6n - Report cursor position (terminfo: u7)
;;    ((eq char ?n)
;;     (term-handle-deferred-scroll)
;;     (process-send-string proc
;; 			 ;; (terminfo: u6)
;; 			 (format "\e[%s;%sR"
;; 				 (1+ (term-current-row))
;; 				 (1+ (term-horizontal-column)))))
;;    ;; \E[r - Set scrolling region (terminfo: csr)
;;    ((eq char ?r)
;;     (term-set-scroll-region
;;      (1- term-terminal-previous-parameter)
;;      (1- term-terminal-parameter)))
;;    (t)))

(defcustom pry-program-name "pry"
  "Program invoked by the `run-pry' command."
  :group 'eri)

(defvar pry-raw-map nil "Keymap used in pry mode.")

(defvar pry-intercept-command nil "Keymap used in pry mode.")

(defvar pry-target-mark nil "Last target of pry-intercept.")

(defvar pry-cursor-overlay nil "Overlay for process-mark location using `vcursor' face")
(defvar pry-last-prompt (make-marker) "position of last displayed prompt")
(set-marker-insertion-type pry-last-prompt nil)

(defvar pry-source-location-regexp "^From: \\(.*\\.rb\\) @ line \\([0-9]+\\)" "Source location regular expression")
(defvar pry-whereami (make-marker) "Location of last pry location")

(if pry-raw-map
    nil
  (setq pry-raw-map (copy-keymap term-raw-map))
  (define-key pry-raw-map [remap self-insert-command] 'pry-send-raw)
  (define-key pry-raw-map [remap term-send-raw] 'pry-send-raw)
  (define-key pry-raw-map [remap term-send-right] 'pry-send-right)
  (define-key pry-raw-map [remap term-send-left] 'pry-send-left)
  (define-key pry-raw-map [remap term-send-del] 'pry-send-del)
  (define-key pry-raw-map [remap term-send-backspace] 'pry-send-backspace)
  (define-key pry-raw-map [remap term-send-raw-meta] 'pry-send-raw-meta)
  (define-key pry-raw-map "\C-x" 'Control-X-prefix)
  (define-key pry-raw-map "\M-x" 'execute-extended-command))



(defun run-pry (cmd)
  "Run an inferior Pry process, input and output via buffer *pry*.
If there is a process already running in `*pry*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `pry-program-name').
\(See `pry-mode' for a list of commands.)"

  (interactive (list (if current-prefix-arg
                         (read-string "Run Pry: " pry-program-name)
                         pry-program-name)))



  (with-current-buffer (get-buffer-create "*pry*")
    (let ((proc (get-buffer-process (current-buffer))))
      (unless (and proc (equal (process-status proc) 'run))
        (let* ((cmdlist (split-string cmd))
               (process-connection-type t)
               ;; We should suppress conversion of end-of-line format.
               (inhibit-eol-conversion t)
               ;; The process's output contains not just chars but also binary
               ;; escape codes, so we need to see the raw output.  We will have to
               ;; do the decoding by hand on the parts that are made of chars.
               (coding-system-for-read 'binary))
        (if (string= pry-program-name cmd)
            (setq cmdlist (apply 'list (car cmdlist) (concat "-r" (pry-source-dir) "ruby/emacs_pry.rb") (cdr cmdlist))))



        (setq proc (apply 'start-process "inferior-pry-process"
                          (current-buffer)
                          (car cmdlist)
                          (cdr cmdlist)))
        (set-process-sentinel proc 'pry-process-sentinel)
        (set-process-query-on-exit-flag proc nil)
        (pry-setup-buffer proc)
        (set-process-filter proc 'pry-filter)))

      (pop-to-buffer (current-buffer))
      proc)))

(defun pry-setup-buffer (proc)
  (pry-mode)
  (goto-char (point-max))
  (set-marker (process-mark proc) (point))
  (term-char-mode)
  (set-process-coding-system proc 'binary 'binary))


(defun pry-process-sentinel (proc msg)
;  (when (eq (current-buffer) (get-buffer "*pry*"))
  (when (memq (process-status proc) '(exit signal))
    (term-line-mode))
  (insert msg))


(defun pry-filter (proc response)
  (with-current-buffer (process-buffer proc)
    (let ((buffer-read-only nil)
          (buffer-undo-list t))
      (term-emulate-terminal proc response))
    (pry-search-for-source)))

(defun pry-select-file (file line)
  (with-selected-window (selected-window)
    (let* ((file (expand-file-name file))
           (newbuf (find-buffer-visiting file))
           (oldbuf (marker-buffer pry-whereami))
           view-window)


      (when oldbuf
        (when (setq view-window (get-buffer-window oldbuf))
          (select-window view-window)))

      (if newbuf
          (unless view-window
            (if (get-buffer-window newbuf)
                (select-window (get-buffer-window newbuf))
              (switch-to-buffer-other-window newbuf)))

        (if view-window
            (view-file file)
          (view-file-other-window file))
        (setq newbuf (current-buffer)))

      (when (and oldbuf (not (eq oldbuf newbuf)))
        (with-current-buffer oldbuf
          (when view-mode
            (View-quit))))

      (with-current-buffer newbuf
        (goto-char (point-min))
        (forward-line (1- line))
        (move-marker pry-whereami (point))))))

(defun pry-search-for-source ()
  (save-excursion
    (goto-char pry-last-prompt)
    (when (search-forward-regexp pry-source-location-regexp nil t)
      (pry-select-file
       (match-string-no-properties 1)
       (let ((line (string-to-number (match-string-no-properties 2))))
         (if (search-forward-regexp "^ => +\\([0-a]+\\):" nil t)
             (string-to-number (match-string-no-properties 1))
           line))))

    (when (search-forward-regexp term-prompt-regexp nil t)
      (move-marker pry-last-prompt (point)))))


(defun pry-send-raw ()
  "Runs `term-send-raw' but first tries to align process-mark to point"
  (interactive)
  (pry-align-process-mark)
  (term-send-raw))

(defun pry-align-process-mark ()
  "Move the Pry process cursor left or right to align with point"
  (let* ((pt (marker-position (process-mark (get-buffer-process (current-buffer)))))
         (readline-pos (save-excursion
                         (goto-char (point-max))
                         (forward-line (if (looking-back "^$" (- (point-max) 2)) -1 0))

                         (when (looking-at term-prompt-regexp)
                           (goto-char (match-end 0)))
                         (if (>= pt (point))
                             (point)
                           nil))))
    (unless (or (eq pt (point)) (not readline-pos) (< (point) readline-pos))
      (if (< pt (point))
          (term-send-raw-string (format "\e%d\eOC" (- (point) pt)))
        (term-send-raw-string (format "\e%d\eOD" (- pt (point))))))))

(defun pry-send-right () (interactive) (pry-align-process-mark) (term-send-right))
(defun pry-send-left  () (interactive) (pry-align-process-mark) (term-send-left))
(defun pry-send-del   () (interactive) (pry-align-process-mark) (term-send-del))
(defun pry-send-backspace  () (interactive) (pry-align-process-mark) (term-send-backspace))
(defun pry-send-raw-meta () (interactive) (pry-align-process-mark) (term-send-raw-meta))


(defun pry-mode ()
  "Major mode for interacting with an inferior ruby (pry) process.

\\{pry-raw-map}"
  (interactive)
  (term-mode)
  (setq term-scroll-show-maximum-output nil)
  (setq term-scroll-to-bottom-on-output t)
  (setq major-mode 'pry-mode)
  (setq mode-name "Pry")
  (make-local-variable 'term-raw-map)
  (move-marker pry-last-prompt (point-min))
  (setq term-raw-map pry-raw-map)
  ;; (make-local-variable 'pry-cursor-overlay)
  ;; (when pry-cursor-overlay (delete-overlay pry-cursor-overlay))
  ;; (setq pry-cursor-overlay (make-overlay (point-max) (point-max) nil t nil))
  ;; (overlay-put pry-cursor-overlay 'face 'vcursor)
  (set (make-local-variable 'term-prompt-regexp) "^\\[[0-9]+\\] pry(.*)[>*] "))


(defvar pry-source-dir nil "Private variable.")

(defun pry-source-dir ()
  (or pry-source-dir
    (setq pry-source-dir (file-name-directory (find-lisp-object-file-name
                                               'pry-source-dir (symbol-function 'pry-source-dir))))))


(defun pry-intercept (arg &optional break-type)
  "Run ruby program intercepting using pry on the current line.

With no prefix argument, use value from last run.
With prefix argument of:
   0,   edit command using previous command as default
   ^U,  edit command using current buffer as default
   string, Run pry-intercept with value of string

The optional argument BREAK-TYPE will set the pry breakpoint. for:
   nil, use the current buffer and line
   'rerun, use the buffer and line of the `pry-intercept'
   'nonstop, don't set a breakpoint

See `pry-intercept-nonstop' and `pry-intercept-rerun'"
  (interactive "P")
  (let ((command
         (if (or (and arg (not (stringp arg))) (not (or arg pry-intercept-command)))
             (read-string "Run Pry: " (if (and pry-intercept-command (eq 0 arg))
                                          pry-intercept-command
                                        (concat "ruby -I. " (buffer-file-name))))
           (if (stringp arg) arg))))

    (if command
        (setq pry-intercept-command command)
      (setq command pry-intercept-command))

    (let* ((proc-buffer (get-buffer "*pry*"))
           (proc (get-buffer-process proc-buffer))
           (process-environment process-environment)
           (main-prog-start (and (or (string-match " -- \\([^ ]+\\.rb\\)" command) (string-match " \\([^- ][^ ]*\\.rb\\)" command)) (match-beginning 1)))
           (pid (number-to-string (emacs-pid)))
           (fn (concat "/tmp/emacs_pry_" pid ".rb"))
           source)

      (unless (eq break-type 'rerun)
        (if (and (> (length buffer-file-name) 3) (string= (substring (buffer-file-name) -3) ".rb"))
            (if pry-target-mark
                (move-marker pry-target-mark (line-beginning-position) (current-buffer))
              (setq pry-target-mark (copy-marker (line-beginning-position))))
          (error "pry-intercept can only be run on ruby buffers")))

      (with-current-buffer (marker-buffer pry-target-mark)
        (unless (eq break-type 'nonstop)
          (setq source (concat (buffer-substring 1 (marker-position pry-target-mark)) "binding.pry;" (buffer-substring (marker-position pry-target-mark) (1+ (buffer-size)))))

          (write-region source nil fn)

          (unless main-prog-start
            (error "Can't find ruby program name: %s" command))

          (setenv "_EMACS_MONKEY_PATCH" (concat pid ":" (buffer-file-name))))


        (setq command (concat (substring command 0 main-prog-start) (pry-source-dir) "ruby/emacs_pry.rb " (substring command main-prog-start))))


      (when (and proc (equal (process-status proc) 'run))
        (set-process-sentinel proc nil)
        (kill-process proc)
        (accept-process-output proc 0.5))
      (when proc-buffer
        (with-current-buffer proc-buffer
          (setq buffer-undo-list t)
          (erase-buffer)
          (setq buffer-undo-list nil)))

      (run-pry command))))

(defun pry-intercept-nonstop (arg &optional break-type)
  "Same as `pry-intercept' but don't set pry breakpoint by default"
  (interactive "P")
  (pry-intercept arg (or break-type 'nonstop)))

(defun pry-intercept-rerun (arg &optional break-type)
  "Same as `pry-intercept' but rerun last intercept by default"
  (interactive "P")
  (pry-intercept arg (or break-type 'rerun)))

(provide 'pry)
