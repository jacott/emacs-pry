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

(defcustom pry-program-name "pry"
  "Program invoked by the `run-pry' command."
  :group 'eri)

(defvar pry-raw-map nil "Keymap used in pry mode.")

(defvar pry-intercept-command nil "Keymap used in pry mode.")

(defvar pry-target-mark nil "Last target of pry-intercept.")

(defvar pry-cursor-overlay nil "Overlay for process-mark location using `vcursor' face")

(if pry-raw-map
    nil
  (setq pry-raw-map (copy-keymap term-raw-map))
  (define-key pry-raw-map [remap self-insert-command] 'pry-send-raw)
  (define-key pry-raw-map [remap term-send-raw] 'pry-send-raw)
  (define-key pry-raw-map [remap term-send-right] 'pry-send-right)
  (define-key pry-raw-map [remap term-send-left] 'pry-send-left)
  (define-key pry-raw-map [remap term-send-del] 'pry-send-del)
  (define-key pry-raw-map [remap term-send-backspace] 'pry-send-backspace)
  (define-key pry-raw-map [remap term-send-raw-meta] 'pry-send-raw-meta))



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
          (set-process-coding-system proc 'binary 'binary)
          (set-process-filter proc 'pry-filter)
          (set-process-query-on-exit-flag proc nil)
          (set-process-sentinel proc 'pry-process-sentinel)
          (pry-mode)))
      (pop-to-buffer (current-buffer))
      proc)))


(defun pry-process-sentinel (proc msg)
  (when (eq (current-buffer) (get-buffer "*pry*"))
    (when (memq (process-status proc) '(exit signal))
      (term-line-mode))
    (insert msg)))


(defun pry-filter (proc response)
  (term-emulate-terminal proc response))

(defun pry-send-raw ()
  "Runs `term-send-raw' but first tries to align process-mark to point"
  (interactive)
  (pry-align-process-mark)
  (term-send-raw))

(defun pry-align-process-mark ()
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
  (setq term-scroll-show-maximum-output t)
  (setq term-scroll-to-bottom-on-output t)
  (setq major-mode 'pry-mode)
  (setq mode-name "Pry")
  (make-local-variable 'term-raw-map)
  (setq term-raw-map pry-raw-map)
  ;; (make-local-variable 'pry-cursor-overlay)
  ;; (when pry-cursor-overlay (delete-overlay pry-cursor-overlay))
  ;; (setq pry-cursor-overlay (make-overlay (point-max) (point-max) nil t nil))
  ;; (overlay-put pry-cursor-overlay 'face 'vcursor)
  (set (make-local-variable 'term-prompt-regexp) "^\\[[0-9]+\\] pry(.*)[>*] ")
  (term-char-mode))


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
         (if (or (not pry-intercept-command) (and arg (not (stringp arg))))
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
           (main-prog-start (and (or (string-match " -- \\([^ ]+\\.rb\\)" command) (string-match " \\([^- ][^ ]+\\.rb\\)" command)) (match-beginning 1)))
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
        (unless (eq break-point 'nonstop)
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
          (delete-region 1 (1+ (buffer-size)))))
      
      (run-pry command))))

(defun pry-intercept-nonstop (arg &optional break-point)
  "Same as `pry-intercept' but don't set pry breakpoint by default"
  (interactive "P")
  (pry-intercept arg (or break-point 'nonstop)))

(defun pry-intercept-rerun (arg &optional break-point)
  "Same as `pry-intercept' but rerun last intercept by default"
  (interactive "P")
  (pry-intercept arg (or break-point 'rerun)))

(provide 'pry)

