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

(defvar pry-mode-map nil "Keymap used in pry mode.")

(defvar pry-intercept-command nil "Keymap used in pry mode.")

(defvar pry-target-buffer nil "Last target buffer of pry-intercept.")
(defvar pry-target-point nil "Last target buffer point of pry-intercept.")


(if pry-mode-map
    nil
  (setq pry-mode-map (copy-keymap term-mode-map)))

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

(defun pry-mode ()
  "Major mode for interacting with an inferior ruby (pry) process.

\\{pry-mode-map}"
  (interactive)
  (term-mode)
  (setq term-scroll-show-maximum-output t)
  (setq term-scroll-to-bottom-on-output t)
  (use-local-map pry-mode-map)
  (setq major-mode 'pry-mode)
  (setq mode-name "Pry")
  (make-local-variable 'term-raw-map)
  (set (make-local-variable 'term-prompt-regexp) "^\\[[0-9]+\\] pry(.*)[>*] ")
  (define-key term-raw-map "\C-x" 'Control-X-prefix)
  (term-char-mode))


(defvar pry-source-dir nil "Private variable.")

(defun pry-source-dir ()
  (or pry-source-dir
    (setq pry-source-dir (file-name-directory (find-lisp-object-file-name 
                                               'pry-source-dir (symbol-function 'pry-source-dir))))))


(defun pry-intercept (command &optional nonstop)
  "Run ruby program intercepting using pry on the current line.

With argument, allows you to edit the command line:
  for argument:
    0   edit command using previous command as default
    ^U  edit command using current buffer as default

Without argument, runs the last command.

Use `pry-intercept-nonstop' if pry breakpoint is not wanted"
  (interactive "P")

  (when (or (not pry-intercept-command) (and command (not (stringp command))))
    (setq command 
          (read-string "Run Pry: " (if (and pry-intercept-command (eq 0 command))
                                       pry-intercept-command
                                     (concat "ruby -I. " (buffer-file-name))))))

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
    
    (when (and (> (length buffer-file-name) 3) (string= (substring (buffer-file-name) -3) ".rb"))
      (setq pry-target-buffer (current-buffer))
      ;; fixme use property on buffer rather than pry-target-point because lines get deleted/added
      (setq pry-target-point (line-beginning-position)))

    (unless pry-target-buffer (error "pry-intercept can only be run on ruby buffers"))

    (with-current-buffer pry-target-buffer
      (when (> pry-target-point (1+ (buffer-size)))
        (setq pry-target-point (1+ (buffer-size))))
      (unless nonstop
        (setq source (concat (buffer-substring 1 pry-target-point) "binding.pry;" (buffer-substring pry-target-point (1+ (buffer-size)))))

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
      
    (run-pry command)))

(defun pry-intercept-nonstop (command)
  "Same as `pry-intercept' but don't set pry breakpoint"
  (interactive (list (if current-prefix-arg
                         (read-string "Run Pry: " (concat "ruby -I. " (buffer-file-name)))
                       pry-intercept-command)))

  (pry-intercept command t))


(provide 'pry)

