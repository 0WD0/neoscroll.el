;;; neoscroll.el --- Smooth scrolling for Emacs inspired by neoscroll.nvim -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Yuantao Wang <wd.1105848296@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: scrolling, smooth-scroll, neovim
;; URL: https://github.com/0WD0/neoscroll.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Neoscroll provides smooth scrolling functionality for Emacs,
;; inspired by neoscroll.nvim. It offers customizable easing functions,
;; native compilation support for performance, and seamless integration
;; with Evil mode.

;; Key features:
;; - Multiple easing functions (linear, quadratic, cubic, sine)
;; - Pixel-perfect smooth scrolling
;; - Native compilation for maximum performance
;; - Interrupt handling for responsive input
;; - Evil mode integration
;; - Customizable scroll durations

;;; Code:

(require 'cl-lib)

;;
;;; Configuration

(defgroup neoscroll nil
  "Smooth scrolling for Emacs inspired by neoscroll.nvim"
  :group 'scrolling
  :prefix "neoscroll-")

(defcustom neoscroll-mappings
  '("C-u" "C-d" "C-b" "C-f" "C-y" "C-e")
  "Default key mappings for smooth scrolling."
  :type '(repeat string)
  :group 'neoscroll)

(defcustom neoscroll-hide-cursor nil
  "Hide cursor while scrolling (disabled due to compatibility issues)."
  :type 'boolean
  :group 'neoscroll)

(defcustom neoscroll-stop-eof t
  "Stop at EOF when scrolling downwards."
  :type 'boolean
  :group 'neoscroll)

(defcustom neoscroll-easing 'linear
  "Default easing function."
  :type '(choice (const linear) (const quadratic) (const cubic) (const sine))
  :group 'neoscroll)

(defcustom neoscroll-pre-hook nil
  "Function to run before scrolling animation starts."
  :type '(choice function null)
  :group 'neoscroll)

(defcustom neoscroll-post-hook nil
  "Function to run after scrolling animation ends."
  :type '(choice function null)
  :group 'neoscroll)

(defcustom neoscroll-scroll-duration 0.15
  "Default duration for C-u/C-d scrolling in seconds."
  :type 'float
  :group 'neoscroll)

(defcustom neoscroll-page-duration 0.25
  "Default duration for C-f/C-b page scrolling in seconds."
  :type 'float
  :group 'neoscroll)

(defcustom neoscroll-line-duration 0.025
  "Default duration for C-y/C-e line scrolling in seconds."
  :type 'float
  :group 'neoscroll)

(defcustom neoscroll-line-step 1
  "Number of lines to scroll with C-y/C-e commands."
  :type 'integer
  :group 'neoscroll)

;;
;;; Core variables

(defvar neoscroll--timer nil "Current animation timer")
(defvar neoscroll--active nil "Whether animation is active")
(defvar neoscroll--interrupt-flag nil "Interrupt flag")

;;
;;; Easing functions

(defun neoscroll--easing-linear (progress)
  "Linear easing function."
  progress)

(defun neoscroll--easing-quadratic (progress)
  "Quadratic easing function."
  (* progress progress))

(defun neoscroll--easing-cubic (progress)
  "Cubic easing function."
  (- 1 (expt (- 1 progress) 3)))

(defun neoscroll--easing-sine (progress)
  "Sine easing function."
  (- 1 (cos (* progress (/ pi 2)))))

(defun neoscroll--apply-easing (progress easing)
  "Apply EASING function to PROGRESS."
  (pcase easing
    ('linear (neoscroll--easing-linear progress))
    ('quadratic (neoscroll--easing-quadratic progress))
    ('cubic (neoscroll--easing-cubic progress))
    ('sine (neoscroll--easing-sine progress))
    (_ progress)))

;;
;;; Core functions

(defun neoscroll--interrupt (&rest _)
  "Interrupt current animation."
  (when neoscroll--timer
    (cancel-timer neoscroll--timer)
    (setq neoscroll--timer nil
          neoscroll--active nil
          neoscroll--interrupt-flag t)
    ;; Clean up highlighting when interrupted
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (overlay-get overlay 'hl-line)
        (delete-overlay overlay)))
    (when (or (bound-and-true-p hl-line-mode)
              (bound-and-true-p global-hl-line-mode))
      (run-hooks 'post-command-hook))))

(defun neoscroll--check-input ()
  "Check if there's pending input and interrupt if so."
  (when (and neoscroll--active (input-pending-p))
    (neoscroll--interrupt)
    t))

(defun neoscroll--cursor-hide ()
  "Hide cursor during scrolling."
  nil)

(defun neoscroll--cursor-show ()
  "Show cursor after scrolling."
  nil)

(defun neoscroll--line-pixel-height ()
  "Get line height in pixels."
  (if (display-graphic-p)
      (line-pixel-height)
    1))

(defun neoscroll--window-pixel-height ()
  "Get window height in pixels."
  (if (display-graphic-p)
      (window-pixel-height)
    (window-height)))

(defun neoscroll--scroll-pixels (pixels)
  "Scroll by PIXELS amount."
  (let ((pixels (round pixels)))
    (when (not (zerop pixels))
      (if (and (display-graphic-p) 
               (fboundp 'pixel-scroll-precision-scroll-down))
          (if (> pixels 0)
              (pixel-scroll-precision-scroll-down pixels)
            (pixel-scroll-precision-scroll-up (- pixels)))
        ;; Fallback to line-based scrolling
        (let ((lines (ceiling (/ (float (abs pixels)) (neoscroll--line-pixel-height)))))
          (when (> lines 0)
            (if (> pixels 0)
                (scroll-up lines)
              (scroll-down lines))))))))

;;
;;; Main scrolling function

(defvar neoscroll--target-line 0 "Target line to scroll to")
(defvar neoscroll--relative-line 0 "Current relative line position")
(defvar neoscroll--total-lines 0 "Total lines to scroll")

(defun neoscroll--compute-time-step (lines-to-scroll)
  "Compute time step based on remaining lines and easing function."
  (let* ((lines-range (abs neoscroll--total-lines))
         (duration (or (plist-get neoscroll--current-opts :duration) 0.25))
         (duration-ms (* duration 1000))
         (easing (or (plist-get neoscroll--current-opts :easing) neoscroll-easing)))
    
    (cond
     ((< lines-to-scroll 1) 1000)
     ((eq easing 'linear)
      (max 1 (floor (/ duration-ms (max 1 (1- lines-range))))))
     (t
      (let* ((ef (pcase easing
                   ('quadratic (lambda (x) (- 1 (expt (- 1 x) 0.5))))
                   ('cubic (lambda (x) (- 1 (expt (- 1 x) (/ 1.0 3)))))
                   ('sine (lambda (x) (/ (* 2 (asin x)) pi)))
                   (_ (lambda (x) x))))
             (x1 (/ (float (- lines-range lines-to-scroll)) lines-range))
             (x2 (/ (float (- lines-range lines-to-scroll -1)) lines-range))
             (time-step (floor (* duration-ms (- (funcall ef x2) (funcall ef x1))))))
        (max 1 time-step))))))

(defvar neoscroll--current-opts nil "Current scroll options")

(defun neoscroll-scroll (lines &optional opts)
  "Scroll LINES with options OPTS using neoscroll.nvim algorithm."
  (let* ((opts (or opts '()))
         (move-cursor (if (plist-member opts :move-cursor) 
                          (plist-get opts :move-cursor) 
                        t))
         (info (plist-get opts :info)))
    
    ;; Interrupt any ongoing animation
    (neoscroll--interrupt)
    
    ;; Early exit if no scrolling needed  
    (unless (zerop lines)
      (setq neoscroll--current-opts opts
            neoscroll--total-lines lines
            neoscroll--target-line lines
            neoscroll--relative-line 0)
      
      ;; Run pre-hook
      (when neoscroll-pre-hook
        (funcall neoscroll-pre-hook info))
      
      ;; Hide cursor
      (neoscroll--cursor-hide)
      
      ;; Clear any existing highlight overlays to prevent ghosting
      (when (bound-and-true-p hl-line-mode)
        (hl-line-unhighlight))
      (when (bound-and-true-p global-hl-line-mode)
        (global-hl-line-unhighlight))
      
      ;; Set up animation state
      (setq neoscroll--active t
            neoscroll--interrupt-flag nil)
      
      ;; Start scrolling
      (neoscroll--scroll-one-step move-cursor info))))

(defun neoscroll--lines-to-scroll ()
  "Get remaining lines to scroll."
  (- neoscroll--target-line neoscroll--relative-line))

(defun neoscroll--scroll-one-step (move-cursor info)
  "Scroll one line and schedule next step."
  (let ((lines-to-scroll (neoscroll--lines-to-scroll)))
    (cond
     ;; Check for interruption or completion
     ((or neoscroll--interrupt-flag 
          (neoscroll--check-input)
          (zerop lines-to-scroll))
      (neoscroll--cursor-show)
      ;; Trigger proper highlight refresh
      (when (or (bound-and-true-p hl-line-mode)
                (bound-and-true-p global-hl-line-mode))
        (run-hooks 'post-command-hook))
      (when neoscroll-post-hook
        (funcall neoscroll-post-hook info))
      (setq neoscroll--timer nil
            neoscroll--active nil))
     
     ;; Continue scrolling
     (t
      ;; Scroll one line
      (if move-cursor
          (if (> lines-to-scroll 0)
              (progn (scroll-up 1) (forward-line 1))
            (progn (scroll-down 1) (forward-line -1)))
        (if (> lines-to-scroll 0)
            (scroll-up 1)
          (scroll-down 1)))
      
      ;; Update position
      (setq neoscroll--relative-line (+ neoscroll--relative-line 
                                       (if (> lines-to-scroll 0) 1 -1)))
      
      ;; Force highlight refresh
      (when (bound-and-true-p hl-line-mode)
        (run-hooks 'post-command-hook))
      (when (bound-and-true-p global-hl-line-mode)
        (run-hooks 'post-command-hook))
      
      ;; Schedule next step
      (let* ((remaining-lines (abs (neoscroll--lines-to-scroll)))
             (time-step-ms (neoscroll--compute-time-step remaining-lines))
             (time-step-sec (/ time-step-ms 1000.0)))
        (setq neoscroll--timer 
              (run-with-timer time-step-sec nil 
                             #'neoscroll--scroll-one-step move-cursor info)))))))

;;
;;; Predefined scroll commands

;;;###autoload
(defun neoscroll-ctrl-u (&optional opts)
  "Smooth scroll up half page."
  (interactive)
  (let ((scroll-amount (max 1 (/ (window-height) 2))))
    (neoscroll-scroll (- scroll-amount)
                     (append opts `(:duration ,neoscroll-scroll-duration)))))

;;;###autoload
(defun neoscroll-ctrl-d (&optional opts)
  "Smooth scroll down half page."
  (interactive)
  (let ((scroll-amount (max 1 (/ (window-height) 2))))
    (neoscroll-scroll scroll-amount
                     (append opts `(:duration ,neoscroll-scroll-duration)))))

;;;###autoload
(defun neoscroll-ctrl-b (&optional opts)
  "Smooth scroll up full page."
  (interactive)
  (neoscroll-scroll (- (window-height))
                   (append opts `(:duration ,neoscroll-page-duration))))

;;;###autoload
(defun neoscroll-ctrl-f (&optional opts)
  "Smooth scroll down full page."
  (interactive)
  (neoscroll-scroll (window-height)
                   (append opts `(:duration ,neoscroll-page-duration))))

;;;###autoload
(defun neoscroll-ctrl-y (&optional opts)
  "Smooth scroll up by configured line step."
  (interactive)
  (neoscroll-scroll (- neoscroll-line-step)
                   (append opts `(:duration ,neoscroll-line-duration :move-cursor nil))))

;;;###autoload
(defun neoscroll-ctrl-e (&optional opts)
  "Smooth scroll down by configured line step."
  (interactive)
  (neoscroll-scroll neoscroll-line-step
                   (append opts `(:duration ,neoscroll-line-duration :move-cursor nil))))

;;
;;; Setup function

;;;###autoload
(defun neoscroll-setup (&optional config)
  "Setup neoscroll with CONFIG options."
  (when config
    (cl-loop for (key . value) in config do
             (set (intern (concat "neoscroll-" (symbol-name key))) value)))
  
  ;; Enable pixel scrolling if available
  (when (and (display-graphic-p)
             (fboundp 'pixel-scroll-precision-mode))
    (pixel-scroll-precision-mode 1)))
  
  ;; For non-Evil users, don't override standard Emacs keybindings
  ;; Users can manually bind keys if they want smooth scrolling

;;
;;; Evil integration

(defun neoscroll--setup-evil-integration ()
  "Setup Evil mode integration."
  (when (featurep 'evil)
    ;; Override default evil movement commands
    (advice-add 'evil-next-line :before #'neoscroll--interrupt)
    (advice-add 'evil-previous-line :before #'neoscroll--interrupt)
    (advice-add 'evil-forward-char :before #'neoscroll--interrupt)
    (advice-add 'evil-backward-char :before #'neoscroll--interrupt)
    
    ;; Emergency stop
    (defun neoscroll-emergency-stop ()
      "Emergency stop all animations."
      (interactive)
      (neoscroll--interrupt)
      (keyboard-quit))
    
    ;; Set up Evil keybindings
    (when (fboundp 'evil-define-key*)
      ;; Advice Evil's scroll commands to use neoscroll
      (advice-add 'evil-scroll-down :override #'neoscroll-ctrl-d)
      (advice-add 'evil-scroll-up :override #'neoscroll-ctrl-u)
      (advice-add 'evil-scroll-page-down :override #'neoscroll-ctrl-f)
      (advice-add 'evil-scroll-page-up :override #'neoscroll-ctrl-b)
      (advice-add 'evil-scroll-line-up :override #'neoscroll-ctrl-y)
      (advice-add 'evil-scroll-line-down :override #'neoscroll-ctrl-e))))

(defun neoscroll--remove-evil-integration ()
  "Remove Evil mode integration."
  (when (featurep 'evil)
    (advice-remove 'evil-next-line #'neoscroll--interrupt)
    (advice-remove 'evil-previous-line #'neoscroll--interrupt)
    (advice-remove 'evil-forward-char #'neoscroll--interrupt)
    (advice-remove 'evil-backward-char #'neoscroll--interrupt)
    ;; Remove scroll command advice
    (advice-remove 'evil-scroll-down #'neoscroll-ctrl-d)
    (advice-remove 'evil-scroll-up #'neoscroll-ctrl-u)
    (advice-remove 'evil-scroll-page-down #'neoscroll-ctrl-f)
    (advice-remove 'evil-scroll-page-up #'neoscroll-ctrl-b)
    (advice-remove 'evil-scroll-line-up #'neoscroll-ctrl-y)
    (advice-remove 'evil-scroll-line-down #'neoscroll-ctrl-e)))

;;
;;; Minor mode

;;;###autoload
(define-minor-mode neoscroll-mode
  "Smooth scrolling minor mode."
  :global t
  :group 'neoscroll
  :lighter " Neo"
  (if neoscroll-mode
      (progn
        (neoscroll-setup)
        (neoscroll--setup-evil-integration))
    ;; Cleanup when disabling
    (neoscroll--interrupt)
    (neoscroll--remove-evil-integration)
    (dolist (mapping neoscroll-mappings)
      (global-unset-key (kbd mapping)))))

(provide 'neoscroll)

;;; neoscroll.el ends here
