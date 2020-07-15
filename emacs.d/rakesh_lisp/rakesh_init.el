;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the custom init file for emacs
;; Author Rakesh Kumar @ cpp.rakesh@gmail.com
;; This file will do all the init stuffs, which I am comfirtable with
;; 27/10/2017
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add packages
(require 'package) ;; You might already have this line
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize) ;; You might already have this line
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize) ;; You might already have this line

;; Because most of the time I use white background and black text
;; Therefore I need this to use light emacs themes
;; Force background color to white for Snow Leopard
;; TODO: revisit this fix when the reason why the background
;; was gray is identified.

(custom-set-faces
 '(default ((t (:stipple nil :background "gray00" :foreground "black"
			 :inverse-video nil :box nil :strike-through nil :overline nil :underline nil
			 :slant normal :weight normal :height 1 :width normal :family "default")))))

;; line number and its formatting
(global-linum-mode t)
(setq linum-format "%4d|  ")

;; Column mode
(setq column-number-mode t)

;; Set up the keyboard so the <delete> key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)

;; Display the time in bar.
(display-time)

;; syntax highlighting by default
(global-font-lock-mode 1)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Scroll down with the cursor,move down the buffer one
;; line at a time, instead of in larger amounts.
(setq scroll-step 1)

;; do not make backup files
(setq make-backup-files nil)
(setq create-lockfiles nil)
     (add-hook 'dired-load-hook
               (lambda ()
                 (require 'dired-x)))
(add-hook 'dired-mode-hook 'dired-omit-toggle)

;; Setting the different keys
(global-set-key (kbd "C-t") 'string-rectangle)


(defun startup-echo-area-message ()                           ; Use a more interesting startup message
  "For the love of Mathematics and Computer Science (ADWITA !!!)")
(set-frame-parameter nil 'alpha 90)                           ; Make the window 90% opaque
(set-scroll-bar-mode 'right)                                  ; Scrollbars should always be on the right
(set-fringe-mode '(1 . 0))                                    ; Turn off the left fringe
(defun major-mode-from-name ()
  "Choose proper mode for buffers created by switch-to-buffer."
  (let ((buffer-file-name (or buffer-file-name (buffer-name))))
    (set-auto-mode)))
(show-paren-mode t)                                           ; Highlight whole parenthetic expressions
(delete-selection-mode t)                                     ; Typed text replaces selection
(global-subword-mode t)                                       ; Treat CamelCase as multiple words
(auto-fill-mode t)                                            ; Automatically wrap lines

;; This is for switching between window frames
(global-set-key (kbd "C-x o") 'ace-window)

;; parenthisis highlight
(show-paren-mode 1)
(setq show-paren-delay 0)

;; key binding for magit start
(global-set-key (kbd "C-x g") 'magit-status)

