;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the custom init file for emacs
;; Author Rakesh Kumar @ cpp.rakesh@gmail.com
;; This file will load the emacs theme
;; 27/10/2017
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package) ;; You might already have this line
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize) ;; You might already have this line
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize) ;; You might already have this line

;; Try using MELPA color theme.
;; White Background themes
;;(load-theme 'leuven t)
;;(load-theme 'solarized-light t)
(load-theme 'intellij t)

;; Black background themes
;;(load-theme 'zenburn t)
;;(load-theme 'monokai t)
