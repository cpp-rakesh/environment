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
;;(load-theme 'intellij t)
;;(load-theme 'tsdh-light t)
;;(load-theme 'adwaita t)
;;(load-theme 'espresso t)
;;(load-theme 'tango-plus t)
;;(load-theme 'whiteboard t)
(load-theme 'spacemacs-light t)
;;(load-theme 'hydandata-light t)
;;(load-theme 'organic-green t)
;;(load-theme 'modus-operandi t)
;;(load-theme 'vs-light t)
;;(load-theme 'hemera t)


;; Light background themes
;;(load-theme 'anti-zenburn t)
;;(load-theme 'alect-light t)
;;(load-theme 'alect-light-alt t)
;;(load-theme 'flatui t)
;;(load-theme 'dichromacy t)

;; Black background themes
;;(load-theme 'zenburn t)
;;(load-theme 'monokai t)
;;(load-theme 'sanityinc-tomorrow-bright t)
;;(load-theme 'alect-black t)
;;(load-theme 'alect-black t)
;;(load-theme 'grandshell t)
;;(load-theme 'spacemacs-dark t)

