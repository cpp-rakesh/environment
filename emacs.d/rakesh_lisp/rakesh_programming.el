;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the custom init file for emacs
;; Author Rakesh Kumar @ cpp.rakesh@gmail.com
;; This file has all the stuff related to programming
;; 27/10/2017
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; set c-mode for all .go and .cmd type files and awk
(setq auto-mode-alist (cons '("\\.text$" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.txt$" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.doc$" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.awk$" . awk-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.perl$" . perl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pl$" . perl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.C$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cpp$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cxx$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tcl$" . tcl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.luax$" . lua-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md$" . markdown-mode) auto-mode-alist))

;; C/C++ mode and I like linux style of indendation
(require 'cc-mode)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-indent 4)
(setq sh-basic-offset 4)
(setq-default c-basic-offset 4 c-default-style "linux")

;; getrid of trailing whitespaces
(require 'whitespace)
(setq-default show-trailing-whitespace t)

;; makefile tabs setup
(defun my-tabs-makefile-hook ()
  (setq indent-tabs-mode t))
(add-hook 'makefile-mode-hook 'my-tabs-makefile-hook)

;; ggtags
(require 'ggtags)
(autoload 'gtags-mode "gtags" "" t)

(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(global-set-key (kbd "C-g") nil)
(global-set-key (kbd "C-g g s") 'ggtags-find-other-symbol)
(global-set-key (kbd "C-g g h") 'ggtags-view-tag-history)
(global-set-key (kbd "C-g g r") 'ggtags-find-reference)
(global-set-key (kbd "C-g g f") 'ggtags-find-file)
(global-set-key (kbd "C-g g c") 'ggtags-create-tags)
(global-set-key (kbd "C-g g u") 'ggtags-update-tags)
(global-set-key (kbd "M-,") 'pop-tag-mark)

;; ggtags: don't whack my useful key bindings!
(eval-after-load 'ggtags
  '(progn
     (define-key ggtags-navigation-map (kbd "M-<") nil)
     (define-key ggtags-navigation-map (kbd "M->") nil)))

;; auto complete feature
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(with-eval-after-load 'go-mode
     (require 'go-autocomplete))

;; auto pair settings for pairing up the braces
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

;; On the fly syntax checker
(global-flycheck-mode)
