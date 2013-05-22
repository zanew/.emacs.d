;;; -*- Mode: Emacs-Lisp -*-
;;; Zane Whitney's Emacs Settings
;;;(using some parts of Jeff Dlouhy's .emacs file: https://github.com/jeffd)

(message "started loading settings ...")

(setq custom-basedir (expand-file-name "~/.emacs.d/site-lisp"))
(add-to-list 'load-path custom-basedir)

(defun add-path (p)
  (add-to-list 'load-path (concat custom-basedir p)))

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/Applications/Adobe Flash Builder 4/sdks/4.0.0/bin")

;;; LAYOUT

;;; Will remove when there is a true GNU Operating System
(setq inhibit-start-screen 1)
(setq inhibit-splash-screen 1)

(setq visible-bell nil)

;;; Mac keyboard on Linux
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

;;; Locate command uses Spotlight Search
(setq locate-command "/usr/bin/mdfind")

;;; I condem thee to Hell!
;;(global-set-key (kbd "C-x C-c") nil)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;;; Smooth Scrolling
(message "applying scrolling settings ...")
(setq scroll-step 1
      scroll-conservatively 10000)

;;; Scrolling
(global-set-key [C-next] 'scroll-other-window)
(global-set-key [C-prior] 'scroll-other-window-down)

;;; Yes or No prompts shorter
(fset 'yes-or-no-p 'y-or-n-p)

;;; For Panic's Prompt app. Enables the backspace key, if using Emacs over ssh.
(cond ((getenv "SSH_CONNECTION")
       (define-key key-translation-map [?\C-h] [?\C-?])))

;;; Oh god why...
;; (let ((translations '( 229 [?\M-a]  nil [?\M-b]   231 [?\M-c]  8706 [?\M-d]   nil [?\M-e]
;;                        402 [?\M-f]  169 [?\M-g]   729 [?\M-h]   nil [?\M-i]  8710 [?\M-j]
;;                        730 [?\M-k]  172 [?\M-l]   181 [?\M-m]   nil [?\M-n]   248 [?\M-o]
;;                        960 [?\M-p]  339 [?\M-q]   174 [?\M-r]   223 [?\M-s]  8224 [?\M-t]
;;                        nil [?\M-u] 8730 [?\M-v]  8721 [?\M-w]  8776 [?\M-x]   165 [?\M-y]
;;                        937 [?\M-z]
;;                         96 [?\M-~]  161 [?\M-1]   162 [?\M-4]   163 [?\M-3]   167 [?\M-6]
;;                        170 [?\M-9]  171 [?\M-\\]  175 [?\M-<]   176 [?\M-*]   177 [?\M-+]
;;                        182 [?\M-7]  183 [?\M-\(]  186 [?\M-0]   187 [?\M-|]   191 [?\M-\?]
;;                        198 [?\M-\"] 230 [?\M-']   247 [?\M-/]   728 [?\M->]  8211 [?\M-\-]
;;                       8212 [?\M-_] 8216 [?\M-\]] 8217 [?\M-}]  8218 [?\M-\)] 8220 [?\M-\[] 
;;                       8221 [?\M-{] 8225 [?\M-&]  8226 [\?M-8]  8249 [?\M-#]  8250 [?\M-$] 
;;                       8260 [?\M-!] 8364 [\?M-@]  8482 [?\M-2]  8734 [\?M-5]  8800 [?\M-=]
;;                       8804 [?\M-,] 8805 [?\M-.] 64257 [?\M-%] 64258 [?\M-^])))
;;   (while translations
;;     (let ((key (car translations)) (def (cadr translations)))
;;       (if key
;;           (define-key key-translation-map (make-string 1 key) def)))
;;     (setq translations (cddr translations))))


;;; Settings Theme
(message "applying theme settings ...")
(load-theme 'misterioso t)

;;; Fullscreen
;; (cond ((eq system-type 'gnu/linux)
;;        (defun fullscreen ()
;;          (interactive)
;;          (set-frame-parameter nil 'fullscreen
;;                               (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

;;        (global-set-key [f11] 'fullscreen)

;;        (defun switch-full-screen ()
;;          (interactive)
;;          (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

;;        (global-set-key [f11] 'switch-full-screen)))

;; (cond ((eq system-type 'darwin)
;;        (global-set-key [(meta return)] 'ns-toggle-fullscreen)))

;;; Cursor and Line
(message "applying cursor settings ...")
(setq-default cursor-type 'box)
(setq-default show-trailing-whitespace t)
(setq-default transient-mark-mode t)
(setq default-truncate-lines t)


;;; Shell Settings
(message "applying shell settings ...")
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

;;; Hide the toolbar and friends
(message "hiding toolbars, scrollbars, and menubars ...")
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;; Turn on Column Number Mode
(column-number-mode)

;;;CUSTOM BINDINGS

;;;Quick Mode Switches
(message "applying custom key bindings ...")
(global-set-key (kbd "C-c C-o") 'objc-mode)
(global-set-key (kbd "C-c C-f") 'ido-find-file)
(global-set-key (kbd "C-c C-x") 'nxml-mode)
(global-set-key (kbd "C-c C-y") 'yas/minor-mode)
(global-set-key (kbd "C-c c") 'auto-complete-mode)

;;; German Language Settings
(message "German langauge settings ...")
(set-language-environment 'german)
(set-terminal-coding-system 'iso-latin-1)

;; to switch to the previous frame
(defun prev-frame ()
  (interactive)
  (other-frame -1))

;;; Other Bindings
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-~") 'prev-frame)

;;; Making sure the clipboard works
(setq x-select-enable-clipboard t)

;;; TRAMP stuff
(setq tramp-default-method "ssh")

;;; Package Repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;;;EXTENSIONS

;;;Auto Complete Mode
;; (message "Auto Complete Mode ...")
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/emacs-cfg/.emacs.d/site-lisp/ac-dict")
;; (ac-config-default)

;;;Other Yasnippet invocation
 ;; (add-to-list 'load-path
 ;;              "~/.emacs.d/plugins/yasnippet")
 ;; (require 'yasnippet)
 ;; (yas/global-mode 1)

;;;Yasnippet
;;(if (y-or-n-p "Load YASnippet with AutoComplete functionality (Takes awhile): ")
;;  (progn (require 'yasnippet "~/.emacs.d/plugins/yasnippet/yasnippet.el")
   ;; (yas/initialize)
   ;; (setq yas/root-directory "~/.emacs.d/plugins/yasnippet/snippets")
   ;; (yas/load-directory yas/root-directory)))

;;;Using YASnippet with AutoComplete
  ;;  (message "auto-complete-yasnippet load successful")
  ;;  (load "auto-complete-yasnippet.el"))

  ;; (progn (message "Okay..."))

;;; LANGUAGE MODES

;;; Flyspell Mode
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t) (autoload 'tex-mode-flyspell-verify "flyspell" "" t)

;;; Objective-C Settings
(message "applying Xcode settings ...")
(setq auto-mode-alist
      (append '(("\\.mm\\'" . objc-mode)
                ("\\.m\\'" . objc-mode)
                ("\\.j\\'" . objj-mode))
              auto-mode-alist))

;;; Header File Support
;;; http://hutley.net/brett/emacs/integrating-emacs-and-xcode/
(defun xcode-choose-header-mode ()
  (interactive)
  (if (string-equal (substring (buffer-file-name) -2) ".h")
      (progn
        ;; OK, we got a .h file, if a .m file exists we'll assume it's
                                        ; an objective c file. Otherwise, we'll look for a .cpp file.
        (let ((dot-m-file (concat (substring (buffer-file-name) 0 -1) "m"))
              (dot-cpp-file (concat (substring (buffer-file-name) 0 -1) "cpp")))
          (if (file-exists-p dot-m-file)
              (progn
                (objc-mode))
            (if (file-exists-p dot-cpp-file)
                (c++-mode)))))))

(add-hook 'find-file-hook 'xcode-choose-header-mode)

;;; Xcode Build Settings
(cond ((eq system-type 'darwin)
       (defun xcode-compile ()
         (interactive)
         (let ((df (directory-files "."))
               (has-proj-file nil))
           (while (and df (not has-proj-file))
             (let ((fn (car df)))
               (if (> (length fn) 10)
                   (if (string-equal (substring fn -10) ".xcodeproj")
                       (setq has-proj-file t))))
             (setq df (cdr df)))
           (if has-proj-file
               (compile "xcodebuild -configuration Debug")
             (cd "..")
             (xcode-compile))))

       (defun xcode-clean ()
         (interactive)
         (let ((df (directory-files "."))
               (has-proj-file nil))
           (while (and df (not has-proj-file))
             (let ((fn (car df)))
               (if (> (length fn) 10)
                   (if (string-equal (substring fn -10) ".xcodeproj")
                       (setq has-proj-file t))))
             (setq df (cdr df)))
           (if has-proj-file
               (compile "xcodebuild -configuration Debug clean"))))

       (defun build-with-xcode ()
         (interactive)
         (defun dir () (shell-command "osascript -e 'tell application \"Xcode\" to get the project directory of project 1'"))
         (shell-command (format "cd %s" (dir))))

                                        ;       (define-key osx-key-mode-map (kbd "A-r") 'build-and-go-in-xcode)

       (defun build-and-go-in-xcode ()

         (interactive)
         (shell-command "osascript -e 'tell application \"Xcode\" to build project 1'")
         (shell-command "osascript -e 'tell application \"Xcode\" to launch project 1'"))))
