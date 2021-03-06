;;; -*- Mode: Emacs-Lisp -*-
;;; Zane Whitney's Emacs Settings
;;;(using some parts of Jeff Dlouhy's .emacs file: https://github.com/jeffd)

;;; Force Package loading on init
(package-initialize)

(defun load-graphical-settings ()

  ;;;Global Nyan-mode
  (define-globalized-minor-mode my-global-nyan-mode nyan-mode
    (lambda () (nyan-mode 1)))

  (my-global-nyan-mode 1)

  ;;; Settings Theme
  (load-theme 'solarized-dark t))

(defun load-terminal-settings ()
  ;;; Settings Theme
  (load-theme 'misterioso t)
)

(if (display-graphic-p)
    (load-graphical-settings)
     (load-terminal-settings))


(setq custom-basedir (expand-file-name "~/.emacs.d/site-lisp"))
(add-to-list 'load-path custom-basedir)

(defun add-path (p)
  (add-to-list 'load-path (concat custom-basedir p)))

(add-to-list 'exec-path "/usr/local/bin")

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

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;;; Smooth Scrolling
;; (message "applying scrolling settings ...")
(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil)

;;; Scrolling
(global-set-key [C-next] 'scroll-other-window)
(global-set-key [C-prior] 'scroll-other-window-down)

;;; Yes or No prompts shorter
(fset 'yes-or-no-p 'y-or-n-p)

;;; Set default dir to home
(cd "~/")

;;; For Panic's Prompt app. Enables the backspace key, if using Emacs over ssh.
(cond ((getenv "SSH_CONNECTION")
       (define-key key-translation-map [?\C-h] [?\C-?])))



;;; Save minibuffer history between sessions
(savehist-mode 1)

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
(global-set-key (kbd "C-c g") 'helm-do-grep)

;;;MODE HOOKS
(defun my-major-mode-hook ()
 (column-enforce-mode))

(add-hook 'after-change-major-mode-hook 'my-major-mode-hook)



;; to switch to the previous frame
(defun prev-frame ()
  (interactive)
  (other-frame -1))

;;; Package Repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;;; Other Bindings
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c e") 'eval-buffer)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-~") 'prev-frame)

;;; Making sure the clipboard works
(setq x-select-enable-clipboard t)

;;; TRAMP stuff
(setq tramp-default-method "ssh")

;;; Helm mode
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;;; Because helm autoloads things
;; with-eval-after-load not in 24.3
(if (not (string-match-p "24.3" (emacs-version)))
    (with-eval-after-load
        'abcd-mode
      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
      (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
      (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

      (helm-mode 1)

      (when (executable-find "curl")
        (setq helm-google-suggest-use-curl-p t))

      (setq helm-quick-update                     t ; do not display invisible candidates
            helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
            helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
            helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
            helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
            helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
            helm-ff-file-name-history-use-recentf t)
      ))

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
        ;; OK, we got a .h file, if a .m file exists we'll assume it's an
        ;; objective c file. Otherwise, we'll look for a .cpp file.
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
         (defun dir ()
           (shell-command
            "osascript -e 'tell application \"Xcode\" to get the project directory of project 1'"))
         (shell-command (format "cd %s" (dir))))

                                        ; (define-key osx-key-mode-map (kbd "A-r")
                                        ; 'build-and-go-in-xcode)

       (defun build-and-go-in-xcode ()

         (interactive)
         (shell-command
          "osascript -e 'tell application \"Xcode\" to build project 1'")
         (shell-command
          "osascript -e 'tell application \"Xcode\" to launch project 1'"))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
