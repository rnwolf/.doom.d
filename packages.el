;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


;; org-super-agenda allows your daily/weekly agenda to be grouped into sections, rather than having them all in one big list.
(package! org-super-agenda)
;; https://github.com/hlissner/evil-snipe navigate to letters very quickly.
(package! evil-snipe)

;; Required for org-roam as per https://earvingad.github.io/posts/org_roam_windows/
(package! emacsql-sqlite3)

;;(package! pdf-tools)
;; Org-noter’s purpose is to let you create notes that are kept in sync when you scroll through the document, but that are external to it
;;(package! org-noter)
;;(package! org-noter-pdftools)

;; visual-regexp for Emacs is like replace-regexp, but with live visual feedback directly in the buffer.
(package! visual-regexp)

;; interactively create searches, doing automatic searches based on the editing context, refining and modifying search results and much more.
(package! rg)

;; see the contents of the subdirectory and the current directory in the same view.
(package! dired-subtree)

;; https://github.com/tamasgal/rc/blob/master/doom.d
;; https://oer.gitlab.io/emacs-reveal-howto/howto.html
(use-package! org-re-reveal
    :after ox
    :init
    (setq org-re-reveal-root "https://revealjs.com"))

;;  Install the latest code direct from the code repo.

;;  (package! org-roam
;;    :recipe (:host github :repo "org-roam/org-roam")
;;    :pin "0d235686f4b2cef9b6cded861edabf747e1d64e2")
(unpin! org-roam)

(package! org-gtd)

;; These packages make it possible for org-pomodoro to play sound wav file for clicking clock.
;; NOTE enabling these packages caused problems with org-pomodoro. Opens up second window of buffer!
;;(package! sound-wav)
;;(package! powershell)


;; Replace the alert package withich is part of the default DOOM configuration
;; See https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#changing-a-recipe-for-a-included-package
(package! alert)
;;  Had to fork and modify the alert.el package in order to use a more recent notification utility app for MS-Windows
;;  There is also another even more recent elisp based package which will be worth exploring.
;;  See https://github.com/gkowzan/alert-toast
(package! alert
  :recipe (:host github :repo "rnwolf/alert"))
;; Also need to add the following to config.el
;; add the following to your .emacs file, because of #30 https://github.com/jwiegley/alert/issues/30
;;(setq alert-default-style (quote toaster))
;;(setq alert-user-configuration (quote ((nil toaster nil))))

;; End of alert comments

;; Adds notification support for org-agenda views.
(package! org-wild-notifier)
