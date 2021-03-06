;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Rudiger Wolf"
      user-mail-address "Rudiger.Wolf@ThroughputFocus.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
(setq doom-font (font-spec :family "JetBrains Mono" :size 24)
      doom-big-font (font-spec :family "JetBrains Mono" :size 36)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 24)
      doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-vibrant) ;; doom-one based theme
(setq doom-theme 'doom-one-light) ;; doom-one based

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; Setting the GC threshold to 32MB (Doom default is 16MB) to avoid hangs
(setq gcmh-high-cons-threshold 33554432)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/OneDrive/Documents/org")
(setq org-log-done 'time) ;; Log the date and time that tasks are done.
(setq org-agenda-files (directory-files-recursively "~/OneDrive/Documents/org/" "\\.org$"))

(setq org-roam-directory (file-truename "~/OneDrive/Documents/roam/"))

;; https://www.orgroam.com/manual/Tags.html
;; 'prop: This extracts tags from the #+roam_tags property. Tags are space delimited, and can be multi-word using double quotes.
;; 'last-directory: if a file is located at relative path foo/bar/file.org, the file will have tag bar
(setq org-roam-tag-sources '(prop last-directory))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

;; Hide the emphasis markup (e.g * / _ ) in org-mode
;; ‘*bold*’, ‘/italic/’, ‘_underlined_’, ‘=verbatim=’ and ‘~code~’ and ‘+strike-through+’.
(setq org-hide-emphasis-markers t)

;; Configure search directory
;;(setq deft-directory org-directory )
(setq deft-directory "~/OneDrive/Documents/roam/")  ;; Default, see below to pick.
(setq deft-recursive t)
(setq deft-default-extension "org")

;; Setup my list of deft directories
;; https://www.reddit.com/r/emacs/comments/h0h4ix/how_to_configure_multile_directories_with_deft/
(defvar my/deft-dir-list '()
  "A list of deft directories to pick")

(setq my/deft-dir-list '(
                         "~/OneDrive/Documents/2020/"
                         "~/OneDrive/Documents/org/"
                         "~/OneDrive/Documents/roam/"
                         "~/OneDrive/Documents/org/archive/"
                         "~/OneDrive/Documents/roam/glossary/"
                         "~/OneDrive/Documents/roam/journal/"
                         "~/OneDrive/Documents/roam/people/"
                         "~/OneDrive/Documents/roam/notes/"
                         "~/OneDrive/Documents/roam/slides/"
                         ))

(defun my/pick-deft-dir ()
  "Select directories from a list"
  (interactive)
  (setq deft-directory
        (ido-completing-read "Select directory: " my/deft-dir-list))
  (deft-refresh))

;; Specify the Projectile root directories
(setq projectile-project-search-path '("~/workspace"))
(setq projectile-ignored-projects '("~/" "~/tmp" "~/.emacs.d/.local/straight/repos/"))

;; Window title
;; I’d like to have just the buffer name, then if applicable the project folder
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string ".*/[0-9]*-?" "🢔 " buffer-file-name)
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))
;; End of modifying windows titles.


(setq display-time-day-and-date t)
(setq display-time-string-forms
      '((propertize (format-time-string "%Y-%m-%d %H:%M"))))
(display-time)

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups '((:name "Today"
                                   :time-grid t
                                   :scheduled today)
                                  (:name "Due today"
                                   :deadline today)
                                  (:name "Important"
                                   :priority "A")
                                  (:name "Overdue"
                                   :deadline past)
                                  (:name "Due soon"
                                   :deadline future)))
  :config
  (org-super-agenda-mode))

;; evil-snipe provides fast find and jump to letters nearby.
;; Forward: f <letter> ; next  , previous
;; Backward: t <letter> ; next  , previous
;; F for find backward
;; s <two letters>

(evil-snipe-mode +1)
(evil-snipe-override-mode +1)

;; evil-snipe has a problem with magit
(add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)

;; Take screenshot, save into directory, that must already exist.
;; Embed screenshots into org or md documents.
;; Type Mx- my/.....

(defvar my/screenshot-directory "./images/")
(defun my/take-screenshot ()
  "This works only in Windows. Call a clipping tool and take a screenshot.
   Store in png format into 'my/screenshot-directory"
   (setq filename
        (concat
         (make-temp-name
          (concat my/screenshot-directory
                  (format-time-string "%Y%-m%-d_%H%M")) ) "--screenshot.png"))
  (shell-command "snippingtool /clip")
  (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('" filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\"")))

(defun my/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (my/take-screenshot)
  (insert (concat "[[file:" filename "]]")))

(defun my/md-screenshot ()
  "Take a screenshot into my/screenshot-directory
Insert a markdown image link"
  (interactive)
  (my/take-screenshot)
  (insert (concat "![]("filename")")))
;; End of screenshot functions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move and Insert link to the most recent screenshot image
;; https://www.reddit.com/r/emacs/comments/52q70g/paste_an_image_on_clipboard_to_emacs_org_mode/
(setq andre--screenshot-folder "~/OneDrive/Inbox")

(defun get-newest-file-from-dir  (path)
      "Get latest file (including directory) in PATH."
      (car (directory-files path 'full nil #'file-newer-than-file-p)))

    (defun my/org-move-insert-screenshot ()
      "Moves latest(sorted by name) image from Pictures folder to ./media, inserting org-mode link"
      (interactive)
      (let* ((indir (expand-file-name andre--screenshot-folder))
             (infile (get-newest-file-from-dir indir))
             (outdir (concat (file-name-directory (buffer-file-name)) "/media"))
             (outfile (expand-file-name (file-name-nondirectory infile) outdir)))
        (unless (file-directory-p outdir)
          (make-directory outdir t))
        (rename-file infile outfile)
        (insert (concat (concat "#+ATTR_HTML: :width 300\n#+ATTR_ORG: :width 300\n[[./media/" (file-name-nondirectory outfile)) "]]")))
      (newline)
      (newline))

;; Spelling functions.
;; Use Choco to install hunspell
;; Create directory for dic & aff files.
;; Download default, en_GB etc files for dictionaries.
;; Specify location of dicts with environment variable. Does not seem to work???

(use-package! flyspell
  :commands (flyspell-mode)
  :init
  (setenv "LANG" "en_GB")
  :config
  (setq ispell-aspell-data-dir "~/.hunspell_default")
  (setq ispell-aspell-dict-dir ispell-aspell-data-dir)

  (setq-default ispell-hunspell-dict-paths-alist
	      '(
		("default" "C:\\Users\rnwol\\.hunspell_default\\default.aff")
		("en_GB" "C:\\Users\\rnwol\\.hunspell_default\\en_GB.aff")
		("en_US" "C:\\Users\\rnwol\\.hunspell_default\\en_US.aff")
		("de_DE" "C:\\Users\\rnwol\\.hunspell_default\\de_DE.aff")
		))
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary "~/.hunspell_personal"))
  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0))

  ;;(add-to-list 'ispell-aspell-dictionary-alist (ispell-aspell-find-dictionary "en_GB"))
  (setq ispell-program-name  "hunspell") ;; Note this is just a name, not a path to exe.
  (setq ispell-local-dictionary "en_GB") ;;Also, specify that the British dictionary should be used
  (setq ispell-dictionary "en_GB")
  (setq ispell-dictionary-alist
        '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)))
  (add-to-list 'ispell-local-dictionary-alist
               '("de_DE"
                 "[a-zäöüßA-ZÄÖÜ]"
                 "[^a-zäöüßA-ZÄÖÜ]"
                 "[']"
                 t
                 ("-d" "de_DE, de_DE_hyph"); Dictionary file name
                 nil
                 utf-8))

(defun switch-dictionary-de-en ()
  "Switch german and english dictionaries."
  (interactive)
  (let* ((dict ispell-current-dictionary)
         (new (if (string= dict "de_DE") "en_GB"
                   "de_DE")))
    (ispell-change-dictionary new)
    (message "Switched dictionary from %s to %s" dict new)))
;; Note that one could use multiple language dictionaries one after another to check multi-language docs
;; See https://200ok.ch/posts/2020-08-22_setting_up_spell_checking_with_multiple_dictionaries.html

(global-set-key (kbd "C-c d") 'switch-dictionary-de-en)

(after! org (add-hook 'org-mode-hook 'turn-on-flyspell))

;; End of Spelling settings.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abbreviations                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq-default abbrev-mode t)
;; save abbreviations upon exiting xemacs
;;(setq save-abbrevs t)
;; set the file storing the abbreviations
;;(setq abbrev-file-name "~/.doom.d/my-abbreviations.el")
;; reads the abbreviations file on startup
;;(quietly-read-abbrev-file)

(use-package abbrev-mode
  :init
  (setq-default abbrev-mode t)
  (setq abbrev-file-name "~/.doom.d/abbrev.el")
  (read-abbrev-file "~/.doom.d/abbrev.el")
  ;; a hook funtion that sets the abbrev-table to org-mode-abbrev-table
  ;; whenever the major mode is a text mode
  (defun mwp-set-text-mode-abbrev-table ()
    (if (derived-mode-p 'text-mode)
        (setq local-abbrev-table org-mode-abbrev-table)))
  :commands abbrev-mode
  :hook
  (abbrev-mode . mwp-set-text-mode-abbrev-table)
  :config
  ;; (setq default-abbrev-mode t)
  (setq save-abbrevs 'silently))


;; This plus Roam Chrome Bookmarklet can be used to add webpages.
(after! org-roam
      (setq org-roam-capture-ref-templates
            '(("r" "ref" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "websites/${slug}"
               :head "#+TITLE: ${title}
    #+ROAM_KEY: ${ref}
    - source :: ${ref}"
               :unnarrowed t))))



(setq org-roam-graph-viewer
    (lambda (file)
      (let ((org-roam-graph-viewer "c:\\Program Files\\Mozilla Firefox\\firefox.exe"))
        (org-roam-graph--open (concat "file:///" file)))))

;; Make it quick to get org-roam menu up.
(map! :leader :desc "Org roam" "r" (general-simulate-key "SPC n r"))

;;;; org-roam-server-light ;;
;;;; https://github.com/AloisJanicek/org-roam-server-light
;;;; move org-roam-server functionality from emacs into external python server process
;;;; due to subjective poor elisp / emacs performance when computing graph data
;;;; for larger amount of org-roam files and serving them to web browser.
;;
(require 'f)

(defvar org-roam-server-light-dir "~/org-roam-server-light"
  "Directory contenting org-roam-server-light repository.")

(defvar org-roam-server-light-network-vis-options nil
  "Options to be passed directly to vis.Network, in JSON format.
e.g. (json-encode (list (cons 'physics (list (cons 'enabled json-false)))))
or { \"physics\": { \"enabled\": false } }"
  )

;; Example of enabling 'to' arrows ->, for more see visjs.github.io/vis-network/docs/network
(setq org-roam-server-light-network-vis-options
      "{ \"edges\": { \"arrows\": { \"to\": { \"enabled\": true,\"scaleFactor\": 1.5 } } } }"
      )

(defvar org-roam-server-light-tmp-dir
  (let ((dir-name "org-roam-server-light/"))
    (if (or IS-WINDOWS IS-MAC)
        (concat (replace-regexp-in-string "\\\\" "/"
                                          (or (getenv "TMPDIR")
                                              (getenv "TMP")))
                "/" dir-name)
      (concat "/tmp/" dir-name)))
  "Directory contenting org-roam-server-light repository.")

(defvar org-roam-server-light-last-roam-buffer nil
  "Variable storing name of the last org-roam buffer")

;;;###autoload
(defun org-roam-server-light-update-last-buffer ()
  "Update `org-roam-server-light-last-roam-buffer'."
  (let ((buf (or (buffer-base-buffer (current-buffer)) (current-buffer))))
    (when (org-roam--org-roam-file-p
           (buffer-file-name buf))
      (setq org-roam-server-light-last-roam-buffer
            (car (last (split-string (org-roam--path-to-slug (buffer-name buf)) "/"))))
      (f-write-text
       org-roam-server-light-last-roam-buffer
       'utf-8
       (concat org-roam-server-light-tmp-dir "org-roam-server-light-last-roam-buffer")))))

;;;###autoload
(defun org-roam-server-light-find-file-hook-function ()
  "If the current visited file is an `org-roam` file, update the current buffer."
  (when (org-roam--org-roam-file-p)
    (add-hook 'post-command-hook #'org-roam-server-light-update-last-buffer nil t)
    (org-roam-server-light-update-last-buffer)))

(define-minor-mode org-roam-server-light-mode
  "Start the http server and serve org-roam files."
  :lighter ""
  :global t
  :init-value nil
  (let* ((title "org-roam-server-light"))
    (if (not (ignore-errors org-roam-server-light-mode))
        (progn
          (when (get-process title)
            (delete-process title))
          (remove-hook 'find-file-hook #'org-roam-server-light-find-file-hook-function nil)
          (dolist (buf (org-roam--get-roam-buffers))
            (with-current-buffer buf
              (remove-hook 'post-command-hook #'org-roam-server-light-update-last-buffer t))))
      (progn
        (add-hook 'find-file-hook #'org-roam-server-light-find-file-hook-function nil nil)
        (unless (file-exists-p org-roam-server-light-tmp-dir)
          (make-directory org-roam-server-light-tmp-dir))
        (f-write-text org-roam-server-light-network-vis-options
                      'utf-8
                      (expand-file-name "org-roam-server-light-network-vis-options" org-roam-server-light-tmp-dir))
        (f-write-text org-roam-db-location
                      'utf-8
                      (expand-file-name "org-roam-db-location" org-roam-server-light-tmp-dir))
        (f-write-text org-roam-directory
                      'utf-8
                      (expand-file-name "org-roam-directory" org-roam-server-light-tmp-dir))
        (let ((default-directory org-roam-server-light-dir))
          (start-process-shell-command "org-roam-server-light" "org-roam-server-light-output-buffer" "py -3.8 main.py")))
      )))
;;;; end org-roam-server-light ;;;;

;; https://awesomeopensource.com/project/nmartin84/.doom.d
;;
(setq org-roam-dailies-capture-templates
   '(("d" "daily" plain (function org-roam-capture--get-point) ""
      :immediate-finish t
      :file-name "journal/%<%Y-%m-%d-%a>"
      :head "#+TITLE: %<%Y-%m-%d %a>\n#+STARTUP: content\n\n* Tasks\n\n* New\n\n* Next\n\n* Notes\n\n* Time Spent\n\n* Insights\n\n
")))

(setq org-roam-capture-templates
        '(("a" "app" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "notes/app/%<%Y%m%d%H%M>-${slug}"
           :head "#+title: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+roam_tags: %^{roam_tags}\n\nsource :: [[%^{link}][%^{link_desc}]]\n\n"
           :unnarrowed t)
          ("d" "digest" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "notes/digest/%<%Y%m%d%H%M>-${slug}"
           :head "#+title: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+roam_tags: %^{roam_tags}\n\nsource :: [[%^{link}][%^{link_desc}]]\n\n"
           :unnarrowed t)
          ("n" "notes" plain (function org-roam-capture--get-point)
           :file-name "notes/${slug}"
           :head "#+title: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+roam_tags: %(read-string \"tags: \")\n\n"
           :unnarrowed t
           "%?")
          ("g" "glossary" plain (function org-roam-capture--get-point)
           :file-name "glossary/${slug}"
           :head "#+title: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+roam_tags: %(read-string \"tags: \")\n\n"
           :unnarrowed t
           "%?")
          ("p" "people" plain (function org-roam-capture--get-point)
           :file-name "notes/people/${slug}"
           :head "#+title: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+roam_tags: %(read-string \"tags: \")\n\n"
           :unnarrowed t
           "%?")
          ("r" "reveal slide" plain (function org-roam-capture--get-point)
           :file-name "slides/%<%Y%m%d%H%M>-${slug}"
           :head "#+title: ${title}\n#+Author: Rudiger Wolf\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+KEYWORDS: %(read-string \"keywords: \")\n#+DESCRIPTION: \n#+OPTIONS: reveal_center:t reveal_progress:t reveal_history:nil reveal_control:t\n#+options: num:nil toc:nil reveal_width:1400 reveal_height:1000\n#+REVEAL_PLUGINS: (highlight notes search zoom)\n#+REVEAL_THEME: %^{theme|white|black|league|beige|sky|night|serif|simple|solarized|blood|moon}\n#+REVEAL_ROOT: https://cdn.jsdelivr.net/npm/reveal.js@4.1.0\n#+REVEAL_OVERVIEW: t\n\n* Headline\n- Some text.\n- More text#+BEGIN_NOTES\nYour note\n#+END_NOTES\n* Next slide\n** Sub-point\n- Bullet1\n#+REVEAL: split\n- Bullet2\n- Bullet3\n"
           :unnarrow t
           "%?")))

;; https://org-roam.discourse.group/t/update-a-field-last-modified-at-save/321/8
;; Start - LAST_MODIFIED update on save if exists
(add-hook 'before-save-hook #'zp/org-set-last-modified)

(defun zp/org-find-time-file-property (property &optional anywhere)
    "Return the position of the time file PROPERTY if it exists.
When ANYWHERE is non-nil, search beyond the preamble."
    (save-excursion
      (goto-char (point-min))
      (let ((first-heading
             (save-excursion
               (re-search-forward org-outline-regexp-bol nil t))))
        (when (re-search-forward (format "^#\\+%s:" property)
                                 (if anywhere nil first-heading)
                                 t)
          (point)))))



(defun zp/org-has-time-file-property-p (property &optional anywhere)
    "Return the position of time file PROPERTY if it is defined.
As a special case, return -1 if the time file PROPERTY exists but
is not defined."
    (when-let ((pos (zp/org-find-time-file-property property anywhere)))
      (save-excursion
        (goto-char pos)
        (if (and (looking-at-p " ")
                 (progn (forward-char)
                        (org-at-timestamp-p 'lax)))
            pos
          -1))))

(defun zp/org-set-time-file-property (property &optional anywhere pos)
    "Set the time file PROPERTY in the preamble.
When ANYWHERE is non-nil, search beyond the preamble.
If the position of the file PROPERTY has already been computed,
it can be passed in POS."
    (when-let ((pos (or pos
                        (zp/org-find-time-file-property property))))
      (save-excursion
        (goto-char pos)
        (if (looking-at-p " ")
            (forward-char)
          (insert " "))
        (delete-region (point) (line-end-position))
        (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (insert now)))))

(defun zp/org-set-last-modified ()
    "Update the LAST_MODIFIED file property in the preamble."
    (when (derived-mode-p 'org-mode)
      (zp/org-set-time-file-property "LAST_MODIFIED")))
;; End - LAST_MODIFIED update on save if exists

;; Org Journal Settings
(setq org-journal-date-prefix "#+TITLE: "
      org-journal-time-prefix "* "
      org-journal-date-format "%Y-%m-%d, %a"
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-skip-carryover-drawers (list "LOGBOOK")) ;;skip carry over of previous days clocked entries when it is under the drawer LOGBOOK

(use-package! org-gtd
  :after org
  :config
  ;; where org-gtd will put its files. This value is also the default one.
  (setq org-gtd-directory "~/OneDrive/Documents/org")
  ;; package: https://github.com/Malabarba/org-agenda-property
  ;; this is so you can see who an item was delegated to in the agenda
  (setq org-agenda-property-list '("DELEGATED_TO"))
  ;; I think this makes the agenda easier to read
  (setq org-agenda-property-position 'next-line)
  ;; package: https://www.nongnu.org/org-edna-el/
  ;; org-edna is used to make sure that when a project task gets DONE,
  ;; the next TODO is automatically changed to NEXT.
  (setq org-edna-use-inheritance t)
  (org-edna-load)
  :bind
  (("C-c g c" . org-gtd-capture) ;; add item to inbox
   ("C-c g a" . org-agenda-list) ;; see what's on your plate today
   ("C-c g p" . org-gtd-process-inbox) ;; process entire inbox
   ("C-c g n" . org-gtd-show-all-next) ;; see all NEXT items
   ("C-c g s" . org-gtd-show-stuck-projects) ;; see projects that don't have a NEXT item
   ("C-c g f" . org-gtd-clarify-finalize)) ;; the keybinding to hit when you're done editing an item in the processing phase
)

(after! (org-gtd org-capture)
  (add-to-list 'org-capture-templates
               '("i" "GTD item"
                 entry (file (lambda () (org-gtd--path org-gtd-inbox-file-basename)))
                 "* %?\n%U\n\n  %i"
                 :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("l" "GTD item with link to where you are in emacs now"
                 entry (file (lambda () (org-gtd--path org-gtd-inbox-file-basename)))
                 "* %?\n%U\n\n  %i\n  %a"
                 :kill-buffer t)))

(defadvice! +zz/load-org-gtd-before-capture (&optional goto keys)
    :before #'org-capture
    (require 'org-capture)
    (require 'org-gtd))

;; Integrate org-journal with org-capture
;; https://github.com/bastibe/org-journal

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(after! (org-gtd org-capture)
  (add-to-list 'org-capture-templates
               '("e" "org-Journal Entry for today"
                 plain (function org-journal-find-location)
                 "** %(format-time-string org-journal-time-format)%^{Title}\nCREATED: %u\n%i%?"
                 :jump-to-captured t :immediate-finish t)))

(defvar org-journal--date-location-scheduled-time nil)

(defun org-journal-date-location (&optional scheduled-time)
  (let ((scheduled-time (or scheduled-time (org-read-date nil nil nil "Date:"))))
    (setq org-journal--date-location-scheduled-time scheduled-time)
    (org-journal-new-entry t (org-time-string-to-time scheduled-time))
    (unless (eq org-journal-file-type 'daily)
      (org-narrow-to-subtree))
    (goto-char (point-max))))

(after! (org-gtd org-capture)
  (add-to-list 'org-capture-templates
               '("f" "future org-Journal entry"
                 plain (function org-journal-date-location)
                 "** TODO %?\nCREATED: %u\n%i\n <%(princ org-journal--date-location-scheduled-time)>\n"
                 :jump-to-captured t)))

;; What do we log for tasks?
;; https://awesomeopensource.com/project/nmartin84/.doom.d
(after! org (setq org-log-into-drawer t
                  org-log-done 'time
                  org-log-repeat 'time
                  org-log-redeadline 'note
                  org-log-reschedule 'note))

;; keybind to disable search highlighting (like :set noh)
(map! :leader
      :desc "Clear search highlight"
      "s c"
      #'evil-ex-nohighlight)

;; Update Load path with any packages installed in the C:\Users\rnwol\.doom.d\packages directory
(let ((default-directory (expand-file-name "packages" doom-private-dir)))
  (normal-top-level-add-subdirs-to-load-path))

;; In the above packages load path
;;(require 'org-alert)

;; erc-burnt-toast.el --- erc-match support for w32 notification center
;; https://github.com/mplscorwin/erc-burnt-toast/blob/master/erc-burnt-toast.el
;;(add-load-path "lisp")

;; Basic setup:
;;   (eval-after-load 'erc-match
;;     (progn (require 'erc-burnt-toast)
;;            (erc-burnt-toast-mode 1)))

;; Configure org-pomodoro alerts so that they work on MS-Windows
;; +pomodoro to org section in the init.el
;; ref https://www.gitmemory.com/issue/marcinkoziej/org-pomodoro/75/517809613

;; Org-pomodoro

(after! org-pomodoro

  ;; Org-pomodoro
(setq org-pomodoro-length 25) ;; 25
(setq org-pomodoro-short-break-length 5) ;; 5
(setq org-pomodoro-long-break-length 15)
(setq org-pomodoro-play-sounds nil)

(defun org-pomodoro-notify (title message)
  "Temporary replacement for function of the same name which uses
the buggy alert.el package.  TITLE is the title of the MESSAGE.
Assumes you have installed toast64.exe from https://github.com/go-toast/toast"
  (let*
      ((toast "toast64")
       (t-title (concat " --title \"" title))
       (t-message (concat "\" --message \"" message "\""))
       (t-image (concat " --icon \"C:\\Program Files\\Emacs\\x86_64\\share\\emacs\\27.1\\etc\\images\\icons\\hicolor\\128x128\\apps\\emacs.png\""))
       (t-duration (concat " --duration \"medium\""))
       (t-appid (concat " --app-id \"EMACS org-Pomodoro\""))
       (t-audio (concat " --audio \"default\""))
       (my-command (concat toast t-title t-message t-image t-audio t-duration t-appid)))
    (call-process-shell-command my-command)))
)


;; In order to get Popup Windows notifications, of style "toaster" working
;; We need to add the following to config.el
;; Also makes use of the toast64.exe utility.
;; because of #30 https://github.com/jwiegley/alert/issues/30
(after! alert
   (setq alert-default-style (quote toaster))
   (setq alert-user-configuration (quote ((nil toaster nil))))
   )
