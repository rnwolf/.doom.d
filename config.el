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
(setq doom-theme 'doom-vibrant) ;; doom-one

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

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

(setq deft-directory org-directory )

;; Specify the Projectile root directories
(setq projectile-project-search-path '("~/workspace"))


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
                                   :dealine past)
                                  (:name "Due soon"
                                   :dealine future)))
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
