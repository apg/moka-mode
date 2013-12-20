
(defconst moka-version "0.1"
  "The current version of moka-mode.")

(defgroup moka nil
  "Enhanced functionality for Java development."
  :link '(emacs-library-link :tag "Source File" "moka.el")
  :group 'tools)

(defcustom moka-javadoc-root-list nil
  "This variable has been deprecated and will be removed."
  :type '(repeat file)
  :group 'moka)

(defcustom moka-javadoc-root-alist
  '(("^java\\." . "http://download.oracle.com/javase/6/docs/api")
    ("^javax\\." . "http://download.oracle.com/javase/6/docs/api")
    ("^org\\.w3c\\.dom" . "http://download.oracle.com/javase/6/docs/api")
    ("^org\\.xml\\.sax" . "http://download.oracle.com/javase/6/docs/api"))
  "*Alist of package patterns vs corresponding Javadoc root URLs.
Each element looks like (REGEXP . URL) where REGEXP is a regexp
that matches a group of imports, and URL is the Javadoc root URL
for that group of imports. The Javadoc root URL is where the
\"index.html\" file resides."
  :type '(alist :key-type regexp :value-type string)
  :group 'moka)

(defcustom moka-browse-url-function 'browse-url
  "*A function used by `moka-tags-browse-url' to display URLs.
This function will be called with one argument: the URL to display."
  :group 'moka
  :type 'function)

(defcustom moka-display-menu-flag 't
  "*Non-nil means that the moka-tags submenu will be added to the menu bar.
Set this variable to nil if you do not want to use the moka-tags submenu. If
non-nil, the submenu will be displayed when moka-tags mode is active."
  :type 'boolean
  :group 'moka)

(defcustom moka-trace-flag nil
  "*Non-nil means that tracing is ON. A nil value means that tracing is OFF."
  :type 'boolean
  :group 'moka)

(defvar moka-temp-buffer-name "*moka-scratch*"
  "The name of the moka-tags temporary buffer.")

;; Require all the other stuff
(require 'moka-lib)
(require 'moka-tags)
(require 'moka-cleanup)
(require 'moka-mvn)

(defvar moka-mode-map nil
  "Keymap used when moka mode is enabled.")

(unless moka-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?\,)]   'moka-tags-member-completion)
    (define-key map [(meta ?\,)]      'moka-tags-show-declaration)
    (define-key map [(meta f1)]       'moka-tags-show-documentation)
    (define-key map [(control c) ?\,] 'moka-tags-update-this-tags-file)
    (define-key map [(control c) ?\+] 'moka-cleanup-add-import)
    (define-key map [(control c) ?\=] 'moka-cleanup-organize-imports)

    (setq moka-mode-map map)))

(defvar moka-menu-list
  (list "Moka"
        ["Member completion" moka-tags-member-completion t]
        ["Show declaration" moka-tags-show-declaration t]
        ["Show documentation" moka-tags-show-documentation t]
        "--"
        ["Update this tags file" moka-tags-update-this-tags-file t]
        ["Update all tags files" moka-tags-update-tags-files t]
        "--"
        ["Add import" moka-cleanup-add-import t]
        ["Organize imports" moka-cleanup-organize-imports t])
  "Moka submenu definition.")

;; Define a menu, and put it in the `moka-mode-map'.
(easy-menu-define moka-menu moka-mode-map
  "Provides menu items for accessing moka-tags functionality."
  moka-menu-list)

;;;###autoload
(define-minor-mode moka-mode
  "Toggle moka- mode.
With arg, turn moka mode on if arg is positive.

When moka mode is enabled, a number of improved tags lookup commands are
available, as shown below. moka-tags mode provides commands for looking up the
identifier before or around point, completing partly typed identifiers, and
managing tags table files.

\\{moka-mode-map}"
  nil
  nil
  moka-mode-map
  (if moka-mode
      (if moka-display-menu-flag
          (easy-menu-add moka-menu-list moka-mode-map))
    (if moka-display-menu-flag
        (easy-menu-remove moka-menu-list))))


