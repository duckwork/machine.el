;;; machine.el --- Load machine-dependendant settings -*- lexical-binding: t; -*-

;;; Commentary:

;; When using Emacs on multiple computers, some variables and functions need
;; different definitions.  This library is built to assist in working with
;; different machine configurations for Emacs.

;;; TODO:

;; machine-case to switch on machine

;;; Code:

(require 'cl-lib)

(defgroup machine nil
  "Machine-specific configurations."
  :group 'emacs
  :prefix "machine-")

;;; Settings

(defcustom machine-load-directory (locate-user-emacs-file "machines"
                                                         "~/.emacs-machines")
  "The directory where machine-specific configurations live."
  :type 'file)

;; These `defcustom's are best-guess defaults.

(defcustom machine-default-font (cond
                                 ((memq system-type '(ms-dos windows-nt))
                                  "Consolas")
                                 (t "monospace"))
  "The font used for the `default' face.
Set this in your machine files."
  :type 'string)

(defcustom machine-default-height 100
  "The height used for the `default' face.
Set this in your machine files."
  :type 'number)

(defcustom machine-variable-pitch-font (cond
                                       ((memq system-type '(ms-dos windows-nt))
                                        "Arial")
                                       (t "sans-serif"))
  "The font used for the `variable-pitch' face.
Set this in your machine files."
  :type 'string)

(defcustom machine-variable-pitch-height 1.0
  "The height used for the `variable-pitch' face.
A floating-point number is recommended, since that makes it
relative to the `default' face height.

Set this in your machine files."
  :type 'number)

(defcustom machine-files-order '(:type :name :user)
  "The order to load `machine-files' in.
The elements of this list correspond to the keys in
`machine-machines'."
  :type '(list (const :tag "System type" :type)
               (const :tag "System name" :name)
               (const :tag "Current user" :user)))

;;; Variables

(defvar machine-machines nil
  "Plist of machines that Emacs is in.
The keys are as follows:

- :name - `system-name'
- :type - `system-type'
- :user - `user-login-name'

Each value is made safe to be a file name by passing through
`machine--safe'.

Do not edit this by hand.  Instead, call `machine-get-machines'.")

(defvar machine-files nil
  "List of files to load for machine-specific configuration.
Do not edit this by hand.  Instead, call `machine-get-machine-files'.")


;;; Functions

(defun machine--warn (message &rest args)
  "Display a machine-file warning message.
This function is like `warn', except it uses a `machine' type."
  (display-warning 'machine (apply #'format-message message args)))

(defun machine--safe (str)
  "Make STR safe for a file name."
  (let ((bad-char-regexp ))
    (downcase (string-trim
               (replace-regexp-in-string "[#%&{}\$!'\":@<>*?/ \r\n\t+`|=]+"
                                         "-" str)
               "-" "-"))))

(defun machine-get-machines ()
  "Determine the current machine(s).
This system updates `machine-machines', which see."
  ;; Add system-name
  (setf (plist-get machine-machines :name)
        (intern (machine--safe (system-name))))
  ;; Add system-type
  (setf (plist-get machine-machines :type)
        (intern (machine--safe (symbol-name system-type))))
  ;; Add current user
  (setf (plist-get machine-machines :user)
        ;; Use `user-real-login-name' in case Emacs gets called under su.
        (intern (machine--safe (user-real-login-name))))
  machine-machines)

(defun machine-get-files ()
  "Determine the current machines' load-files.
The machine load-files should live in `machine-load-directory', and
named using either the raw name given by the values of
`machine-machines', or that name prepended with the type, e.g.,
\"name-bob.el\", for a machine named \"bob\".

The second form of file-name is to work around name collisions,
e.g. if a there's a user named \"bob\" and a machine named
\"bob\".

This function updates `machine-files'."
  ;; Get machines
  (machine-get-machines)
  ;; Re-set `machine-files'
  (setq machine-files nil)

  (let (ret)
    (dolist (key (reverse machine-files-order))
      (let* ((val (plist-get machine-machines key))
             (key-val (intern (machine--safe (format "%s-%s" key val)))))
        (push (list key-val val) ret)))

    ;; Update `machine-files'.
    (setq machine-files ret)))

(defvar machine-after-load-theme #'ignore
  "Function to run after `load-theme'.
Add machine-specific functionality to this function using
`add-function'.")

;;;###autoload
(defun machine-settings-load (&optional error nomessage)
  "Load machine settings from `machine-files'.
Each list in `machine-files' will be considered item-by-item; the
first found file in each will be loaded.

ERROR determines how to deal with errors: if nil, warn the user
when no machine-files can be found or when the machine being used
cannot be determined.  If t, these warnings are elevated to
errors.  Any other value ignores the warnings completely.

NOMESSAGE is passed directly to `load'."
  (interactive)                         ; what the hell
  (machine-get-files)
  (if machine-files
      (let (files-loaded)
        (dolist (ss machine-files)
          (catch :done
            (dolist (s ss)
              (let ((fn (expand-file-name (format "%s" s)
                                          machine-load-directory)))
                (when (load fn t nomessage)
                  (push fn files-loaded)
                  (throw :done nil))))))
        (unless files-loaded
          (cond ((eq error t) (error "Error loading machine-files.")
                 (null error) (machine--warn "Couldn't load machine-files."))))
        ;; Set up `machine-after-load-theme'.
        (funcall machine-after-load-theme)
        (advice-add 'load-theme :after machine-after-load-theme)
        ;; Return the files loaded.
        files-loaded)
    (funcall (cond ((eq error t) #'error)
                   ((null error) #'machine--warn)
                   (t #'ignore))
             "Couldn't determine the machine being used.")))

(provide 'machine)
;;; machine.el ends here
