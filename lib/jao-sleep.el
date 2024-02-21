;;; jao-sleep.el --- Actions upon sleep/awake        -*- lexical-binding: t; -*-

;; Copyright (C) 2020  jao

;; Author: jao <mail@jao.io>
;; Keywords: hardware

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'dbus)

(defvar jao-sleep-sleep-functions nil)
(defvar jao-sleep-awake-functions nil)

(defvar jao-sleep--dbus-registration-object nil)

(defun jao-sleep--dbus-sleep-handler (sleep-start)
  (condition-case nil
      (if sleep-start
          (progn (message "Running on sleep functions")
                 (run-hooks 'jao-sleep-sleep-functions))
        (message "Running on awake functions")
        (run-hooks 'jao-sleep-awake-functions))
      (error (message "There was an error running %s" sleep-start))))

;;;###autoload
(defun jao-sleep-dbus-register (&optional session-dbus)
  "Register actions to take on sleep and on awake, using the system D-BUS."
  (when (featurep 'dbusbind)
    (setq jao-sleep--dbus-sleep-registration-object
	  (dbus-register-signal (if session-dbus :session :system)
				"org.freedesktop.login1"
				"/org/freedesktop/login1"
				"org.freedesktop.login1.Manager"
				"PrepareForSleep"
				#'jao-sleep--dbus-sleep-handler))))

;;;###autoload
(defun jao-sleep-dbus-unregister ()
  (condition-case nil
      (dbus-unregister-object jao-sleep--dbus-sleep-registration-object)
    (wrong-type-argument nil)))



(provide 'jao-sleep)
;;; jao-sleep.el ends here
