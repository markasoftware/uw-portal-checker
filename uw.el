;; University of Washington automatic admissions portal checker.
;; Copyright (C) 2019     Mark Polyakov
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Basic usage:
;; 1. Install this file by `load'ing it in your init.el or equivalent.
;; 2. Setup your UW NetID credentials in init.el or equivalent:
;; (setq uw-netid-username "johndoe"
;;       uw-netid-password "hunter2")
;; 3. Setup is done! M-x uw-check to see if your portal has updated yet!

;; vaiables that should be overridden
(defvar uw-netid-username nil "University of Washington NetID login")
(defvar uw-netid-password nil "University of Washington NetID password")
(defvar uw-expected-hash nil "University of Washington expected login page hash")

;; variables that typically shouldn't be overridden but are documented anyway
(defvar uw-login-link "https://my.uw.edu/"
  "The link that starts off the redirect process which initializes important cookies and finally redirects to the main login page")
(defvar uw-idp-domain "https://idp.u.washington.edu"
  "The domain of the login form action")
(defvar uw-my-sso-link "https://my.uw.edu/saml/sso"
  "The SSO post action.")
(defvar uw-application-status-link "https://my.uw.edu/out?u=https://sdb.admin.uw.edu/admissions/uwnetid/appstatus.asp"
  "The page which is updated when decisions are announced.")
(defvar uw-undecided-text "This message will not change unless additional information is required or until a final admission decision is made."
  "The text on the application status page which indicates that no decision has been made yet")

(defun uw-unentitize-current-buffer nil
  "Un-escape all hex HTML entities in current buffer (like &#x3a -> :)"
  (let ((initial-point (point)))
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward "&#x\\([a-fA-F0-9]+\\);" nil t)
          (replace-match (string (string-to-number (match-string-no-properties 1) 16)))))
      (goto-char initial-point)))

(defun uw-find-input-value (input-name)
  "Find the value of an input with the given name in the current buffer"
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (re-search-forward (concat "<input .*name=\"" input-name "\" .*value=\"\\([^\"]+\\)"))
      (match-string-no-properties 1))))

(defun uw-find-form-action nil
  "Find the action URL of the first form in the current buffer"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "action=\"\\([^\"]+\\)")
    (let ((raw-action (match-string-no-properties 1)))
      (if (equal (string-to-char raw-action) ?/)
          (concat uw-idp-domain raw-action)
        raw-action))))

(defun uw-submit-urlencoded-form (request-data link)
  "Submit the URL-encoded request-data to link synchronously, returning the buffer"
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data request-data))
    (url-retrieve-synchronously link)))

(defun uw-submit-sso-form nil
  "Find an SSO form in the current buffer and submit it synchronously.
Make sure the buffer is already unentitized"
  (uw-submit-urlencoded-form (concat
                              "RelayState="
                              (url-hexify-string (uw-find-input-value "RelayState"))
                              "&SAMLResponse="
                              (url-hexify-string (uw-find-input-value "SAMLResponse")))
                             (uw-find-form-action)))

(defun uw-sso-p nil
  "Determine whether the current buffer is an SSO loading page"
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (search-forward "name=\"SAMLResponse\"" nil t))))

(defun uw-follow-sso-redirects nil
  "After a request, call this to follow any number of SSO redirects and normal 302s. Returns the final buffer"
  (save-excursion
    (uw-unentitize-current-buffer)
    (while (uw-sso-p)
      (set-buffer (uw-submit-sso-form))
      (uw-unentitize-current-buffer))
    (current-buffer)))

(defun uw-retrieve-sso-url-synchronously (link)
  "Retrieve a page then follow all SSO redirects."
  (with-current-buffer (url-retrieve-synchronously link)
    (uw-follow-sso-redirects)))

(defun uw-pre-login nil
  "Make requests which setup the cookies that must be set before login. Also deletes existing cookies.
Returns the login action URL"
  (when (functionp 'url-cookie-delete-cookies)
    (url-cookie-delete-cookies "uw\\.edu")
    (url-cookie-delete-cookies "washington\\.edu"))
  (with-current-buffer (url-retrieve-synchronously uw-login-link)
    (uw-unentitize-current-buffer)
    (uw-find-form-action)))

(defun uw-login nil
  "Perform NetID login. Performs pre-login first. Returns whether login succeeded"
  (let ((uw-login-action (uw-pre-login)))
    (with-current-buffer
        (uw-submit-urlencoded-form
         (concat
          "j_username=" (url-hexify-string uw-netid-username)
          "&j_password=" (url-hexify-string uw-netid-password)
          "&_eventId_proceed=Sign+in")
         uw-login-action)
      (uw-follow-sso-redirects))))

(defun uw-status-page nil
  "Get the status page. Performs NetID login first"
  (uw-login)
  (uw-retrieve-sso-url-synchronously uw-application-status-link))

(defun uw-check nil
  "Display messages based on whether a decision has been made yet.
This is the primary user-facing function of the UW Portal checker package."
  (interactive)
  (unless (and
           (bound-and-true-p uw-netid-username)
           (bound-and-true-p uw-netid-password))
    (error "uw-netid-username and uw-netid-password must be set"))
  (with-current-buffer (uw-status-page)
    ;; we could use save-excursion and save-match-data here, but that buffer
    ;; will never be used again, so why bother?
    (goto-char (point-min))
    (message "%s" (if (search-forward uw-undecided-text nil t)
                      "No decision has been made."
                    "PORTAL UPDATED!!!! Only the best of luck."))))
