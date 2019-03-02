;; Checks whether your UW admissions status has changed. Stop checking the portal!
;; Basic usage:
;; 1. Install this file by `load'ing it in your init.el or equivalent.
;; 2. Setup your UW NetID credentials in init.el or equivalent:
;; (setq uw-netid-username "johndoe"
;;       uw-netid-password "hunter2")
;; 3. Get the undecided hash of the status page, like a "control group": M-x uw-status-hash
;; 4. Update your init.el to include this hash: (setq uw-expected-hash "1234567890abcdef")
;; 5. Setup is done! M-x uw-check to see if your portal has updated yet!
;;
;; Authored by Mark Polyakov. Released under the GPL v3 or later.

;; vaiables that should be overridden
(defvar uw-netid-username nil "University of Washington NetID login")
(defvar uw-netid-password nil "University of Washington NetID password")
(defvar uw-expected-hash nil "University of Washington expected login page hash")

;; variables that typically shouldn't be overridden but are documented anyway
(defvar uw-login-bookmark "https://my.uw.edu/saml/login?next=/"
  "The link that starts off the redirect process which initializes important cookies and finally redirects to the main login page")
(defvar uw-idp-domain "https://idp.u.washington.edu"
  "The domain of the login form action")

(defun uw-pre-login nil
  "Make requests which setup the cookies that must be set before login.
Returns the login action URL"
  ;; URL module follows up to 30 redirects by default, but lets make sure
  (let ((url-max-redirections 5))
    (with-current-buffer (url-retrieve-synchronously uw-login-bookmark)
      (goto-char (point-min)) ;; point starts at the end of buffer
      (let ((action-start-point (search-forward "action=\"")))
        (search-forward "\"")
        (backward-char)
        (buffer-substring-no-properties action-start-point (point))
      ))))

(defun uw-login nil
  (let ((uw-login-action (uw-pre-login))
        (url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data (concat
                           "j_username=" (url-hexify-string uw-netid-username)
                           "&j_password=" (url-hexify-string uw-netid-password)
                           "&_eventId_proceed=Sign+in")))
    (url-retrieve-synchronously (concat uw-idp-domain uw-login-action))
      ))

(defun uw-status-hash nil
  (interactive)
  (switch-to-buffer (uw-login))
  )

;; Use `uw-netid-username' and `uw-netid-password' to set your credentials.
;; Then, run `uw-hash-collect' to find what you should set fo `uw-expected-hash'"
;;   (interactive)
;;   (if (and
;;        (bound-and-true-p uw-netid-username)
;;        (bound-and-true-p uw-netid-password)
;;        (bound-and-true-p uw-expected-hash))
;;       (uw-check-portal-args uw-netid-username uw-netid-password uw-expected-hash)
;;     (error "Some required variables were not set! Check documentation."))
