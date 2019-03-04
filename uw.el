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
         '(("Content-Type" . "application/x-www-form-urlencoded")
           ("Referer" . "https://idp.u.washington.edu/")))
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
  ;; URL module follows up to 30 redirects by default, but lets make sure
  (let ((url-max-redirections 5))
    (with-current-buffer (url-retrieve-synchronously uw-login-link)
      (uw-unentitize-current-buffer)
      (uw-find-form-action))))

(defun uw-login nil
  "Perform NetID login. Performs pre-login first"
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
  (with-current-buffer (uw-status-page)
    ;; don't *really* need this, but good practice?
    (save-excursion
      (goto-char (point-min))
      (message "%s" (if (search-forward uw-undecided-text nil t)
                        "No decision has been made."
                      "PORTAL UPDATED!!!! Only the best of luck.")))))

;; THE REST OF THIS FILE IS NOT ORIGINAL CODE
;; IT IS COPIED FROM THE EMACS GIT TREE
;; This is because, at the time of writing, it has not been included in an
;; emacs release yet but contains critical bug fixes for the handling of 302
;; redirects in the URL library

(defun url-http-parse-headers () "Parse and handle HTTP specific headers.Return t if and only if the current buffer is still active andshould be shown to the user."  (url-http-mark-connection-as-free (url-host url-current-object)				    (url-port url-current-object)				    url-http-process)  (when (gnutls-available-p)    (let ((status (gnutls-peer-status url-http-process)))      (when (or status		(plist-get (car url-callback-arguments) :peer))	(setcar url-callback-arguments		(plist-put (car url-callback-arguments)			   :peer status)))))  (if (or (not (boundp 'url-http-end-of-headers))	  (not url-http-end-of-headers))      (error "Trying to parse headers in odd buffer: %s" (buffer-name)))  (goto-char (point-min))  (url-http-debug "url-http-parse-headers called in (%s)" (buffer-name))  (url-http-parse-response)  (mail-narrow-to-head)  (let ((connection (mail-fetch-field "Connection")))    (cond     ((string= url-http-response-version "1.0")      (unless (and connection		   (string= (downcase connection) "keep-alive"))	(delete-process url-http-process)))     (t      (when (and connection		 (string= (downcase connection) "close"))	(delete-process url-http-process)))))  (let* ((buffer (current-buffer))         (class (/ url-http-response-status 100))         (success nil)         (status-symbol (cadr (assq url-http-response-status url-http-codes))))    (url-http-debug "Parsed HTTP headers: class=%d status=%d"                    class url-http-response-status)    (when (url-use-cookies url-http-target-url)      (url-http-handle-cookies))    (pcase class      (1				       (url-mark-buffer-as-dead buffer)       (error "HTTP responses in class 1xx not supported (%d)"              url-http-response-status))      (2				       (pcase status-symbol	 ((or 'no-content 'reset-content)		  (url-mark-buffer-as-dead buffer))	 (_			  (widen)	  (if (and url-automatic-caching (equal url-http-method "GET"))	      (url-store-in-cache buffer))))       (setq success t))      (3				       (let ((redirect-uri (or (mail-fetch-field "Location")			       (mail-fetch-field "URI"))))	 (pcase status-symbol	   ('multiple-choices	    																    nil)           ('found							    (setq url-http-method "GET"		  url-http-data nil))           ('see-other							    (setq url-http-method "GET"		  url-http-data nil))	   ('not-modified				    (url-http-debug "Extracting document from cache... (%s)"			    (url-cache-create-filename (url-view-url t)))	    (url-cache-extract (url-cache-create-filename (url-view-url t)))	    (setq redirect-uri nil		  success t))	   ('use-proxy									    (error "Redirection thru a proxy server not supported: %s"		   redirect-uri))	   (_		    nil))	 (when redirect-uri		   (if (string-match "\\([^ \t]+\\)[ \t]" redirect-uri)	       (setq redirect-uri (match-string 1 redirect-uri)))	   (if (string-match "^<\\(.*\\)>$" redirect-uri)	       (setq redirect-uri (match-string 1 redirect-uri)))				   (if (not (string-match url-nonrelative-link redirect-uri))	       (setq redirect-uri		     (url-expand-file-name redirect-uri url-http-target-url)))				   (setq url-http-extra-headers		 (cl-remove "Authorization"			    url-http-extra-headers :key 'car :test 'equal))           (let ((url-request-method url-http-method)		 (url-request-data url-http-data)		 (url-request-extra-headers url-http-extra-headers))		     (if (or (< url-max-redirections 0)		     (and (> url-max-redirections 0)			  (let ((events (car url-callback-arguments))				(old-redirects 0))			    (while events			      (if (eq (car events) :redirect)				  (setq old-redirects (1+ old-redirects)))			      (and (setq events (cdr events))				   (setq events (cdr events))))			    (< old-redirects url-max-redirections))))						 (progn				   (setf (car url-callback-arguments)			 (nconc (list :redirect redirect-uri)				(car url-callback-arguments)))														   (set (make-local-variable 'url-redirect-buffer)			(url-retrieve-internal			 redirect-uri url-callback-function			 url-callback-arguments			 (url-silent url-current-object)			 (not (url-use-cookies url-current-object))))		   (url-mark-buffer-as-dead buffer))			       (url-http-debug "Maximum redirections reached")	       (setf (car url-callback-arguments)		     (nconc (list :error (list 'error 'http-redirect-limit					       redirect-uri))			    (car url-callback-arguments)))	       (setq success t))))))      (4				       (setq success             (pcase status-symbol               ('unauthorized			                (url-http-handle-authentication nil))               ('payment-required                              (url-mark-buffer-as-dead buffer)                (error "Somebody wants you to give them money"))               ('forbidden			                t)               ('not-found			                t)               ('method-not-allowed		                t)               ('not-acceptable		                t)               ('proxy-authentication-required                 (url-http-handle-authentication t))               ('request-timeout		                t)               ('conflict			                t)               ('gone                                          t)               ('length-required		                t)               ('precondition-failed		                t)               ((or 'request-entity-too-large 'request-uri-too-large)                 t)               ('unsupported-media-type	                t)               ('requested-range-not-satisfiable                 t)               ('expectation-failed		                t)               (_                t)))       (when success	 (setf (car url-callback-arguments)	       (nconc (list :error (list 'error 'http url-http-response-status))		      (car url-callback-arguments)))))      (5       (setq success t)       (pcase url-http-response-status	 ('not-implemented					  nil)	 ('bad-gateway							  nil)	 ('service-unavailable										  nil)	 ('gateway-timeout								  nil)	 ('http-version-not-supported					  nil)	 ('insufficient-storage										  nil))       (when success	 (setf (car url-callback-arguments)	       (nconc (list :error (list 'error 'http url-http-response-status))		      (car url-callback-arguments)))))      (_       (error "Unknown class of HTTP response code: %d (%d)"	      class url-http-response-status)))    (if (not success)	(url-mark-buffer-as-dead buffer)      (url-handle-content-transfer-encoding))    (url-http-debug "Finished parsing HTTP headers: %S" success)    (widen)    (goto-char (point-min))success))
