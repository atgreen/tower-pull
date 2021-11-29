#|
Copyright (C) 2021  Anthony Green <green@redhat.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
|#

(in-package :tower-pull)

(defvar *tower-server* "example.com")
(defvar *username* nil)
(defvar *password* nil)

(defparameter +jobs-per-page+ 200)

(defun top-level/options ()
  "Creates and returns the options for the top-level command"
  (list
   (clingon:make-option
    :string
    :description "report jobs since this time"
    :short-name #\s
    :long-name "since"
    :key :since)
   (clingon:make-option
    :string
    :description "the Tower server"
    :short-name #\t
    :long-name "server"
    :key :server)
   (clingon:make-option
    :string
    :description "username"
    :short-name #\u
    :long-name "username"
    :key :username)
   (clingon:make-option
    :string
    :description "password"
    :short-name #\p
    :long-name "password"
    :key :password)
   (clingon:make-option
    :counter
    :description "verbosity level"
    :short-name #\v
    :long-name "verbose"
    :key :verbose)))

(defun call-api (api)
  (let ((uri (str:concat "https://" *tower-server* api)))
    (multiple-value-bind (answer error-code)
        (drakma:http-request uri :method :get
                             :basic-authorization `(,*username* ,*password*))
      (when (not (= error-code 200))
        (error "Error ~A reading from tower API: ~A" error-code uri))
      (json:decode-json-from-string (flexi-streams:octets-to-string answer)))))

(defvar *name-cache* (make-hash-table :test 'equal))

(defun lookup-name (api)
  (or
   (gethash api *name-cache*)
   (setf (gethash api *name-cache*)
         (cdr (assoc :name (call-api api))))))

;; An arbitratily far-away date, representing "far in the future"
(defparameter +the-year-3000+ (date-time-parser:parse-date-time "3000"))

(defun tower-time-to-datetime (tt)
  (if (null tt)
      +the-year-3000+
      (progn
        (setf (aref tt 10) #\T)
        (local-time:parse-timestring tt))))

(defun top-level/command ()
  "Creates and return the top-level command"
  (clingon:make-command
   :name "tower-pull"
   :description "pulls job data in csv format"
   :version "1.0.0"
   :license "GPL3"
   :usage "-t <TOWER SERVER> -u <USER NAME> -p <PASSWORD> -s <SINCE WHEN>"
   :options (top-level/options)
   :handler #'top-level/handler
   :authors '("Anthony Green <green@redhat.com>")))

(defun top-level/handler (cmd)
  "The top-level handler"
  (let ((args (clingon:command-arguments cmd))
        (verbose (clingon:getopt cmd :verbose))
        (since (clingon:getopt cmd :since)))
    (if (or (not (eq (length args) 0))
            (not (clingon:getopt cmd :server))
            (not (clingon:getopt cmd :username))
            (not (clingon:getopt cmd :password)))
        (clingon:print-usage *app* t)
        (let ((report-start-time (chronicity:parse since :context :past)))
          (when (null report-start-time)
            (format t "ERROR: Can't parse start time ~S~%" since)
            (sb-ext:quit))
          (setf *username* (clingon:getopt cmd :username))
          (setf *password* (clingon:getopt cmd :password))
          (setf *tower-server* (clingon:getopt cmd :server))
          (loop
            with page = (format nil "/api/v2/jobs?order_by=started&started__gt=~A&page_size=~A&page=1"
                                (subseq (local-time:format-rfc3339-timestring nil report-start-time) 0 19)
                                +jobs-per-page+)
            while page
            do (let* ((json (call-api (quri:url-decode page)))
                      (results (cdr (assoc :results json))))
                 (setf page (cdr (assoc :next json)))
                 (dolist (job results)
                   (let ((related (cdr (assoc :related job))))
                     (let ((org (cdr (assoc :organization related)))
                           (prj (cdr (assoc :project related)))
                           (playbook (cdr (assoc :playbook job)))
                           (started (cdr (assoc :started job))))
                       (when started
                         (let ((start-time (tower-time-to-datetime (cdr (assoc :started job)))))
                           (if (local-time:timestamp< report-start-time start-time)
                               (format t "~S, ~S, ~S, ~S, ~A~%"
                                       (lookup-name org)
                                       (lookup-name prj)
                                       playbook
                                       (local-time:format-rfc3339-timestring nil start-time)
                                       (cdr (assoc :elapsed job)))))))))))))))

(defun main ()
  "The main entrypoint."
  (setf *app* (top-level/command))
  (clingon:run *app*))
