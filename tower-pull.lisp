(in-package :tower-pull)

(defvar *tower-server* "example.com")
(defvar *username* nil)
(defvar *password* nil)

(defparameter +jobs-per-page+ 20)

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

(defun get-job-count ()
  (let ((jobs (call-api "/api/v2/jobs/")))
    (cdr (assoc :count jobs))))

(defun get-last-job-page-number ()
  (multiple-value-bind (page remainder)
      (floor (get-job-count) +jobs-per-page+)
    (+ page (if (> remainder 0) 1 0))))

(defvar *name-cache* (make-hash-table :test 'equal))

(defun lookup-name (api)
  (or
   (gethash api *name-cache*)
   (setf (gethash api *name-cache*)
         (cdr (assoc :name (call-api api))))))

(defun tower-time-to-datetime (tt)
  (setf (aref tt 10) #\T)
  (local-time:parse-timestring tt))

(defun find-page-by-date (max-page-number date)
  (loop
    with l = 1
    and r = max-page-number
    for m = (floor (+ l r) 2)
    for mth-page = (call-api (format nil "/api/v2/jobs?order_by=started&page_size=~A&page=~A" +jobs-per-page+ m))
    if (let ((started (tower-time-to-datetime
                       (cdr (assoc :started
                                   (car (cdr (assoc :results mth-page))))))))
         (format t "Searching for start date on page ~A...~%" m)
         (local-time:timestamp< started date))
      do (setf l m)
    else do (setf r m)
    until (<= (- r l) 1)
    finally (return (max (- m 1) 1))))

(defun top-level/command ()
  "Creates and return the top-level command"
  (clingon:make-command
   :name "tower-pull"
   :description "pulls job data in csv format"
   :version "1.0.0"
   :license "AGPL3"
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
        (let ((report-start-time (chronicity:parse since)))
          (setf *username* (clingon:getopt cmd :username))
          (setf *password* (clingon:getopt cmd :password))
          (setf *tower-server* (clingon:getopt cmd :server))
          (loop for page from (find-page-by-date (get-last-job-page-number) report-start-time) upto (get-last-job-page-number)
                do (let* ((json (call-api (format nil "/api/v2/jobs?order_by=started&page_size=~A&page=~A" +jobs-per-page+ page)))
                          (results (cdr (assoc :results json))))
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
                                           (local-time:format-rfc3339-timestring nil (tower-time-to-datetime (cdr (assoc :started job))))
                                           (cdr (assoc :elapsed job)))))))))))))))

(defun main ()
  "The main entrypoint."
  (setf *app* (top-level/command))
  (clingon:run *app*))
