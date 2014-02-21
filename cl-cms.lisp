;;;; cl-cms.lisp

(in-package :cl-cms)

(defvar *server*) 
(defvar *logs* nil) 
(defvar *id* 0)
(defparameter *nodes* '())
(defparameter *edges* (make-hash-table))
(defparameter *usernames* (make-hash-table :test 'equal))

;; Utilities

(defmacro mklist (x)
  `(if (listp ,x) 
      ,x 
      (list ,x)))
; (mklist 1)
; (mklist (1 2 3))

;; Node Utitilies

(defun reset-users ()
  (progn 
    (setq *usernames* (make-hash-table :test 'equal))))

(defun reset-edges ()
  (progn 
    (setq *edges* (make-hash-table))))

(defun reset-nodes ()
  (progn 
    (setf *id* 0)
    (setq *nodes* '())))

(defun reset-all ()
  (progn
    (reset-users)
    (reset-edges)
    (reset-nodes)))

(defun get-id ()
  (setf *id* (1+ *id*)))
; (get-id)

(defun create-node (lst &optional id)
  (progn
    (format *logs* "Creating a node: ~a ~%" lst)
    (if (null id)
        (let ((id (get-id)))
          (if (equal (getf lst :type) "user")
              (let ((password-hash (create-password-hash (getf lst :password)))
                    (username (getf lst :username)))
                (remf lst :password)
                (setf (gethash username *usernames*) id)
                (setf lst (append lst (list :password password-hash)))))
          (push (list id (append lst (list :id id) (list :created-date (timestamp-to-unix (now))))) *nodes*)
          (write-to-string id))
        (progn
          (push (list id lst) *nodes*)
          (write-to-string id)))))
; (create-node '(:type "project" :title "Project Title"))
; (create-node '(:type "task" :title "Task Title"))
; (create-node '(:type "user" :username "nate" :password "fun"))
; (getf (get-node "9") :create-date)
 
(defun delete-node (id)
  (progn
    (setf *nodes* (remove (assoc id *nodes*) *nodes*))
    ""))
; (delete-node 16)

(defun save-node (id lst)
  (let ((created-date (get-node-property id :created-date)))
    (delete-node id)
    (create-node (append lst (list :created-date created-date :updated-date (timestamp-to-unix (now)))) id)))
; (save-node 8 '(:type "typhoon" :title "title"))

(defun get-nodes (ids)
  (let ((lst '()))
    (dolist (id ids)
      (push (cadr (assoc id *nodes*)) lst))
    lst))
; (get-nodes '(1 2 3))

(defun get-edges (id)
  (gethash id *edges*))
; (get-edges 1)
 
(defun get-node (id)
  (progn
    (if (stringp id)
      (setf id (parse-integer id)))
    (let ((edges (get-edges id))
          (node (cadr (assoc id *nodes*))))
      (append node (list :edges edges)))))
;; (get-node 1)
;; (get-node 20)
;; (get-node "14")
;; (get-node "15")

(defun get-type (id)
  (or
    (getf (get-node id) :type)
    (getf (get-node id) 'type)))
; (getf (get-node 1) 'type)
; (get-type 4)
; (get-type "15")

(defun get-node-property (node-id property-name)
  (getf (get-node node-id) property-name))
;; (get-node-property 1 :type)

(defun view-node (&key type limit id)
  (if (null id)
    (progn
      (let ((return-nodes '()))
        (do ((c 0)
             (nodes *nodes* (cdr nodes)))
          ((or (eq c limit) (null nodes)) return-nodes)
          (if (equal (get-type (caar nodes)) type)
            (progn
              (push (attach-edges (car nodes)) return-nodes)
              (incf c))))))
    (list (list id (get-node id)))))
;; (view-node :type "user")
;; (view-node :type "task" :limit -1)
;; (view-node :type "this is 1 yeah yea" :limit 3)
;; (view-node :id 5)

(defun nodes->list (lst)
  "Removes IDs from list of nodes and returns just lists of nodes"
  (let ((result '()))
    (dolist (i lst)
      (push (cadr i) result))
    result))
;; (nodes->list (view-node :type "tree" :limit -1))
;; (nodes->list (view-node :id 5))

(defun nodes->json (lst)
  (let ((return-str "["))
    (dolist (i lst)
      (setf return-str (concatenate 'string return-str (node->json i) ",")))
    (concatenate 'string (string-right-trim "," return-str) "]")))
;; (nodes->json (nodes->list (view-node :type "project" :limit -1)))
;; (nodes->json (nodes->list (view-node :type "project" :limit -1)))

(defun node->json (i)
  (encode-json-plist-to-string i))
;; (node->json (get-node 1))

(defun create-edge (from ids &key end)
  (progn 
    (setf ids (mklist ids))
    (dolist (to ids)
      (let ((lst (gethash from *edges*)))
        (cond ((not lst)
               (setf (gethash from *edges*) (list to)))
              (t (if (not (member to lst)) 
                   (setf (gethash from *edges*) (append lst (list to)))))))
      (unless end (create-edge to from :end t)))))
; (create-edge 1 3)
; (create-edge 3 4) 
; (create-edge 6 7) 
; (create-edge 5 '(7 8 9))
; (create-edge 10 '(7 8 9))

(defun delete-edge (froms ids &key end)
  (progn 
    (format t "Remove ~a from ~a" ids froms)
    (setf ids (mklist ids))
    (setf froms (mklist froms))
    (dolist (from froms)
      (let ((lst (gethash from *edges*)))
        (cond ((listp lst)
               (setf (gethash from *edges*) (set-difference lst ids))))
        (unless end (delete-edge ids from :end t))))))
;; (delete-edge 6 7)
;; (delete-edge 7 '(10 5 6))

(defun save-edge (from ids)
  (progn
    (format *logs* "Saving Edge: ~a ~a~%" from ids)
    (delete-edge from (get-edges from))
    (create-edge from ids)))
;; (save-edge 1 2)
;; (save-edge 1 '(2 3))    
  

(defun print-hash-entry (key value)
      (format t "The value associated with the key ~S is ~S~%" key value))
;; (maphash #'print-hash-entry *edges*)
;; (maphash #'print-hash-entry *usernames*)
         

(defun get-node-edges (edges)
  (get-nodes edges))
;; (get-node-edges (get-edges 1))
   
(defun attach-edges (assoc-node)
  (let ((node (cadr assoc-node)))
    (list (car assoc-node) (append node (list :edges (get-edges (getf node :id)))))))
;; (attach-edges (assoc 1 *nodes*))

;; User Functions

(defun create-password-hash (password)
  (ironclad:pbkdf2-hash-password-to-combined-string (babel:string-to-octets password)))
; (create-password-hash "my cool password")

(defun check-password-hash (password password-hash)
  (ironclad:pbkdf2-check-password (babel:string-to-octets password) password-hash))
; (check-password-hash "fun" (create-password-hash "fun"))
; (check-password-hash "fn" (create-password-hash "fun"))

(defun find-user (username)
  (let ((userid (gethash username *usernames*)))
    (if userid 
        (get-node userid)
        nil)))
;; (find-user "nate")
;; (find-user "not listed")
 
(defun check-user-password (username password)
  (let ((user (find-user username)))
    (if user
        (check-password-hash password (getf user :password))
        nil)))
;; (check-user-password "not listed" "fun")
;; (check-user-password "nate" "fun")

(defun login-user (params)
  (if 
    (check-user-password (getf params :username)
                         (getf params :password))
    (progn
      (hunchentoot:start-session)
      (setf (hunchentoot:session-value :user) (find-user (getf params :username)))
      (format *logs* "Started session for user: ~a" (hunchentoot:session-value :user))
      (encode-json-plist-to-string '(:status "success")))
    (encode-json-plist-to-string '(:status "fail"))))
;; (login-user '(:username "nate" :password "fun"))

(defun check-logged-in ()
  (let ((user (hunchentoot:session-value :user)))
    (format *logs* "Checking logged in for ~a~%" user)
    (remf user :password)
    (encode-json-plist-to-string user)))
;; (check-logged-in)

;; Server

(defun register-rest-handlers ()
  (progn
    (push
      (tbnl:create-prefix-dispatcher "/rest" 'rest-handlers) *dispatch-table*)))
   
(defun rest-handlers ()
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((request-type (hunchentoot:request-method hunchentoot:*request*))
         (uri (hunchentoot:request-uri* hunchentoot:*request*)))
    (cond ((eq request-type :get)
           (get-request (subseq uri (length "/rest/"))))
          ((eq request-type :post)
           (let* ((data-string (hunchentoot:raw-post-data :force-text t)))
             (post-request (subseq uri (length "/rest/"))
                           (post-to-plist (json:decode-json-from-string data-string))))))))

(defun get-request (route)
  (let* ((path-elms (cl-utilities:split-sequence #\/ route :remove-empty-subseqs t))
         (verb (car path-elms))
         (noun (cadr path-elms))
         (id (caddr path-elms)))
    (format *logs* "~%GET REQUEST:~%Path-Elms: ~a~%Verb: ~a~%Noun: ~a~%Id: ~a~%" path-elms verb noun id)
    (cond 
      ((equal verb "view")
       (cond
         ((equal noun "user") 
          (check-logged-in))
         ((equal id "all") 
          (nodes->json (nodes->list (view-node :type noun :limit -1)))))))))

(defun post-request (route params)
   (let* ((path-elms (cl-utilities:split-sequence #\/ route :remove-empty-subseqs t))
          (verb (car path-elms))
          (noun (cadr path-elms)))
    (format *logs* "~%POST REQUEST: ~a ~a ~a~%" route params path-elms)
    (or 
      (if (equal verb "login")
          (login-user params))
      (if (equal verb "view")
          (cond ((equal noun "node")
                 (let ((limit (or (getf params :limit) -1)))
                    (format *logs* "Limit: ~a~%" limit)
                    (format *logs* "id ~a~%" (getf params :id))
                    (if (not (getf params :id)) ; (not (getf '(:ID 5) :id))
                        (nodes->json (nodes->list (view-node :type (getf params :type) :limit limit)))
                        (nodes->json (nodes->list (view-node :id (getf params :id)))))))
                ((equal noun "edge")
                 (nodes->json (get-node-edges (get-edges (getf params :id)))))))
      ; (get-edges 5)
      ; (get-node-edges (get-edges 5))
      ; (nodes->json (get-node-edges (get-edges 5)))
      (if (equal verb "save")
          (cond ((equal noun "node")
                 (save-node (getf params :id) params))
                ((equal noun "edge")
                 (save-edge (getf params :id) (getf params :edges)))))
      (if (equal verb "create")
        (cond ((equal noun "node")
               (progn
                 (format *logs* "Creating node~%")
                 (create-node params)))
              ((equal noun "edge")
               (progn
                 (format *logs* "Creating edge~%")
                 (create-edge (getf params :id) (getf params :edges)))))))))
          
;;    (cond 
;;      ((equal route "create-organization")
;;       (apply #'create-organization params)
;;       "")
;;      ((equal route "create-project")
;;       (apply #'create-project params)
;;       "")
;;      ((equal route "create-task")
;;       (apply #'create-task params)
;;       "")
;;      ((equal route "create-user")
;;       (apply #'create-user params)
;;       "")
;;      ((equal route "create-comment")
;;       (apply #'create-comment params)
;;       ""))))

;; Server io utilities 

(defun send-json (x)
    (encode-json-alist-to-string x))

(defun post-to-plist (post-data)
  (let ((lst (list)))
  (dolist (i post-data)
    (push (car i) lst)
    (push (cdr i) lst))
  (nreverse lst)))

;; (defun post-to-alist (post-data)
;;   (let ((lst (list)))
;;   (dolist (i post-data)
;;     (push (list (car i) (cdr i)) lst))
;;   (nreverse lst)))


;; Application utilities

(defun start-hunchentoot (name port) 
  (progn 
    (setf *logs* (open (concatenate 'string "/tmp/" name "-lisp-log.txt") :direction :output :if-exists :append :if-does-not-exist :create))
    (setf *server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port
                                                       :access-log-destination *logs*
                                                       :message-log-destination *logs*)))))
  
(defun stop-server () 
  (progn
    (close *logs*)  
    (hunchentoot:stop *server*)
    (stop-logging)))

(defun stop-logging ()
  (setf (acceptor-access-log-destination *server*) nil))

(defun start-server (name-str port)
  (progn
    (register-rest-handlers)
    (start-hunchentoot name-str port)))

  

