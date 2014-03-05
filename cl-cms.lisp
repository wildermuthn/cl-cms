;;;; cl-cms.lisp

(in-package :cl-cms)

(defvar *server*) 
(defvar *logs* nil) 
(defvar *id* 0)
(defvar *node-version* 0)
(defvar *edge-version* 0)
(defvar *username-version* 0)
(defparameter *sample* '())
(defvar *nodes* '())
(defvar *edges* (make-hash-table))
(defvar *usernames* (make-hash-table :test 'equal))

;;; Utilities

(defmacro mklist (x)
  `(if (listp ,x) 
      ,x 
      (list ,x)))
; (mklist 1)
; (mklist (1 2 3))

(defmacro pop2 (x)
  `(progn 
     (pop ,x)
     (pop ,x)))
; (pop2 '(one two three four))


;;; Node Utitilies

(defmacro backup-type (type)
  (let (
        (type-string (string-upcase (concatenate 'string (symbol-name type) "s"))) 
        (variable-symbol (intern (string-upcase (concatenate 'string "*" (symbol-name type) "s*"))))
        (version-symbol (intern (string-upcase (concatenate 'string "*" (symbol-name type) "-version*")))))
    `(backup-to-disk ,type-string ,variable-symbol ,version-symbol)))
; (backup-type node)

(defmacro get-version (x)
  `(incf ,x))
; (get-version *node-version*)
; (get-version *edge-version*)
; (get-version *usernames-version*)
 
(defmacro backup-to-disk (file-name data type)
  (let ((file-name (string-downcase file-name)))
    `(cl-store:store ,data (concatenate 'string "/srv/logs/" ,file-name "-v" (write-to-string (get-version ,type)) ".db"))))
; (backup-to-disk "nodes" *nodes* *node-version*)
; (backup-type node)
; (backup-type username)
; (backup-type edge)

(defmacro restore-type (type &optional id)
   (let* ((type-string (string-downcase (concatenate 'string (symbol-name type) "s"))) 
          (version-symbol (intern (string-upcase (concatenate 'string "*" (symbol-name type) "-version*")))))
     `(progn
        (if (not ,id)
            (restore-from-disk ,type-string ,version-symbol)
            (restore-from-disk ,type-string ,id)))))
; (restore-type node)
; (restore-type edge)
; (restore-type username)
; (restore-type edge 1)

(defun restore-from-disk (file-name id)
  (cl-store:restore (concatenate 'string "/srv/logs/" file-name "-v" (write-to-string id) ".db")))
; (setf *sample-nodes* (restore-from-disk "nodes" 6))
; (setf *sample-nodes* (restore-from-disk "nodes"))

(defun save-data ()
  (progn
    (backup-type node)
    (backup-type username)
    (backup-type edge)))
; (save-data)

(defun restore-data ()
  (progn
    (setf *nodes* (restore-type node))
    (setf *usernames* (restore-type username))
    (setf *edges* (restore-type edge))))
; (restore-data) 
    
(defun reset-users ()
  (progn 
    (setf *username-version* 0)
    (setq *usernames* (make-hash-table :test 'equal))))

(defun reset-edges ()
  (progn 
    (setf *edge-version* 0)
    (setq *edges* (make-hash-table))))

(defun reset-nodes ()
  (progn 
    (setf *node-version* 0)
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
    (delete-all-edges id)
    ""))
; (delete-node 8)

(defun save-node (id lst)
  (let ((created-date (get-node-property id :created-date)))
    (delete-node id)
    (create-node (append lst (list :created-date created-date :updated-date (timestamp-to-unix (now)))) id)))
; (save-node 8 '(:type "typhoon" :title "title"))

(defun get-nodes (ids)
  (let ((lst '()))
    (dolist (id ids)
      (let ((node (cadr (assoc id *nodes*))))
        (unless (not node) (push node lst))))
    lst))
; (get-nodes '(1 2 3))
; (get-nodes '(1 2 4))

(defun get-node (id)
  (progn
    (if (stringp id)
      (setf id (parse-integer id)))
    (cadr (assoc id *nodes*))))
; (get-node 1)
; (get-node 20)
; (get-node "14")
; (get-node "15") ; what

(defun get-type (id)
  (or
    (getf (get-node id) :type)
    (getf (get-node id) 'type)))
; (getf (get-node 1) 'type)
; (get-type 4)
; (get-type "15")

(defun get-node-property (node-id property-name)
  (getf (get-node node-id) property-name))
; (get-node-property 1 :type)

(defun view-node (&key type limit id)
  (if (null id)
    (progn
      (let ((return-nodes '()))
        (do ((c 0)
             (nodes *nodes* (cdr nodes)))
          ((or (eq c limit) (null nodes)) return-nodes)
          (if (equal (get-type (caar nodes)) type)
            (progn
              (push (car nodes) return-nodes)
              (incf c))))))
    (list (list id (get-node id)))))
; (view-node :type "user")
; (view-node :type "project")
; (view-node :type "task" :limit -1)
; (view-node :type "this is 1 yeah yea" :limit 3)
; (view-node :id 5)

(defun nodes->list (lst)
  "Removes IDs from list of nodes and returns just lists of nodes"
  (let ((result '()))
    (dolist (i lst)
      (push (cadr i) result))
    result))
; (nodes->list (view-node :type "tree" :limit -1))
; (nodes->list (view-node :id 5))

(defun nodes->json (lst)
  (let ((return-str "["))
    (dolist (i lst)
      (setf return-str (concatenate 'string return-str (node->json i) ",")))
    (concatenate 'string (string-right-trim "," return-str) "]")))
; (nodes->json (nodes->list (view-node :type "project" :limit -1)))
; (nodes->json (nodes->list (view-node :type "project" :limit -1)))
; (nodes->json (nodes->list (view-node :id 1)))

(defun node->json (i)
  (encode-json-plist-to-string i))
; (node->json (get-node 7))

;;; Edges

(defun add-edge-to-json (lst edge)
  (concatenate 'string (subseq lst 0 (- (length lst) 2)) ",\"edge\":" edge "}]"))
; (add-edge-to-json (nodes->json (view-node-request '(:id 2))) (get-edges->json 2))

(defun get-edges (id)
  (gethash id *edges*))
; (get-edges 1)

(defun get-edge-direction (id direction)
  (getf (get-edges id) direction))
; (get-edge-direction 1 :to)

(defun get-edge-direction->types (id direction)
  (let ((lst (get-edge-direction id direction)))
    (do ((result '()))
        ((not lst) result)
        (unless (member (cadr lst) result :test #'equal) (push (cadr lst) result))
        (pop2 lst))))
; (get-edge-direction->types 1 :to)

(defun get-edge-type->ids (id type direction)
  "Get node ids of all edges for node 'id', filtering for type and direction"
  (let ((lst (get-edge-direction id direction))
        (result '()))
    (do ((x (car lst) (car lst))
         (y (cadr lst) (cadr lst)))
        ((not lst) (reverse result))
        (if (equal y type) (push x result))
        (pop lst)
        (pop lst))))
; (get-edge-type->ids 1 "project" :to)
; (get-edge-type->ids 1 "project" :from)

(defun get-edge-type->nodes (id direction)
  (let ((result '()))
    (dolist (type (get-edge-direction->types id direction))
      (let ((nodes 
              (do ((nlst (get-nodes (get-edge-type->ids id type direction)))
                   (vals '()))
                  ((not nlst) vals)
                  (push (plist-to-dotlist (car nlst)) vals)
                  (pop nlst))))
      (setf result (append result `(,(intern (string-upcase type) "KEYWORD") . (,nodes))))))
    result))

; (get-edge-type->nodes 1 :to)
; (node->json (get-edge-type->nodes 1 :to))
; (node->json (get-edge-type->nodes 1 :from))
; (node->json (get-node 9))
 
(defun wrap-json-object (id val)
  (concatenate 'string "\"" id "\":" val ""))
; (wrap-json-object "to" (node->json (get-edge-type->nodes 1 :to)))

(defun get-edges->json (id) 
  (concatenate 'string "{"
                       (wrap-json-object "to" (node->json (get-edge-type->nodes id :to)))
                       ","
                       (wrap-json-object "from" (node->json (get-edge-type->nodes id :from)))
                       "}"))
; (get-edges->json 1)

(defun opposite-direction (direction) 
  (if (equal :from direction)
      :to
      :from))
; (opposite-direction :to)
; (opposite-direction :from)

(defun plist-number->type (n lst direction)
  "Get unique list of types associated with id n in a list with two directions, using direction
   
   Example: 
   
        CL-USER> (plist-number->type 1 '(:to (1 \"task\" 3 \"project\" 1 \"pizza\") :from nil) :to) 
        (\"task\" \"pizza\")
  "
  (let ((lst (getf lst direction)))
    (do ((result '())
         (type (getf lst n) (getf lst n)))
      ((not lst) (mklist result))
      (unless (or (not type)
              (member type result :test #'equal))
        (push type result))
      (pop lst)
      (pop lst))))

; (plist-number->type 1 '(:to (1 "task" 3 "project" 1 "pizza") :from nil) :to) 
; (plist-number->type 1 (:to ( :from)
; (plist-number->type 2 (gethash 1 *edges*) :from)
; (plist-number->type 2 (gethash 1 *edges*) :to)
; (plist-number->type 1 (gethash 2 *edges*) :to)

(defun create-edge (from ids &key end type direction)
  "Create an edge from node from to node ids (can be a list), with type and direction. Automatically links two-way"
  (progn 
    (setf ids (mklist ids))
    (dolist (to ids)
      (let ((lst (gethash from *edges*)))
        (cond ((not lst)
               (setf (gethash from *edges*) `(,direction (,to ,type))))
              (t (unless (member type (plist-number->type to lst direction) :test #'equal)
                           (setf (getf (gethash from *edges*) direction) 
                                 (append (getf (gethash from *edges*) direction) (list to type)))))))
      (unless end (create-edge to from :end t :type type :direction (opposite-direction direction))))))
; (reset-edges)
; (format t "hi")
; (maphash #'print-hash-entry *edges*)
; (create-edge 2 1 :type "project" :direction :to)
; (create-edge 2 1 :type "task" :direction :to)
; (create-edge 1 2 :type "thumb" :direction :to)
; (create-edge 1 2 :type "project" :direction :to)
; (create-edge 1 '(2 3 4) :type "project" :direction :to)
; (create-edge 1 '(2 3 4) :type "project" :direction :to)

(defun fn-delete-edge (from id &key type direction)
  "Functional style, returns new list of edge for type and direction"
      (let ((lst (gethash from *edges*)))
        (cond ((listp lst)
               (let ((p-list (getf (gethash from *edges*) direction))
                     (result '()))
                 (do ((p-id (car p-list) (car p-list))
                      (p-val (cadr p-list) (cadr p-list)))
                     ((not p-list) (reverse result))
                     (if (and (equal p-id id) (equal p-val type))
                         (progn
                           (pop p-list)
                           (pop p-list))
                         (progn
                           (push p-id result)
                           (push p-val result)
                           (pop p-list)
                           (pop p-list)))))))))
; (fn-delete-edge 1 2 :type "project" :direction :to)
; (opposite-direction :from)

(defun n-delete-edge (from id &key type direction end)
  "Destructive function to delete a piece from an edge"
  (progn
    (unless (not (getf (gethash from *edges*) direction))
      (setf (getf (gethash from *edges*) direction) (apply #'fn-delete-edge (list from id :type type :direction direction))))
    (when (not end)
      (n-delete-edge id from :type type :direction (opposite-direction direction) :end t))))
; (n-delete-edge 2 1 :type "project" :direction :to)
; (n-delete-edge 2 1 :type "task" :direction :to)
; (n-delete-edge 3 1 :type "project" :direction :to)
; (n-delete-edge 1 2 :type "task" :direction :from)
; (n-delete-edge 4 1 :type "project" :direction :from)

(defun delete-direction-edges (id direction)
  (let ((edge (getf (get-edges id) direction)))
    (do ((nid (car edge) (car edge))
         (type (cadr edge) (cadr edge)))
        ((not edge))
        (format t "~a ~a ~a~%" direction nid type)
        (n-delete-edge id nid :type type :direction direction)
        (pop2 edge))))  
; (delete-direction-edges 1 :to)
; (delete-direction-edges 1 :from)
; (delete-direction-edges 1 :from)
; (cadr (getf (get-edges 1) :from))
    
(defun delete-all-edges (id)
  (progn 
    (delete-direction-edges id :to)
    (delete-direction-edges id :from)))
; (delete-all-edges 2)
; (delete-all-edges 1)

(defun print-hash-entry (key value)
      (format t "The value associated with the key ~S is ~S~%" key value))
; (maphash #'print-hash-entry *edges*)
; (maphash #'print-hash-entry *usernames*)
         
;;; User Functions

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
; (find-user "nate")
; (find-user "not listed")
 
(defun check-user-password (username password)
  (let ((user (find-user username)))
    (if user
        (check-password-hash password (getf user :password))
        nil)))
; (check-user-password "not listed" "fun")
; (check-user-password "nate" "fun")
; (check-user-password "nate" "fn")

(defun plist-to-dotlist (plist)
  (let ((lst '()))
    (do ((prop (car plist) (car plist))
         (value (cadr plist) (cadr plist)))
        ((not plist) lst)
        (push (cons prop value) lst)
        (pop plist)
        (pop plist))))
; (plist-to-dotlist (find-user "nate"))

(defun start-user-session (params user)
  (progn
    (hunchentoot:start-session)
    (setf (hunchentoot:session-value :user) (find-user (getf params :username)))
    (format *logs* "Started session for user: ~a" (hunchentoot:session-value :user))
    (encode-json-plist-to-string `(:status "success" :data ,(plist-to-dotlist user)))))

(defun login-user (params)
  (let ((current-user (find-user (getf params :username))))
    (cond 
      ((check-user-password (getf params :username)
                            (getf params :password))
       (start-user-session params current-user))
      ((not current-user)
       (create-node `(:type "user" :username ,(getf params :username) :password ,(getf params :password)))
       (start-user-session params (find-user (getf params :username))))
      (t (encode-json-plist-to-string '(:status "fail"))))))
; (login-user '(:username "nate" :password "fun"))

(defun check-logged-in ()
  (let ((user (hunchentoot:session-value :user)))
    (format *logs* "Checking logged in for ~a~%" user)
    (remf user :password)
    (encode-json-plist-to-string user)))
; (check-logged-in)

;;; Permissions

(defparameter *user-permissions* (make-hash-table :test 'equal))
(setf (gethash "anonymous" *user-permissions*) '(view (node edge)
                                                 create (node ("comment"))))
(setf (gethash "logged-in" *user-permissions*) '(view (node edge)
                                                 create (node ("comment" "project") edge)))
(setf (gethash "admin" *user-permissions*) '(create (node edge)
                                             delete (node edge)
                                             save (node edge)))

(defun check-permission (user verb noun params)
  (let ((verb (intern (string-upcase verb)))
        (noun (intern (string-upcase noun))))
    (if (equal verb 'remove) (setf verb 'delete))
    (let ((perm (member noun (getf (gethash user *user-permissions*) verb) :test #'equal)))
      (if (listp (getf perm noun)) 
          (member (getf params :type) (getf perm noun) :test #'equal)
          t))))

; (check-permission "admin" "create" "node" '(:type "project"))
; (check-permission "anonymous" "create" "node" '(:type "project"))
; (check-permission "anonymous" "create" "node" '(:type "comment"))
; (check-permission "logged-in" "create" "node" '(:type "comment"))
; (check-permission "admin" "delete" "node" '())
; (check-permission "nate" "delete" "pool")
; (check-permission "charlie" "delete" "pool")

(defun permission-denied ()
  (encode-json-plist-to-string '(:status "error" :message "Permission denied")))

(defun check-permissions (verb noun params &rest body)
  (let ((user (hunchentoot:session-value :user)))
    (cond ((not user) 
           (setf user '(:username "anonymous")))
          ((not (equal (getf user :username) "nate"))
           (setf user '(:username "logged-in")))
          (t (setf user '(:username "admin"))))
    (format *logs* "User: ~a~%" user)
    (cond ((or (equal verb "login"))
           (car body)) 
          ((check-permission (getf user :username) verb noun params)
           (car body))
          (t (permission-denied)))))

;;; Server

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

(defun view-node-request (params)
  (let ((limit (or (getf params :limit) -1)))
    (format *logs* "Limit: ~a~%" limit)
    (format *logs* "id ~a~%" (getf params :id))
    (if (not (getf params :id)) ; (not (getf '(:ID 5) :id))
      (nodes->list (view-node :type (getf params :type) :limit limit))
      (nodes->list (view-node :id (getf params :id))))))

(defun post-request (route params)
   (let* ((path-elms (cl-utilities:split-sequence #\/ route :remove-empty-subseqs t))
          (verb (car path-elms))
          (noun (cadr path-elms)))
    (format *logs* "~%POST REQUEST: ~a ~a ~a~%" route params path-elms)
    (check-permissions verb noun params
                      (or 
                        (if (equal verb "login")
                          (login-user params))
                        (if (equal verb "view")
                          (cond ; ((equal noun "node")
                            ;  (nodes->json (view-node-request params)))
                            ((equal noun "node")
                             (let ((node (nodes->json (view-node-request params))))
                               ;; (nodes->json (view-node-request '(:id 2)))
                               (if (not (equal node "[{}]"))
                                 (add-edge-to-json (nodes->json (view-node-request params)) (get-edges->json (getf params :id)))
                                 "")))
                            ((equal noun "edge")
                             (get-edges->json (getf params :id)))))
                        (if (or (equal verb "delete") (equal verb "remove"))
                          (cond ((equal noun "node")
                                 (delete-node (getf params :id)))) 
                          (cond ((equal noun "edge")
                                 (n-delete-edge (getf params :id) (getf params :to) :type (getf params :type) :direction :to))))
                        (if (equal verb "save")
                          (cond ((equal noun "node")
                                 (save-node (getf params :id) params))))
                        (if (equal verb "create")
                          (cond ((equal noun "node")
                                 (progn
                                   (format *logs* "Creating node~%")
                                   (create-node params)))
                                ((equal noun "edge")
                                 (progn
                                   (format *logs* "Creating edge~%")
                                   (create-edge (getf params :id) (getf params :to) :type (getf params :type) :direction :to)))))))))

;;; Server io utilities 

(defun send-json (x)
    (encode-json-alist-to-string x))

(defun post-to-plist (post-data)
  (let ((lst (list)))
  (dolist (i post-data)
    (push (car i) lst)
    (push (cdr i) lst))
  (nreverse lst)))

;;; Application utilities

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

  

