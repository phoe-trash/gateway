;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; test.lisp

(in-package #:gateway)

(begin-tests)


;;;; STANDARD-LIBRARY unit test
(let* ((library (make-instance 'standard-library))
       (maximum #.(expt 2 15))
       (number-1 (random maximum))
       (number-2 (random maximum))
       (number-3 (random maximum)))
  (setf (lookup library '(a a 1)) number-1)
  (assert (= (lookup library '(a a 1)) number-1))
  (setf (lookup library '(a a 1)) number-2)
  (assert (= (lookup library '(a a 1)) number-2))
  (setf (lookup library '(a b 1)) number-3)
  (assert (= (lookup library '(a b 1)) number-3)))






;;;; STANDARD-ACCEPTOR unit test, types: :N :I
(with-clean-config
    (flet ((do-test (type fn)
             (macrolet ((mk () '(make-instance 'standard-connection
                                 :type :client :port port)))
               (let* ((crown (make-instance 'standard-crown))
                      (acceptor (make-instance 'standard-acceptor
                                               :port 0 :owner crown :type type))
                      (port (get-local-port (socket acceptor)))
                      (connection-1 (mk)) (connection-2 (mk)) (connection-3 (mk))
                      (port-1 (get-local-port (socket connection-1)))
                      (port-2 (get-local-port (socket connection-2)))
                      (port-3 (get-local-port (socket connection-3)))
                      (data '(dummy test object)))
                 (send connection-1 data)
                 (send connection-2 data)
                 (send connection-3 data)
                 (labels
                     ((find-conn (port)
                        (find port (funcall fn crown)
                              :key (compose #'get-peer-port #'socket)))
                      (test (port)
                        (loop do (sleep 0.01) until (find-conn port))
                        (let ((received-data (receive (find-conn port))))
                          (assert (data-equal (sexp data) (sexp received-data))))))
                   (mapc #'test (list port-1 port-2 port-3)))
                 (mapc #'kill (list acceptor connection-1
                                    connection-2 connection-3))))))
      (do-test :n #'n-connections)
      (do-test :i #'i-connections)))

;;;; STANDARD-CONNECTION unit test
(with-clean-config
  (labels ((check-conns () (assert (null (maphash #'list *connection-cache*)))))
    (kill (make-instance 'standard-connection :type :listen :port 0))
    (check-conns)
    (macrolet ((mkcn (&body body) `(make-instance 'standard-connection ,@body)))
      (with-connections
          ((listen (mkcn :type :listen :port 0))
           (client (mkcn :type :client :port (get-local-port (socket listen))))
           (accept (mkcn :type :accept :socket (socket listen))))
        (labels ((test (x y data)
                   (send x data)
                   (assert (readyp y))
                   (assert (data-equal data (receive y))))
                 (test-case (data)
                   (test client accept data)
                   (test accept client data)))
          (let ((test-cases '((1 2 3 4 5 6 7 8 9 0)
                              (a b c d e f)
                              (lorem ipsum dolor sit amet)
                              ("a" a "a" a "a" "b"))))
            (mapcar #'test-case test-cases)))))
    (check-conns)))

;;;; STANDARD-CROWN unit test
(let ((crown (make-instance 'standard-crown)))
  (assert (library crown))
  (assert (event-queue crown))
  (assert (null (gems crown)))
  (assert (null (n-acceptor crown)))
  (assert (null (n-connections crown)))
  (assert (n-lock crown))
  (assert (null (n-listener crown)))
  (assert (null (e-connections crown)))
  (assert (e-lock crown))
  (assert (null (e-listener crown)))
  (assert (null (i-acceptor crown)))
  (assert (null (i-connections crown)))
  (assert (i-lock crown))
  (assert (null (i-listener crown)))
  (assert (null (alivep crown))))

;;;; STANDARD-LISTENER unit test, types: :N :E :I
(with-clean-config
    (macrolet ((mk () '(make-instance 'standard-connection :type :client :port port))
               (amk () '(make-instance 'standard-connection :type :accept
                         :socket (socket connection)))
               (dq () '(dequeue queue))
               (do-test (conn-fn lock-fn type)
                 `(let* ((crown (make-instance 'standard-crown))
                         (queue (event-queue crown))
                         (connection (make-instance 'standard-connection
                                                    :port 0 :type :listen))
                         (port (get-local-port (socket connection)))
                         (connection-1 (mk)) (aconnection-1 (amk))
                         (connection-2 (mk)) (aconnection-2 (amk))
                         (connection-3 (mk)) (aconnection-3 (amk))
                         (conns (list connection-1 connection-2 connection-3))
                         (aconns (list aconnection-1 aconnection-2 aconnection-3))
                         (listener (make-instance 'standard-listener
                                                  :owner crown :type ,type))
                         (data '(dummy test object)))
                    (with-lock-held ((,lock-fn crown))
                      (setf (,conn-fn crown) aconns))
                    (send connection-1 (cons 'first data))
                    (send connection-2 (cons 'second data))
                    (send connection-3 (cons 'third data))
                    (loop do (sleep 0.01) until (= 3 (size queue)))
                    (let ((queue-data (mapcar #'caddr
                                              (list (dq) (dq) (dq))))
                          (expected (list (cons 'first data)
                                          (cons 'second data)
                                          (cons 'third data))))
                      (flet ((find-data (x) (find x queue-data :test #'data-equal)))
                        (assert (every #'identity (mapcar #'find-data expected)))))
                    (kill listener)
                    (mapc (lambda (x) (push x (,conn-fn crown))) conns)
                    (mapc #'kill (cons connection conns))
                    (mapc #'kill aconns))))
      (do-test n-connections n-lock :n)
      (do-test e-connections e-lock :e)
      (do-test i-connections i-lock :i)))

;;;; STANDARD-GEM unit test
(with-clean-config
    (macrolet ((mk () '(make-instance 'standard-connection :type :client :port port))
               (amk () '(make-instance 'standard-connection
                         :type :accept :socket (socket connection))))
      (let* ((crown (make-instance 'standard-crown))
             (queue (event-queue crown))
             (connection (make-instance 'standard-connection :port 0 :type :listen))
             (port (get-local-port (socket connection)))
             (connection-1 (mk)) (aconnection-1 (amk))
             (connection-2 (mk)) (aconnection-2 (amk))
             (connection-3 (mk)) (aconnection-3 (amk))
             (gem (make-instance 'standard-gem :owner crown)))
        (with-lock-held ((n-lock crown))
          (push aconnection-1 (n-connections crown)))
        (with-lock-held ((e-lock crown))
          (push aconnection-2 (e-connections crown)))
        (with-lock-held ((i-lock crown))
          (push aconnection-3 (i-connections crown)))
        (kill connection-1)
        (kill connection-2)
        (kill connection-3)
        (receive aconnection-1)
        (receive aconnection-2)
        (receive aconnection-3)
        (loop do (sleep 0.01)
              until (and (empty? queue)
                         (null (n-connections crown))
                         (null (e-connections crown))
                         (null (i-connections crown))))
        (kill gem)
        (kill crown)
        (mapc #'kill (list connection connection-1 connection-2 connection-3
                           aconnection-1 aconnection-2 aconnection-3)))))

;; PING command test
(with-clean-config
  (let*
      ((crown (make-instance 'standard-crown :full t))
       (n-port (get-local-port (socket (n-acceptor crown))))
       (connection (make-instance 'standard-connection :port n-port :type :client))
       (login '(login "username" "password-is-ignored"))
       (ping '(#:ping (#:test #:dummy 1234 "abcABC")))
       (pong (cons '#:pong (cdr ping))))
    (send connection login)
    (receive connection)
    (send connection ping)
    (loop do (sleep 0.01) until (= 1 (length (e-connections crown))))
    (assert (data-equal (receive connection) pong))
    (kill connection)
    (kill crown)))

;; LOGIN command test
(with-clean-config
  (let*
      ((crown (make-instance 'standard-crown :full t))
       (library (library crown))
       (n-port (get-local-port (socket (n-acceptor crown))))
       (connection-1 (make-instance 'standard-connection :port n-port :type :client))
       (connection-2 (make-instance 'standard-connection :port n-port :type :client))
       (username "test-user")
       (data-1 `(login ,username "password-is-ignored")))
    (send connection-1 data-1)
    (loop do (sleep 0.01) until (= 1 (length (e-connections crown))))
    (assert (data-equal (receive connection-1) `(ok ,data-1)))
    (assert (equal (car (e-connections crown)) (lookup library `(auth ,username))))
    (send connection-2 data-1)
    (assert (data-equal (receive connection-2) `(error :username-taken ,username)))
    (mapcar #'kill (list connection-1 connection-2))
    (kill crown)))

;; LOGOUT command test
(with-clean-config
  (let*
      ((crown (make-instance 'standard-crown :full t))
       (n-port (get-local-port (socket (n-acceptor crown))))
       (connection-1 (make-instance 'standard-connection :port n-port :type :client))
       (username "test-user")
       (data-1 `(login ,username "password-is-ignored"))
       (data-2 '(logout)))
    (send connection-1 data-1) (receive connection-1)
    (send connection-1 data-2)
    (assert (data-equal (receive connection-1) `(ok ,data-2)))
    (receive connection-1) (assert (not (alivep connection-1)))
    (mapcar #'kill (list connection-1))
    (kill crown)))

;; EMIT command test
(with-clean-config
  (let*
      ((crown (make-instance 'standard-crown :full t))
       (n-port (get-local-port (socket (n-acceptor crown))))
       (connection-1 (make-instance 'standard-connection :port n-port :type :client))
       (connection-2 (make-instance 'standard-connection :port n-port :type :client))
       (connection-3 (make-instance 'standard-connection :port n-port :type :client))
       (data-1 `(login "test-user1" "password-is-ignored"))
       (data-2 `(login "test-user2" "password-is-ignored"))
       (data-3 '(emit "test-message")))
    (send connection-1 data-1) (receive connection-1)
    (send connection-2 data-2) (receive connection-2)
    (loop do (sleep 0.01) until (= 2 (length (e-connections crown))))
    (send connection-1 data-3)
    (assert (data-equal (receive connection-1) `(ok ,data-3)))
    (assert (data-equal (receive connection-1) `(emit "test-user1" "test-message")))
    (assert (data-equal (receive connection-2) `(emit "test-user1" "test-message")))
    (assert (not (readyp connection-3)))
    (mapcar #'kill (list connection-1 connection-2 connection-3))
    (kill crown)))

;; TODO rewrite all locks and incorporate them into custom accessors
;; TODO implement passwords
;; TODO implement dates
;; TODO ONLINE test
;; TODO implement sane logging all around
;; TODO implement logging into files
;; TODO kill parse rules based on special variables and create a parser class instead

(finish-tests)
