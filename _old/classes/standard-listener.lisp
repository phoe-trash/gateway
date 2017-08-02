;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-listener.lisp

(in-package #:gateway)

;; (define-test test-standard-listener-dead-connection
;;   (let* ((connections nil) (data nil) (lock (make-lock "STANDARD-LISTENER test"))
;;          (conn-getter (lambda () (with-lock-held (lock) connections)))
;;          (conn-pusher (lambda (x) (with-lock-held (lock) (push x connections))))
;;          (data-pusher (lambda (x y) (with-lock-held (lock) (push (list x y) data)))))
;;     (finalized-let*
;;         ((listener (%make-listener conn-getter conn-pusher data-pusher conn-getter)
;;                    (kill listener) (is (wait () (deadp listener))))
;;          (conns (multiple-value-list (make-connection-pair))
;;                 (mapc #'kill conns) (is (wait () (every #'deadp conns)))))
;;       (with-lock-held (lock)
;;         (push (first conns) connections)
;;         (notify listener)
;;         (is (alivep (first conns)))
;;         (kill (second conns))
;;         (is (wait () (deadp (first conns))))))))

(define-test test-standard-listener
  (let* ((connections nil) (data nil) (lock (make-lock "STANDARD-LISTENER test"))
         (sample-data-1 '(#:foo #:bar #:baz #:quux)) (sample-data-2 '(1 2 3 #:quux))
         (conn-getter (lambda () (with-lock-held (lock) connections)))
         (conn-pusher (lambda (x) (with-lock-held (lock) (push x connections))))
         (data-pusher (lambda (x y) (with-lock-held (lock) (push (list x y) data)))))
    (finalized-let*
        ((listener (%make-listener conn-getter conn-pusher data-pusher conn-getter)
                   (kill listener) (is (wait () (deadp listener))))
         (conns-1 (multiple-value-list (make-connection-pair))
                  (mapc #'kill conns-1) (is (wait () (every #'deadp conns-1))))
         (conns-2 (multiple-value-list (make-connection-pair))
                  (mapc #'kill conns-2) (is (wait () (every #'deadp conns-2)))))
      (with-lock-held (lock)
        (push (first conns-1) connections)
        (push (first conns-2) connections)
        (notify listener)
        (data-send (second conns-1) sample-data-1)
        (data-send (second conns-1) sample-data-2)
        (data-send (second conns-2) sample-data-1))
      (flet ((output-present-p (connection output)
               (wait () (with-lock-held (lock) (member (list connection output) data
                                                       :test #'data-equal)))))
        (is (output-present-p (first conns-1) sample-data-1))
        (is (output-present-p (first conns-1) sample-data-2))
        (is (output-present-p (first conns-2) sample-data-1))))))

;; (define-test test-standard-listener-clean-connections
;;   (with-crown-and-connections crown (connection) ()
;;     (kill connection)
;;     (is (wait () (= 1 (length (n-connections crown)))))))

;; Oh goodness, I remember the days when I've had no idea what a closure was
;; and how a function can be an object.
;; ~phoe, 28 Dec 2016
