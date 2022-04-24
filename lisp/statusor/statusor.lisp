(defpackage statusor
  (:use :cl)
  (:export
   #:if-let-ok
   #:make-status
   #:handle-status
   #:error-to-signal
   #:error-to-status
   #:nil-to-error
   #:->?))

(in-package #:statusor)

(defmacro if-let-ok (on-error-spec bindings &rest on-success)
  "Let macro similar to if-let, but short-circuits on status or signalled errors

  instead of nils.

  If any of the let bindings in BINDINGS either signal an error or evaluate to an
  (error) status, execution is aborted and ON-ERROR-SPEC is invoked.
  Otherwise, the ON-SUCCESS body is invoked.

  ON-ERROR-SPEC has the form (STATUS-ERROR-VAR BODY) and is defined in
  handle-status."

  (if (null bindings)
      `(progn ,@on-success)
      (destructuring-bind ((val-var val-or-err) . rest) bindings
        `(handle-status
          (error-to-status ,val-or-err)
          (,val-var (if-let-ok ,on-error-spec ,rest ,@on-success))
          ,on-error-spec))))

(defmacro make-status (error-message)
  "Make a status error with the given ERROR-MESSAGE."
  `(values nil ,error-message))

(defmacro handle-status (form
                         &optional
                           on-success-spec
                           on-error-spec)
  "Handle status error in FORM.

   ON-SUCCESS-SPEC has the form (VALUE-VAR BODY),
   where VALUE-VAR is bound to the non-error value of FORM,
   which is accessible within the result BODY.

   ON-ERROR-SPEC has the form (ERROR-VAR ON-ERROR), where
   ERROR-VAR is bound to FORM's status error, and is available within the
   ON-ERROR result body."
  (destructuring-bind (val-var on-success)
      (or on-success-spec
          (let ((val-sym (gensym "VAL-VAR-")))
            `(,val-sym ,val-sym)))
    (destructuring-bind (err-var on-error)
        (or on-error-spec
            (let ((err-sym (gensym "ERR-VAR-")))
              `(,err-sym (make-status ,err-sym))))
      `(multiple-value-bind (,val-var ,err-var) ,form
         (if (null ,err-var) ,on-success
             ,on-error)))))

(defmacro status-to-error (form)
  "If FORM results in a status error, a corresponding signal error is raised."
  `(handle-status
    ,form
    nil ;; use default on-success, i.e. x => x
    (err
     (error "error on ~A:~% ~A" ',form err))))

(defmacro error-to-status (form)
  "Catches any signal errors in FORM and converts them a status error."
  `(handler-case ,form
     (error (err-signal)
       (make-status err-signal))))

(defmacro nil-to-error (form &optional error-message)
  "If FORM evaluates to nil, returns a status error."
  `(or ,form
       (make-status
        ,(or error-message (format nil "~A is nil" form)))))

(defmacro ->? (forms &optional on-error-spec)
  "A threading macro that short-circuits on error."
  (let ((val-var (gensym "val-var-")))
    (if (null (cadr forms))
        `(handle-status ,(car forms)
                       (,val-var ,val-var)
                       ,on-error-spec)
        (destructuring-bind (first second . rest) forms
          `(handle-status
            (error-to-status (,(car second) ,first ,@(cdr second)))
            (,val-var (->? ,val-var ,@rest))
            ,on-error-spec)))))
