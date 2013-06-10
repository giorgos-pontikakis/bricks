(in-package :bricks)



;;; ------------------------------------------------------------
;;; RECORD
;;; The record is either a plist or an instance of a storage class
;;; ------------------------------------------------------------

(defun make-record (record-class &rest params)
  "Return a fresh record, either a plist or an instance of a storage class."
  (case record-class
    (cons params)
    (t (apply #'make-instance record-class params))))

(defgeneric get-record (widget)
  (:documentation "Returs a record of the widget."))

(defgeneric update-record (record payload)
  (:documentation "Update a record of the widget with the payload, which should be a plist."))

(defmethod update-record ((record standard-object) payload)
  (plist-mapc (lambda (key val)
                (let ((slot-name
                        (if (keywordp key)
                            (find-symbol (symbol-name key)
                                         (symbol-package (class-name (class-of record))))
                            key)))
                  (when (slot-exists-p record slot-name)
                    (setf (slot-value record slot-name) val))))
              payload)
  record)

(defmethod update-record ((record list) payload)
  (plist-union payload record))

(defgeneric get-key (record)
  (:documentation "Get the primary key of the record, assuming that it
  belongs to the collection."))

(defgeneric get-parent-key (record)
  (:documentation "Get the primary key of the record, assuming that it
  belongs to the collection."))



;;; ------------------------------------------------------------
;;; RECORDS
;;; ------------------------------------------------------------

(defclass records-mixin ()
  ((records      :accessor records      :initarg :records)
   (record-class :accessor record-class :initarg :record-class)))

(defgeneric get-records (collection)
  (:documentation "Retrieve the raw records for the collection"))

(defgeneric find-record (records-mixin key)
  (:documentation "Find a record of the record set that has the given key value"))

(defmethod find-record ((obj records-mixin) key)
  (find key (records obj) :key #'get-key))
