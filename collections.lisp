(in-package :bricks)



;;; ------------------------------------------------------------
;;; COLLECTIONS
;;; ------------------------------------------------------------

;;; Superclass

(defclass collection (widget records-mixin)
  ((item-class :accessor item-class :initarg :item-class)))

(defgeneric get-items (collection)
  (:documentation "Uses get-records to return the items of the collection"))

(defgeneric create-item (collection payload position)
  (:documentation "Create a new item and insert it in the collection."))

(defgeneric update-item (collection payload position)
  (:documentation "Update an item of the collection"))

(defgeneric find-item (collection position)
  (:documentation "Give a position and return a matching item of the collection"))


(defmethod update-item ((collection collection) payload position)
  (let ((item (find-item collection position)))
    (setf (record item)
          (update-record (record item) payload))))



;;; ------------------------------------------------------------
;;; ITEMS
;;; ------------------------------------------------------------

(defclass item ()
  ((collection :accessor collection :initarg :collection)
   (record     :accessor record     :initarg :record)))

(defgeneric key (item)
  (:documentation "Given a record of a collection, return the value of
  the field which is its key. If the record belongs to a new item (not
  existing in the database), (key item) must return nil."))

(defmethod key ((item item))
  (get-key (record item)))



;;; ------------------------------------------------------------
;;; TREES
;;; ------------------------------------------------------------

(defclass tree (collection)
  ((root            :accessor root            :initarg :root)
   (root-key        :accessor root-key        :initarg :root-key)
   (root-parent-key :accessor root-parent-key :initarg :root-parent-key))
  (:default-initargs :item-class 'node))

(defclass node (item)
  ((parent   :accessor parent   :initarg :parent)
   (children :accessor children :initarg :children))
  (:default-initargs :parent nil
                     :children nil))

(defmethod create-item ((tree tree) payload position)
  (let ((parent (find-item tree position)))
    (push (make-instance (item-class tree)
                         :collection tree
                         :parent parent
                         :record (update-record (make-record (record-class tree))
                                                payload))
          (children parent))))

(defmethod find-item ((tree tree) key)
  ;; The position argument is the key of the item
  (dfs key #'children (root tree) :test #'equal))

(defgeneric parent-key (node)
  (:documentation "Given a record of a tree, return the value of the
  field which is the parent key. If the record belongs to a new item (not
  existing in the database), (parent-key node) must return nil."))

(defmethod parent-key ((node node))
  (get-parent-key (record node)))



;;; ------------------------------------------------------------
;;; TABLES
;;; ------------------------------------------------------------

(defclass table (collection)
  ((header-labels :accessor header-labels :initarg :header-labels)
   (start-index   :accessor start-index   :initarg :start-index)
   (create-pos    :accessor create-pos    :initarg :create-pos)
   (paginator     :accessor paginator     :initarg :paginator)
   (rows          :accessor rows))
  (:default-initargs :item-class 'row
                     :start-index nil
                     :create-pos :first
                     :paginator nil))

(defclass row (item)
  ((index :accessor index :initarg :index)))

(defmethod initialize-instance :after ((table table) &key)
  ;; If there is a paginator, link it with the table
  (when-let (pg (paginator table))
    (setf (slot-value pg 'table)
          table)))


(defmethod index ((item null))
  nil)

(defmethod create-item ((table table) payload position)
  (setf (rows table)
        (ninsert-list position
                      (make-instance (item-class table)
                                     :collection table
                                     :index position
                                     :record (update-record (make-record (record-class table))
                                                            payload))
                      (rows table))))

(defmethod find-item ((table table) position)
  ;; The position argument is the index of the item
  (find position (rows table) :key #'index))



;;; ------------------------------------------------------------
;;; TABLE PAGINATOR
;;; ------------------------------------------------------------

(defclass paginator (widget)
  ((table :accessor table :initarg :table)
   (delta :accessor delta :initarg :delta)))

(defgeneric target-url (paginator start-index)
  (:documentation "Given a paginator and a page start index, return a
  target url for the page index."))


;;; start

(defgeneric page-start (paginator index start))

(defmethod page-start ((pg null) index start)
  "If there is no paginator, we start displaying table rows from
record zero"
  (declare (ignore index start))
  0)

(defmethod page-start ((pg paginator) selected-index start-index)
  (declare (ignore start-index))
  (let ((delta (delta pg)))
    (* (floor (/ selected-index delta))
       delta)))

(defmethod page-start ((pg paginator) (selected-index null) start-index)
  (declare (ignore selected-index))
  (let* ((delta (delta pg))
         (table (table pg))
         (len (length (records table))))
    (if (or (null start-index)
            (< start-index 0)
            (> start-index len))
        (if (eql (op table) :create)
            (ecase (create-pos table)
              (:first 0)
              (:last (* (floor (/ len delta))
                        delta)))
            0)
        start-index)))


;;;  previous start

(defgeneric previous-page-start (paginator start-index)
  (:documentation "Given a paginator and a page start index, return
  the start index of the previous page, or nil if we are at the
  first page."))

(defmethod previous-page-start ((pg paginator) (start-index number))
  (let ((delta (delta pg)))
    (if (>= (- start-index delta) 0)
        (- start-index delta)
        (if (> start-index 0)
            0
            nil))))

(defmethod previous-page-start ((pg paginator) (start-index null))
  0)


;;; next start

(defgeneric next-page-start (paginator start-index)
  (:documentation "Given a paginator and a page start index, return
  the start index of the next page, or nil if we are at the last
  page."))

(defmethod next-page-start ((pg paginator) (start-index number))
  (let ((delta (delta pg))
        (len (length (records (table pg)))))
    (if (<= (+ start-index delta) (1- len))
        (+ start-index delta)
        nil)))

(defmethod next-page-start ((pg paginator) (start-index null))
  0)
