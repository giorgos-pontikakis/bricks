(in-package :bricks)


;;; ------------------------------------------------------------
;;; RECORD mixins
;;; ------------------------------------------------------------

(defclass record-mixin ()
  ((records      :accessor records      :initarg :records)
   (record-class :reader   record-class)))

(defgeneric get-key (record)
  (:documentation "Get the primary key of the record, assuming that it
  belongs to the collection."))

(defgeneric get-parent-key (record)
  (:documentation "Get the primary key of the record, assuming that it
  belongs to the collection."))

(defgeneric merge-record-payload (record payload)
  (:documentation "foo"))

(defmethod merge-record-payload ((record standard-object) payload)
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

(defmethod merge-record-payload ((record list) payload)
  (plist-union payload record))

(defun make-record (record-class &rest params)
  (typecase record-class
    (built-in-class params)
    (t (apply #'make-instance record-class params))))



;;; ------------------------------------------------------------
;;; COLLECTIONS
;;; ------------------------------------------------------------

;;; Superclass

(defclass collection (widget record-mixin)
  ((filter       :accessor filter       :initarg :filter)
   (item-class   :accessor item-class   :initarg :item-class))
  (:default-initargs :filter nil :selected-key nil))

(defgeneric get-records (collection)
  (:documentation "Retrieve the raw records for the collection"))

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
          (merge-record-payload (record item) payload))
    (break)))

(defun ensure-record-consistency (collection)
  ;; Make sure we have the records of the table
  (unless (slot-boundp collection 'records)
    (setf (records collection) (get-records collection)))
  ;; All records of the collection should have the same type. Set the
  ;; record class of the collection with that type.
  (setf (slot-value collection 'record-class)
        (reduce (lambda (x y)
                  (if (eql x y)
                      x
                      (error "All records should be of the same type")))
                (mapcar #'class-of (records collection)))))



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
   (root-parent-key :reader   root-parent-key :initarg :root-parent-key))
  (:default-initargs :item-class 'node))

(defclass node (item)
  ((parent   :accessor parent   :initarg :parent)
   (children :accessor children :initform nil))
  (:default-initargs :parent nil))

(defmethod initialize-instance :after ((tree tree) &key)
  (ensure-record-consistency tree))


(defmethod create-item ((tree tree) payload position)
  (let ((parent (find-item tree position)))
    (push (make-instance (item-class tree)
                         :collection tree
                         :parent parent
                         :record (merge-record-payload (make-record (record-class tree))
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

;; (find-node-rec key (list (root tree)))
;; (defun find-node-rec (target-key fringe)
;;   (let ((node (first fringe)))
;;     (cond
;;       ;; fringe exhausted, target not found
;;       ((null node)
;;        nil)
;;       ;; target found
;;       ((equal (key node) target-key)
;;        node)
;;       ;; expand fringe and continue (depth-first search)
;;       (t
;;        (find-node-rec target-key
;;                       (append (children node) (rest fringe)))))))



;;; ------------------------------------------------------------
;;; TABLES
;;; ------------------------------------------------------------

(defclass table (collection)
  ((header-labels :accessor header-labels :initarg :header-labels)
   (start-index   :accessor start-index   :initarg :start-index)
   (create-pos    :accessor create-pos    :initarg :create-pos)
   (paginator     :accessor paginator)
   (rows          :accessor rows))
  (:default-initargs :item-class 'row :start-index nil :create-pos :first))

(defclass row (item)
  ((index :accessor index :initarg :index)))

(defmethod initialize-instance :after ((table table) &key)
  (ensure-record-consistency table)
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
                                     :record (merge-record-payload (make-record (record-class table))
                                                                   payload))
                      (rows table))))

(defmethod find-item ((table table) position)
  ;; The position argument is the index of the item
  (find position (rows table) :key #'index))



;;; ------------------------------------------------------------
;;; CRUD mixin
;;; ------------------------------------------------------------

(defclass crud-collection-mixin ()
  ((op           :accessor op           :initarg :op)
   (selected-key :accessor selected-key :initarg :selected-key)))

(defmethod initialize-instance :after ((obj crud-collection-mixin) &key)
  ;; If we get called with no selected id and update/delete op, do not
  ;; even try - the caller is in error, signal it.
  (when (and (null (selected-key obj))
             (member (op obj) '(:update :delete)))
    (error "Error: Cannot execute op ~A with nothing selected" (op obj)))
  ;; Only four accepted values for op
  (unless (member (op obj) '(:catalogue :create :update :delete))
    (error "Unknown OP slot value for BRICKS:COLLECTION object of class name: ~A."
           (class-name (class-of obj)))))


(defclass crud-item-mixin ()
  ((css-selected :reader css-selected :initarg :css-selected)
   (css-selector :reader css-selector :initarg :css-selector)
   (css-payload  :reader css-payload  :initarg :css-payload)
   (css-controls :reader css-controls :initarg :css-controls)))

(defgeneric selected-p (crud-item selected-key)
  (:documentation "Returns T if the item is selected."))

(defgeneric enabled-p (crud-item selected-p)
  (:documentation "Returns T if the item is enabled."))

(defgeneric controls-p (crud-item selected-p)
  (:documentation "Returns T if the item has active controls."))

(defgeneric selector (crud-item enabled-p)
  (:documentation "Returns a single widget which is used to select an
  item from the collection."))

(defgeneric payload (crud-item enabled-p)
  (:documentation "Returns a list with the widgets which are the
  payload of the item, i.e. they use the records of the database."))

(defgeneric controls (crud-item enabled-p)
  (:documentation "Returns a list with the widgets which are the
  controls of the item, e.g. ok/cancel buttons."))


;;; Some default methods

(defmethod selected-p ((item crud-item-mixin) selected-key)
  (equal (key item) selected-key))

(defmethod enabled-p ((item crud-item-mixin) selected-p)
  (and (controls-p item selected-p)
       (member (op (collection item)) '(:create :update))))



;;; ------------------------------------------------------------
;;; CRUD TREE
;;; ------------------------------------------------------------

(defclass crud-tree (tree crud-collection-mixin)
  ())

(defmethod get-items ((tree crud-tree))
  (let* ((records (records tree))
         (root-key (root-key tree))
         (root-rec (if root-key
                       (find-if (lambda (rec)
                                  (equal root-key
                                         (get-key rec)))
                                records)
                       (find-if (lambda (rec)
                                  (equal (root-parent-key tree)
                                         (get-parent-key rec)))
                                records))))
    (unless root-rec
      (error "Root record not found"))
    (let ((root-node (make-instance (item-class tree)
                                    :collection tree
                                    :record root-rec)))
      (dft (lambda (parent)
             (let ((children (loop for r in records
                                   when (and (not (eq r parent))
                                             (equalp (get-parent-key r)
                                                     (get-key (record parent))))
                                     collect (make-instance (item-class tree)
                                                            :collection tree
                                                            :record r))))
               (setf (children parent) children)
               children))
           root-node)
      root-node)))

(defmethod display :before ((tree crud-tree) &key payload)
  (declare (ignore payload))
  (setf (root tree) (get-items tree)))

(defmethod display ((tree crud-tree) &key payload hide-root-p)
  (let ((selected-key (selected-key tree)))
    ;; If root is hidden and nothing is selected, we want to
    ;; create-item directly under the root.
    (when (and (eql (op tree) :create)
               (or selected-key
                   (and hide-root-p
                        (null selected-key))))
      (create-item tree
                   payload
                   (or selected-key (key (root tree)))))
    ;; Update
    (when (eql (op tree) :update)
      (update-item tree
                   payload
                   selected-key))
    (with-html
      (:ul :id (id tree) :class (conc (css-class tree) " op-" (string-downcase (op tree)))
        (display (if hide-root-p
                     (children (root tree))
                     (root tree))
                 :selected-key (if (and (null selected-key)
                                        (eql (op tree) :create))
                                   (key (root tree))
                                   selected-key))))))



;;; ------------------------------------------------------------
;;; CRUD NODE
;;; ------------------------------------------------------------

(defclass crud-node (node crud-item-mixin)
  ((css-indent :reader css-indent :initarg :css-indent)))

(defmethod controls-p ((node crud-node) selected-p)
  (let ((parent-item (parent node)))
    (or
     ;; update or delete
     (and (member (op (collection node)) '(:update :delete))
          selected-p)
     ;; create
     (and (eq (op (collection node)) :create)
          (and (not (null parent-item)) ;; avoid root object
               selected-p
               (null (key node)))))))

(defmethod display ((node crud-node) &key selected-key)
  (let* ((selected-p (selected-p node selected-key))
         (controls-p (controls-p node selected-p))
         (enabled-p (enabled-p node selected-p)))
    (with-html
      (:li :class (if selected-p
                      (css-selected node)
                      nil)
        (:div (:span :class (css-selector node)
                (display (selector node selected-p)))
          (mapc (lambda (cell)
                  (htm (:span :class (css-payload node)
                         (display cell))))
                (ensure-list (payload node enabled-p)))
          (mapc (lambda (cell)
                  (htm (:span :class (css-controls node)
                         (display cell))))
                (ensure-list (controls node controls-p))))

        ;; Continue with children
        (when (children node)
          (htm (:ul :class (css-indent node)
                 (mapc (lambda (node)
                         (display node
                                  :selected-key selected-key))
                       (children node)))))))))



;;; ------------------------------------------------------------
;;; CRUD TABLES
;;; ------------------------------------------------------------

(defclass crud-table (table crud-collection-mixin)
  ())

(defmethod get-items ((table crud-table))
  (let* ((pg (paginator table))
         (records (records table))
         (selected-index (position (selected-key table) records
                                   :key #'get-key :test #'equalp))
         (page-start (page-start pg selected-index (start-index table)))
         (page-end (if pg
                       (min (+ page-start (delta pg))
                            (length records))
                       (length records))))
    (loop for rec in (subseq records page-start page-end)
          for i from page-start
          collect (make-instance (item-class table)
                                 :record rec
                                 :collection table
                                 :index i))))

(defmethod display :before ((table crud-table) &key payload)
  (declare (ignore payload))
  (setf (rows table) (get-items table)))

(defmethod display ((table crud-table) &key payload)
  (if (rows table)
      ;; Take care of create/update entries and display the table
      (let* ((selected-key (selected-key table))
             (index (index (first (rows table))))
             (pg (paginator table)))
        ;; Create
        (when (eq (op table) :create)
          (create-item table
                       payload
                       (ecase (create-pos table)
                         (:first 0)
                         (:last (length (rows table))))))
        ;; Update
        (when (eq (op table) :update)
          (update-item table payload index))
        ;; Finally display paginator and table
        (with-html
          (when pg
            (display pg :start index))
          (:table :id (id table) :class (conc (css-class table) " op-" (string-downcase (op table)))
            (when-let (hlabels (header-labels table))
              (htm (:thead (:tr (mapc (lambda (i)
                                        (htm (:th (str i))))
                                      hlabels)))))
            (:tbody
              (loop for row in (rows table)
                    do (display row :selected-key selected-key))))))
      (with-html
        (:h4 "Δεν υπάρχουν εγγραφές"))))



;;; ------------------------------------------------------------
;;; CRUD ROW
;;; ------------------------------------------------------------

(defclass crud-row (row crud-item-mixin)
  ())

(defmethod controls-p ((row crud-row) selected-p)
  (and selected-p
       (member (op (collection row)) '(:create :update :delete))))

(defmethod display ((row crud-row) &key selected-key)
  (let* ((selected-p (selected-p row selected-key))
         (controls-p (controls-p row selected-p))
         (enabled-p (enabled-p row selected-p)))
    (with-html
      (:tr :class (if selected-p
                      (css-selected row)
                      nil)
        (:td :class (css-selector row)
          (display (selector row selected-p)))
        (mapc (lambda (cell)
                (htm (:td :class (css-payload row)
                       (display cell))))
              (ensure-list (payload row enabled-p)))
        (mapc (lambda (cell)
                (htm (:td :class (css-controls row)
                       (display cell))))
              (ensure-list (controls row controls-p)))))))



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
