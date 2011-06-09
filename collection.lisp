(in-package :bricks)



;;; ------------------------------------------------------------
;;; CRUD Collections
;;; ------------------------------------------------------------

(defclass collection (widget)
  ((op         :accessor op         :initarg :op)
   (filter     :accessor filter     :initarg :filter)
   (item-class :accessor item-class :initarg :item-class)))

(defclass crud-collection-mixin ()
  ())

(defgeneric read-records (collection)
  (:documentation "Retrieve the raw records for the collection"))

(defgeneric read-items (collection)
  (:documentation "User read-records to return the items of the collection"))

(defgeneric insert-item (collection &key)
  (:documentation "Insert an new item to the collection."))

(defgeneric update-item (collection &key)
  (:documentation "Update an item of the collection"))

(defclass tree (collection)
  ((root-parent-key :accessor root-parent-key :initarg :root-parent-key)
   (root            :accessor root            :initarg :root))
  (:default-initargs :filter nil :item-class 'node))

(defclass table (collection)
  ((header-labels :accessor header-labels :initarg :header-labels)
   (start-index   :accessor start-index   :initarg :start-index)
   (paginator     :accessor paginator)
   (rows          :accessor rows))
  (:default-initargs :filter nil :item-class 'row :start-index 0))



;;; ------------------------------------------------------------
;;; Collection Items
;;; ------------------------------------------------------------

(defclass item ()
  ((collection :accessor collection :initarg :collection)
   (record     :accessor record     :initarg :record)))

(defclass crud-item-mixin ()
  ())

(defgeneric key (crud-item)
  (:documentation "Given an crud-item instance, return the key of its
  record. If it is a new item (not existing in the database), (key
  item) must return nil."))

(defgeneric parent-key (crud-item)
  (:documentation "Given an crud-item instance, return the key of its record."))

(defclass node (item)
  ((parent   :accessor parent   :initarg :parent)
   (children :accessor children :initform nil))
  (:default-initargs :parent nil))

(defgeneric find-node (root-node key))

(defmethod find-node ((root-node node) key)
  (find-node-rec key (list root-node)))

(defun find-node-rec (target-key fringe)
  (let ((node (first fringe)))
    (cond
      ;; fringe exhausted, target not found
      ((null node)
       nil)
      ;; target found
      ((equal (key node) target-key)
       node)
      ;; expand fringe and continue (depth-first search)
      (t
       (find-node-rec target-key
                      (append (children node) (rest fringe)))))))

(defclass row (item)
  ((index :accessor index :initarg :index)))



;;; ------------------------------------------------------------
;;; CRUD generic functions
;;;
;;; Should be defined only for classes inheriting from the
;;; crud-item-mixin.
;;; ------------------------------------------------------------

(defgeneric selected-p (crud-item selected-id)
  (:documentation "Returns T if the item is selected."))

(defgeneric enabled-p (crud-item selected-id)
  (:documentation "Returns T if the item is enabled."))

(defgeneric controls-p (crud-item selected-id)
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

(defmethod selected-p ((item crud-item-mixin) selected-id)
  (equal (key item) selected-id))

(defmethod enabled-p ((item crud-item-mixin) selected-id)
  (and (controls-p item selected-id)
       (member (op (collection item)) '(:create :update))))



;;; ------------------------------------------------------------
;;; CRUD TREE
;;; ------------------------------------------------------------

(defclass crud-tree (tree crud-collection-mixin)
  ())

(defmethod initialize-instance :after ((tree crud-tree) &key)
  (setf (slot-value tree 'root)
        (read-items tree)))

(defmethod read-items ((tree crud-tree))
  (let* ((nodes (mapcar (lambda (rec)
                          (make-instance (item-class tree)
                                         :collection tree
                                         :record rec))
                        (read-records tree)))
         (root-node (find-if (lambda (node)
                               (equal (root-parent-key tree) (parent-key node)))
                             nodes)))
    (iter (for pivot in nodes)
          (iter (for n in nodes)
                (when (and (not (eq pivot n))
                           (equal (parent-key pivot) (key n)))
                  (setf (parent pivot) n)
                  (push pivot (children n)))))
    root-node))

(defmethod insert-item ((tree crud-tree) &key record key)
  (let* ((parent (find-node (root tree) key))
         (new-node (make-instance (item-class tree)
                                  :record record
                                  :collection tree
                                  :parent parent)))
    (push new-node (children parent))))

(defmethod display ((tree crud-tree) &key selected-id selected-data hide-root-p)
  ;; If we get called with no selected id and update/delete op, do not
  ;; even try - the caller is in error, signal it.
  (when (and (null selected-id)
             (member (op tree) '(:update :delete)))
    (with-html
      (error "Error: Cannot execute op ~A with nothing selected" (op tree))))
  ;; If root is hidden and nothing is selected, we want to insert-item
  ;; directly under the root, so we provide .
  (when (and (eql (op tree) :create)
             (or selected-id
                 (and hide-root-p
                      (null selected-id))))
    (insert-item tree
                 :record selected-data
                 :key (or selected-id
                          (key (root tree)))))
  ;; Update
  (when (eql (op tree) :update)
    (update-item tree
                 :data selected-data
                 :key selected-id))
  (with-html
    (:ul :id (id tree) :class (css-class tree)
         (display (if hide-root-p
                      (children (root tree))
                      (root tree))
                  :selected-id (if (and (null selected-id)
                                        (eql (op tree) :create))
                                   (key (root tree))
                                   selected-id)
                  :selected-data selected-data))))



;;; ------------------------------------------------------------
;;; CRUD NODE
;;; ------------------------------------------------------------

(defclass crud-node (node crud-item-mixin)
  ((css-delete   :reader css-delete   :initarg :css-delete)
   (css-selected :reader css-selected :initarg :css-selected)
   (css-selector :reader css-selector :initarg :css-selector)
   (css-payload  :reader css-payload  :initarg :css-payload)
   (css-controls :reader css-controls :initarg :css-controls)
   (css-indent   :reader css-indent   :initarg :css-indent)))

(defmethod controls-p ((node crud-node) selected-id)
  (let ((parent-item (parent node)))
    (or
     ;; update or delete
     (and (member (op (collection node)) '(:update :delete))
          (selected-p node selected-id))
     ;; create
     (and (eq (op (collection node)) :create)
          (and (not (null parent-item)) ;; avoid root object
               (selected-p parent-item selected-id)
               (null (key node)))))))

(defmethod display ((node crud-node) &key selected-id selected-data)
  (let ((controls-p (controls-p node selected-id))
        (selected-p (selected-p node selected-id))
        (enabled-p (enabled-p node selected-id)))
    (with-html
      (:li :class (if selected-p
                      (if (eq (op (collection node)) :delete)
                          (css-delete node)
                          (css-selected node))
                      nil)
           (:span :class (css-selector node)
                  (display (selector node selected-p)))
           (mapc (lambda (cell)
                   (htm (:span :class (css-payload node)
                               (display cell))))
                 (ensure-list (payload node enabled-p)))
           (mapc (lambda (cell)
                   (htm (:span :class (css-controls node)
                               (display cell))))
                 (ensure-list (controls node controls-p)))

           ;; Continue with children
           (when (children node)
             (htm (:ul :class (css-indent node)
                       (mapc (lambda (node)
                               (display node
                                        :selected-id selected-id
                                        :selected-data selected-data))
                             (children node)))))))))



;;; ------------------------------------------------------------
;;; TABLES
;;; ------------------------------------------------------------

(defclass crud-table (table crud-collection-mixin)
  ())

(defmethod initialize-instance :after ((table crud-table) &key)
  (setf (slot-value table 'rows)
        (read-items table))
  (when-let (pg (paginator table))
    (setf (slot-value pg 'table)
          table)))

(defmethod read-items ((table crud-table))
  (iter (for rec in (read-records table))
        (for i from 0)
        (collect (make-instance (item-class table)
                                :record rec
                                :collection table
                                :index i))))

(defmethod insert-item ((table crud-table) &key record index)
  (let* ((rows (rows table))
         (new-row (make-instance (item-class table)
                                 :record record
                                 :collection table
                                 :index index)))
    (setf (rows table)
          (ninsert-list index new-row rows))))

(defmethod display ((table crud-table) &key selected-id selected-data)
  ;; If we get called with no selected id and update/delete op, do not
  ;; even try - the caller is in error, signal it.
  (when (and (null selected-id)
             (member (op table) '(:update :delete)))
    (with-html
      (error "Error: Cannot execute op ~A with nothing selected" (op table))))
  ;; Take care of create/update entries and display the table
  (let ((selected-row (find selected-id (rows table)
                            :key #'key
                            :test #'equal)))
    (let ((index (if selected-row (index selected-row) nil))
          (pg (paginator table)))
      ;; Create
      (when (eq (op table) :create)
        (insert-item table
                     :record selected-data
                     :index 0))
      ;; Update
      (when (eq (op table) :update)
        (update-item table
                     :data selected-data
                     :index index))
      ;; Finally display paginator and table
      (let* ((page-start (page-start pg index (start-index table)))
             (page-end (if pg
                           (min (+ page-start (delta pg))
                                (length (rows table)))
                           (length (rows table)))))
        (with-html
          (when pg
            (display pg :start page-start))
          (:table :id (id table) :class (css-class table)
                  (:thead (:tr (mapc (lambda (i)
                                       (htm (:th (str i))))
                                     (header-labels table))))
                  (:tbody
                   (iter (for row in (subseq (rows table) page-start page-end))
                         (display row
                                  :selected-id selected-id)))))))))



;;; ------------------------------------------------------------
;;; CRUD ROW
;;; ------------------------------------------------------------

(defclass crud-row (row crud-item-mixin)
  ((css-delete   :reader css-delete   :initarg :css-delete)
   (css-selected :reader css-selected :initarg :css-selected)
   (css-selector :reader css-selector :initarg :css-selector)
   (css-payload  :reader css-payload  :initarg :css-payload)
   (css-controls :reader css-controls :initarg :css-controls)))

(defmethod controls-p ((row crud-row) selected-id)
  (and (selected-p row selected-id)
       (member (op (collection row)) '(:create :update :delete))))

(defmethod display ((row crud-row) &key selected-id)
  (let ((selected-p (selected-p row selected-id))
        (controls-p (controls-p row selected-id))
        (enabled-p (enabled-p row selected-id)))
    (with-html
      (:tr :class (if selected-p
                      (if (eq (op (collection row)) :delete)
                          (css-delete row)
                          (css-selected row))
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
  ((table              :accessor table              :initarg :table)
   (start-index        :accessor start-index        :initarg :start-ndex)
   (delta              :accessor delta              :initarg :delta)
   (urlfn              :accessor urlfn              :initarg :urlfn)
   (body-prev          :accessor body-prev          :initarg :body-prev)
   (body-next          :accessor body-next          :initarg :body-next)
   (body-prev-inactive :accessor body-prev-inactive :initarg :body-prev-inactive)
   (body-next-inactive :accessor body-next-inactive :initarg :body-next-inactive)))


;;; start

(defgeneric page-start (paginator index start))

(defmethod page-start ((pg (eql nil)) index start)
  "If there is no paginator, we start displaying table rows from row zero"
  (declare (ignore index start))
  0)

(defmethod page-start ((pg paginator) index start)
  (if (null index)
      (if (or (null start)
              (< start 0)
              (> start (length (rows (table pg)))))
          0
          start)
      (let ((delta (delta pg)))
        (* (floor (/ index delta))
           delta))))


;;;  previous start

(defgeneric previous-page-start (paginator start)
  ())

(defmethod previous-page-start ((pg paginator) start)
  (let ((delta (delta pg)))
    (if (>= (- start delta) 0)
        (- start delta)
        (if (> start 0)
            0
            nil))))


;;; next start

(defgeneric next-page-start (paginator start)
  ())

(defmethod next-page-start ((pg paginator) start)
  (let ((delta (delta pg))
        (len (length (rows (table pg)))))
    (if (<= (+ start delta) (1- len))
        (+ start delta)
        nil)))


;;; generic display

(defmethod display ((pg paginator) &key (start 0))
  (let* ((delta (delta pg))
         (len (length (rows (table pg))))
         (prev (previous-page-start pg start))
         (next (next-page-start pg start)))
    (with-html
      (:div :id (id pg) :class (css-class pg)
            (fmt "Εγγραφές ~A–~A από ~A"
                 (1+ start)
                 (min (+ start delta) len)
                 len)
            (if prev
                (htm (:a :href (apply (urlfn pg) :start prev (filter (table pg)))
                         (display (body-prev pg))))
                (display (body-prev-inactive pg)))
            (if next
                (htm (:a :href (apply (urlfn pg) :start next (filter (table pg)))
                         (display (body-next pg))))
                (display (body-next-inactive pg)))))))
