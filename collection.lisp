(in-package :bricks)



;;; ------------------------------------------------------------
;;; CRUD Collections
;;; ------------------------------------------------------------

(defclass collection (widget)
  ((op           :accessor op           :initarg :op)
   (filter       :accessor filter       :initarg :filter)
   (item-class   :accessor item-class   :initarg :item-class)
   (record-class :accessor record-class :initarg :record-class)))

(defgeneric read-records (collection)
  (:documentation "Retrieve the raw records for the collection"))

(defgeneric read-items (collection)
  (:documentation "User read-records to return the items of the collection"))

(defgeneric insert-item (collection &key)
  (:documentation "Insert an new item to the collection."))

(defgeneric update-item (collection &key)
  (:documentation "Update an item of the collection"))

(defgeneric find-item (collection position)
  (:documentation "Give a position and return a matching item of the collection"))

(defmethod update-item ((collection collection) &key payload position-id)
  (let ((item (find-item collection position-id)))
    (update-record item payload)))


(defclass tree (collection)
  ((root-parent-key :accessor root-parent-key :initarg :root-parent-key)
   (root            :accessor root            :initarg :root))
  (:default-initargs :filter nil :item-class 'node))

(defmethod insert-item ((tree tree) &key payload position-id)
  (let* ((parent (find-item tree position-id))
         (new-node (make-instance (item-class tree)
                                  :record (new-record (record-class tree) payload)
                                  :collection tree
                                  :parent parent)))
    (push new-node (children parent))))

(defmethod find-item ((tree tree) key)
  ;; The position argument is the key of the item
  (find-node-rec key (list (root tree))))

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


(defclass table (collection)
  ((header-labels :accessor header-labels :initarg :header-labels)
   (start-index   :accessor start-index   :initarg :start-index)
   (paginator     :accessor paginator)
   (rows          :accessor rows))
  (:default-initargs :filter nil :item-class 'row :start-index 0))

(defmethod insert-item ((table table) &key payload position-id)
  (let* ((rows (rows table))
         (new-row (make-instance (item-class table)
                                 :record (new-record (record-class table) payload)
                                 :collection table
                                 :index position-id)))
    (setf (rows table)
          (ninsert-list position-id new-row rows))))

(defmethod find-item ((table table) index)
  ;; The position argument is the index of the item
  (nth index (rows table)))



;;; ------------------------------------------------------------
;;; ITEMS
;;; ------------------------------------------------------------

(defclass item ()
  ((collection :accessor collection :initarg :collection)
   (record     :accessor record     :initarg :record)))

(defgeneric find-item (collection key))

(defclass crud-item-mixin ()
  ((css-delete   :reader css-delete   :initarg :css-delete)
   (css-selected :reader css-selected :initarg :css-selected)
   (css-selector :reader css-selector :initarg :css-selector)
   (css-payload  :reader css-payload  :initarg :css-payload)
   (css-controls :reader css-controls :initarg :css-controls)))

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

(defclass row (item)
  ((index :accessor index :initarg :index)))



;;; ------------------------------------------------------------
;;; RECORD mixins
;;; ------------------------------------------------------------

(defclass record/obj-mixin ()
  ())

(defclass record/plist-mixin ()
  ())

(defun new-record (record-class payload)
  (if (null record-class)
      payload
      (apply #'make-instance record-class payload)))

(defgeneric update-record (item payload))

(defmethod update-record ((item record/obj-mixin) payload)
  (let ((record (record item)))
    (plist-mapc (lambda (key val)
                  (when val
                    (let ((slot-name
                           (if (keywordp key)
                               (find-symbol (symbol-name key)
                                            (symbol-package (class-name (class-of record))))
                               key)))
                      (setf (slot-value record slot-name) val))))
                payload)))

(defmethod update-record ((item record/plist-mixin) payload)
  (setf (record item) (plist-union payload (record item))))



;;; ------------------------------------------------------------
;;; CRUD mixin
;;; ------------------------------------------------------------

(defclass crud-collection-mixin ()
  ())

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

;; (defmethod insert-item ((tree crud-tree) &key record key)
;;   (let* ((parent (find-item tree key))
;;          (new-node (make-instance (item-class tree)
;;                                   :record record
;;                                   :collection tree
;;                                   :parent parent)))
;;     (push new-node (children parent))))

(defmethod display ((tree crud-tree) &key key payload hide-root-p)
  ;; If we get called with no selected id and update/delete op, do not
  ;; even try - the caller is in error, signal it.
  (when (and (null key)
             (member (op tree) '(:update :delete)))
    (error "Error: Cannot execute op ~A with nothing selected" (op tree)))
  ;; If root is hidden and nothing is selected, we want to insert-item
  ;; directly under the root.
  (when (and (eql (op tree) :create)
             (or key
                 (and hide-root-p
                      (null key))))
    (insert-item tree
                 :payload payload
                 :position-id (or key
                                  (key (root tree)))))
  ;; Update
  (when (eql (op tree) :update)
    (update-item tree
                 :payload payload
                 :position-id key))
  (with-html
    (:ul :id (id tree) :class (css-class tree)
         (display (if hide-root-p
                      (children (root tree))
                      (root tree))
                  :selected-key (if (and (null key)
                                         (eql (op tree) :create))
                                    (key (root tree))
                                    key)))))


;;; ... records are objects ....................................

;; (defmethod insert-item ((tree crud-tree/obj) &key payload key)
;;   ;; New item => no id => record = payload
;;   (let* ((parent (find-item tree key))
;;          (new-node (make-instance (item-class tree)
;;                                   :record (new-record tree payload)
;;                                   :collection tree
;;                                   :parent parent)))
;;     (push new-node (children parent))))

;; (defmethod update-item ((tree crud-tree/obj) &key payload key)
;;   (let ((record (record (find-item (root tree) key))))
;;     (update-record/obj record payload)))


;;; ... records are plists .....................................


;; (defmethod insert-item ((tree crud-tree/plist) &key payload key)
;;   ;; New item => no id => record = payload
;;   (let* ((parent (find-item tree key))
;;          (new-node (make-instance (item-class tree)
;;                                   :record (new-record tree payload)
;;                                   :collection tree
;;                                   :parent parent)))
;;     (push new-node (children parent))))

;; (defmethod update-item ((tree crud-tree/plist) &key payload key)
;;   (let ((node (find-item tree key)))
;;     (update-record/plist (record node) payload)))



;;; ------------------------------------------------------------
;;; CRUD NODE
;;; ------------------------------------------------------------

(defclass crud-node (node crud-item-mixin)
  ((css-indent   :reader css-indent   :initarg :css-indent)))

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

(defmethod display ((node crud-node) &key selected-key)
  (let* ((selected-p (selected-p node selected-key))
         (controls-p (controls-p node selected-p))
         (enabled-p (enabled-p node selected-p)))
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
                                        :selected-p selected-p))
                             (children node)))))))))

(defclass crud-node/obj (crud-node record/obj-mixin)
  ())

(defclass crud-node/plist (crud-node record/plist-mixin)
  ())



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

(defmethod display ((table crud-table) &key key payload)
  ;; If we get called with no selected id and update/delete op, do not
  ;; even try - the caller is in error, signal it.
  (when (and (null key)
             (member (op table) '(:update :delete)))
    (error "Error: Cannot execute op ~A with nothing selected" (op table)))
  ;; Take care of create/update entries and display the table
  (let* ((index (if-let (selected (find key (rows table)
                                        :key #'key :test #'equal))
                  (index selected)
                  nil))
         (pg (paginator table)))
    ;; Create
    (when (eq (op table) :create)
      (insert-item table
                   :payload payload
                   :position-id 0))
    ;; Update
    (when (eq (op table) :update)
      (update-item table
                   :payload payload
                   :position-id index))
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
                       (display row :selected-key key))))))))



;;; ... records are objects ....................................

;; (defmethod insert-item ((table crud-table/obj) &key payload index)
;;   ;; new-item => no id => record = payload
;;   (let* ((rows (rows table))
;;          (new-row (make-instance (item-class table)
;;                                  :record (new-record tree payload)
;;                                  :collection table
;;                                  :index index)))
;;     (setf (rows table)
;;           (ninsert-list index new-row rows))))

;; (defmethod update-item ((table crud-table/obj) &key payload index)
;;   ;; We assume that the row's record and payload are both objects
;;   (let* ((record (record (nth index (rows table)))))
;;     (update-record/obj record payload)))



;;; ... records are plists .....................................

;; (defmethod insert-item ((table crud-table/plist) &key payload index)
;;   ;; new-item => no id => record = payload
;;   (let* ((rows (rows table))
;;          (new-row (make-instance (item-class table)
;;                                  :record (new-record tree payload)
;;                                  :collection table
;;                                  :index index)))
;;     (setf (rows table)
;;           (ninsert-list index new-row rows))))

;; (defmethod update-item ((table crud-table/plist) &key payload index)
;;   (let ((row (find-item table key)))
;;     (update-record/plist (record row) payload)))



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

(defclass crud-row/obj (crud-row record/obj-mixin)
  ())

(defclass crud-row/plist (crud-row record/plist-mixin)
  ())



;;; ------------------------------------------------------------
;;; TABLE PAGINATOR
;;; ------------------------------------------------------------

(defclass paginator (widget)
  ((table :accessor table :initarg :table)
   (delta :accessor delta :initarg :delta)))


;;; start

(defgeneric page-start (paginator index start))

(defmethod page-start ((pg (eql nil)) index start)
  "If there is no paginator, we start displaying table rows from row zero"
  (declare (ignore index start))
  0)

(defmethod page-start ((pg paginator) index start)
  (let ((delta (delta pg)))
    (* (floor (/ index delta))
       delta)))

(defmethod page-start ((pg paginator) (index (eql nil)) start)
  (if (or (null start)
          (< start 0)
          (> start (length (rows (table pg)))))
      0
      start))


;;;  previous start

(defgeneric previous-page-start (paginator start)
  (:documentation "Given a paginator and a page starting index, return
  the starting index of the previous page, or nil if we are at the
  first page."))

(defmethod previous-page-start ((pg paginator) start)
  (let ((delta (delta pg)))
    (if (>= (- start delta) 0)
        (- start delta)
        (if (> start 0)
            0
            nil))))


;;; next start

(defgeneric next-page-start (paginator start)
  (:documentation "Given a paginator and a page starting index, return
  the starting index of the next page, or nil if we are at the
  last page."))

(defmethod next-page-start ((pg paginator) start)
  (let ((delta (delta pg))
        (len (length (rows (table pg)))))
    (if (<= (+ start delta) (1- len))
        (+ start delta)
        nil)))

(defgeneric target-url (paginator start)
  (:documentation "Given a paginator and a page start index, return a
  target url for the page index."))
