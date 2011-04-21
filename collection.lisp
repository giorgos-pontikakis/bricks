(in-package :bricks)



;;; ------------------------------------------------------------
;;; CRUD Collections
;;; ------------------------------------------------------------

(defclass collection (widget)
  ((op             :accessor op             :initarg :op)
   (filter         :accessor filter         :initarg :filter)
   (item-class     :accessor item-class     :initarg :item-class)
   (item-key-field :accessor item-key-field :initarg :item-key-field)))

(defclass crud-collection-mixin ()
  ())

(defgeneric read-records (collection)
  (:documentation "Retrieve the raw records for the collection"))

(defgeneric read-items (collection)
  (:documentation "User read-records to return the items of the collection"))

(defgeneric insert-item (collection &key)
  (:documentation "Insert an new item to the collection"))

(defgeneric update-item (collection &key)
  (:documentation "Update an item of the collection"))

(defclass tree (collection)
  ((root-id               :accessor root-id               :initarg :root-id)
   (root                  :accessor root                  :initarg :root)
   (item-parent-key-field :accessor item-parent-key-field :initarg :item-parent-key-field))
  (:default-initargs  :filter nil :item-class 'node :root-id :null))

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
   (record     :accessor record     :initarg :record)
   (key        :accessor key        :initarg :key)))

(defclass crud-item-mixin ()
  ())

(defclass node (item)
  ((parent-key :accessor parent-key :initarg :parent-key)
   (children   :accessor children   :initarg :children)))

(defgeneric find-node (node key))

(defmethod find-node ((root node) key)
  (find-node-rec key (list root)))

(defun find-node-rec (target-key fringe)
  (let ((node (first fringe)))
    (cond
      ;; fringe exhausted, target not found
      ((null node)
       nil)
      ;; target found
      ((equal (key node) target-key)
       node)
      ;; expand fringe and continue (depth-first)
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
       (member (op (collection item)) '(:create :update ))))



;;; ------------------------------------------------------------
;;; CRUD TREE
;;; ------------------------------------------------------------

(defclass crud-tree (tree crud-collection-mixin)
  ())

(defmethod initialize-instance :after ((tree crud-tree) &key)
  (setf (slot-value tree 'root)
        (read-items tree)))

(defmethod read-items ((tree crud-tree))
  (let ((records (read-records tree)))
    (labels ((make-nodes (parent-key)
               (mapcar (lambda (rec)
                         (let ((key (getf rec (item-key-field tree))))
                           (make-instance (item-class tree)
                                          :collection tree
                                          :key key
                                          :record rec
                                          :parent-key parent-key
                                          :children (make-nodes key))))
                       (remove-if-not (lambda (rec)
                                        (equal parent-key (getf rec (item-parent-key-field tree))))
                                      records))))
      (make-instance (item-class tree)
                     :collection tree
                     :key (root-id tree)
                     :record (find-if (lambda (rec)
                                        (equal (root-id tree)
                                               (getf rec (item-key-field tree))))
                                      records)
                     :parent-key nil
                     :children (make-nodes (root-id tree))))))

(defmethod update-item ((tree crud-tree) &key record key)
  (let ((node (find-node (root tree) key)))
    (setf (record node)
          (plist-union record (record node)))))

(defmethod insert-item ((tree crud-tree) &key record parent-key)
  (let ((parent-node (find-node (root tree) parent-key))
        (new-node (make-instance (item-class tree)
                                 :key parent-key
                                 :record record
                                 :collection tree
                                 :parent-key parent-key
                                 :children ())))
    (push new-node (children parent-node))))

(defmethod display ((tree crud-tree) &key selected-id selected-data)
  (when (and (eq (op tree) :create)
             (null selected-id))
    (insert-item tree
                 :record selected-data
                 :parent-key nil))
  (with-html
    (:ul :id (id tree) :class (style tree)
         (mapc (lambda (node)
                 (display node
                          :selected-id selected-id
                          :selected-data selected-data))
               (children (root tree))))))



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
  (let ((parent-item (find-node (root (collection node)) (parent-key node))))
    (or
     ;; update or delete
     (and (member (op (collection node)) '(:update :delete))
          (selected-p node selected-id))
     ;; create
     (and (eql (key node) (parent-key node)) ;; this implies create
          (selected-p parent-item selected-id)))))

(defmethod display ((node crud-node) &key selected-id selected-data)
  (let ((controls-p (controls-p node selected-id))
        (selected-p (selected-p node selected-id))
        (enabled-p (enabled-p node selected-id))
        (tree (collection node)))
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
           ;; Create
           (when (and selected-p
                      (eql (op tree) :create))
             (insert-item tree
                          :record selected-data
                          :parent-key selected-id))
           ;; Update
           (when (and selected-p
                      (eql (op tree) :update))
             (update-item tree
                          :record selected-data
                          :key selected-id))
           ;; Continue with children
           (when (children node)
             (htm (:ul :class (css-indent node)
                       ( (lambda (node)
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
                                :key (getf rec (item-key-field table))
                                :record rec
                                :collection table
                                :index i))))

(defmethod update-item ((table crud-table) &key record index)
  (let ((row (nth index (rows table))))
    (setf (record row)
          (plist-union record (record row)))))

(defmethod insert-item ((table crud-table) &key record index)
  (let* ((rows (rows table))
         (new-row (make-instance (item-class table)
                                 :key (getf record :id)
                                 :record record
                                 :collection table
                                 :index index)))
    (setf (rows table)
          (ninsert-list index new-row rows))))

(defmethod display ((table crud-table) &key selected-id selected-data)
  (let ((selected-row (find selected-id (rows table) :key #'key :test #'equal)))
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
                     :record selected-data
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
          (:table :id (id table) :class (style table)
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
   (css-controls :reader css-controls :initarg :css-controls)
   (css-indent   :reader css-indent   :initarg :css-indent)))

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
   (html-prev          :accessor html-prev          :initarg :html-prev)
   (html-next          :accessor html-next          :initarg :html-next)
   (html-prev-inactive :accessor html-prev-inactive :initarg :html-prev-inactive)
   (html-next-inactive :accessor html-next-inactive :initarg :html-next-inactive)))

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

(defmethod display ((pg paginator) &key (start 0))
  (let* ((delta (delta pg))
         (len (length (rows (table pg))))
         (prev (if (>= (- start delta) 0)
                   (- start delta)
                   (if (> start 0)
                       0
                       nil)))
         (next (if (<= (+ start delta) (1- len))
                   (+ start delta)
                   nil)))
    (with-html
      (:div :id (id pg) :class (style pg)
            (fmt "Εγγραφές ~A–~A από ~A"
                 (1+ start)
                 (min (+ start delta) len)
                 len)
            (if prev
                (htm (:a :href (apply (urlfn pg) :start prev (filter (table pg)))
                         (display (html-prev pg))))
                (display (html-prev-inactive pg)))
            (if next
                (htm (:a :href (apply (urlfn pg) :start next (filter (table pg)))
                         (display (html-next pg))))
                (display (html-next-inactive pg)))))))



;;; ------------------------------------------------------------
;;; MULTISTATE ANCHOR
;;; ------------------------------------------------------------

(defclass multistate-anchor (widget)
  ((href    :accessor href    :initarg :href)
   (content :accessor content :initarg :content)
   (state :accessor state :initarg :state)))

(defmethod display ((multistate-anchor multistate-anchor) &key)
  (let ((state (state multistate-anchor)))
    (with-html
      (:a :id (id multistate-anchor)
          :class (style multistate-anchor)
          :href (getf (href multistate-anchor) state)
          (display (getf (content multistate-anchor) state))))))

(defun multistate-anchor (href content &key state)
  (display (make-instance 'multistate-anchor
                          :href href
                          :contant content
                          :state state)))
