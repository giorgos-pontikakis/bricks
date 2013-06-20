(in-package :bricks)



;;; ------------------------------------------------------------
;;; CRUD mixin
;;; ------------------------------------------------------------

(defclass crud-collection-mixin ()
  ((op           :accessor op           :initarg :op)
   (selected-key :accessor selected-key :initarg :selected-key))
  (:default-initargs :selected-key nil))

(defmethod initialize-instance :after ((obj crud-collection-mixin) &key)
  ;; If we get called with no selected id and update/delete op, do not
  ;; even try - the caller is in error, signal it.
  (when (and (null (selected-key obj))
             (member (op obj) '(:update :delete)))
    (error "Error: Cannot execute op ~A with nothing selected" (op obj)))
  ;; Only four accepted values for op
  (unless (member (op obj) '(:create :read :update :delete))
    (error "Unknown OP slot value for BRICKS:COLLECTION object of class name: ~A."
           (class-name (class-of obj)))))


(defclass crud-item-mixin ()
  ((css-selected :accessor css-selected :initarg :css-selected)
   (css-selector :accessor css-selector :initarg :css-selector)
   (css-payload  :accessor css-payload  :initarg :css-payload)
   (css-controls :accessor css-controls :initarg :css-controls))
  (:default-initargs :css-selected nil
                     :css-selector nil
                     :css-payload nil
                     :css-controls nil))

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
;;; CRUD FORM
;;; ------------------------------------------------------------

(defclass crud-form (widget)
  ((op           :accessor op           :initarg :op)
   (key          :accessor key          :initarg :key)
   (record       :accessor record       :initarg :record)
   (record-class :accessor record-class :initarg :record-class))
  (:default-initargs :key nil))

(defmethod initialize-instance :after ((form crud-form) &key)
  (when (and (eql (op form) :create)
             (key form))
    (error "Contradiction in crud-form initialization. Slot OP is :create and slot KEY is not null"))
  (unless (slot-boundp form 'record)
    (setf (slot-value form 'record) (if (key form)
                                        (get-record form)
                                        (make-record (record-class form))))))

(defmethod display :before ((form crud-form) &key payload)
  (when (member (op form) '(:create :update))
    (setf (record form) (update-record (record form) payload))))



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

(defmethod display :before ((tree crud-tree) &key)
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
  ((css-indent :accessor css-indent :initarg :css-indent)))

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

(defmethod display :before ((table crud-table) &key)
  (setf (rows table) (get-items table)))

(defmethod display ((table crud-table) &key payload)
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
    (if (rows table)
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
                         do (display row :selected-key selected-key)))))
        (with-html
          nil #|(:h4 "Δεν υπάρχουν εγγραφές")|#))))



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
