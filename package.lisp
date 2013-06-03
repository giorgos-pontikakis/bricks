(in-package :cl-user)

(defpackage :bricks
  (:use :closer-common-lisp :alexandria :cl-who :lisputils)
  (:export
   ;; --------------------
   ;; core
   ;; --------------------
   :lisp->html
   :+html-true+
   :+html-false+
   :+html-null+
   :with-html
   :defhtml
   :html
   :with-document
   :indent-mode
   :doctype
   ;; --------------------
   ;; widgets
   ;; --------------------
   :widget
   :id
   :css-class
   :display
   ;; --------------------
   ;; forms
   ;; --------------------
   ;;
   ;; --- classes ---
   :crud-form
   :get-record
   :form
   :form-element
   :with-form
   :input-text
   :input-radio
   :input-checkbox
   :input-radio-set
   :input-checkbox-set
   :dropdown
   :button
   :submit
   :label
   ;;
   ;; --- slots ---
   :action
   :reqtype
   :hidden
   :body
   :disabled
   :readonly
   :name
   :value
   :password
   :checked
   :kind
   :value-label-alist
   :selected
   ;; --------------------
   ;; collection
   ;; --------------------
   ;;
   ;; --- classes ---
   :records-mixin
   :collection
   :crud-collection-mixin
   :tree
   :table
   :item
   :crud-item-mixin
   :node
   :row
   :crud-tree
   :crud-node
   :crud-table
   :crud-row
   :paginator
   :multistate-anchor
   :ok-button
   :cancel-button
   ;;
   ;; --- generics ---
   :get-key
   :get-parent-key
   :update-record
   :make-record
   :find-record
   :get-records
   :get-items
   :create-item
   :update-item
   :find-item
   :key
   :parent-key
   :selected-p
   :enabled-p
   :controls-p
   :selector
   :payload
   :controls
   :target-url
   :page-start
   :previous-page-start
   :next-page-start
   ;;
   ;; --- slots ---
   :records
   :record-class
   :item-class
   :collection
   :record
   :root
   :root-key
   :root-parent-key
   :parent
   :children
   :header-labels
   :start-index
   :create-pos
   :paginator
   :rows
   :index
   :op
   :selected-key
   :css-selected
   :css-selector
   :css-payload
   :css-controls
   :css-indent
   :table
   :delta
   ;; --------------------
   ;; misc
   ;; --------------------
   ;;
   ;; --- classes ---
   :multistate-anchor
   :navbar
   :menu
   ;;
   ;; --- slots ---
   :href
   :body
   :state
   :spec
   :test
   :active
   :textbox
   :format-fn
   ))
