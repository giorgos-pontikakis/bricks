(in-package :cl-user)

(defpackage :bricks
  (:use :common-lisp :alexandria :cl-who :lisputils)
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
   :widget
   :id
   :css-class
   :display
   :indent-mode
   :doctype
   ;; --------------------
   ;; utilities
   ;; --------------------
   :css
   :js
   ;; --------------------
   ;; forms
   ;; --------------------
   ;;
   :with-form
   ;; --- classes ---
   :form
   :form-element
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
   :submit-page
   :action
   :hidden
   :body
   :password
   :checked
   :value-label-alist
   :selected
   :readonly
   :body
   :name
   :value
   :disabled
   :kind
   ;; --------------------
   ;; variations
   ;; --------------------
   :textbox
   ;; --------------------
   ;; collection
   ;; --------------------
   ;;
   ;; --- classes ---
   :collection
   :crud-collection-mixin
   :tree
   :table
   :item
   :crud-item-mixin
   :node
   :row
   :record/obj-mixin
   :record/plist-mixin
   :crud-tree
   :crud-node
   :crud-table
   :crud-row
   :paginator
   ;; :table/obj
   ;; :table/plist
   ;; :tree/obj
   ;; :tree/plist
   ;; :crud-table/obj
   ;; :crud-table/plist
   ;; :crud-tree/obj
   ;; :crud-tree/plist
   :multistate-anchor
   :ok-button
   :cancel-button
   ;;
   ;; --- generics ---
   :get-records
   :get-items
   :insert-item
   :update-item
   :create-record
   :update-record
   :find-node
   :selected-p
   :enabled-p
   :controls-p
   :selector
   :payload
   :controls
   :page-start
   :previous-page-start
   :next-page-start
   :target-url
   ;;
   ;; --- slots ---
   :op
   :selected-key
   :filter
   :item-class
   :records
   :get-key
   :get-parent-key
   :root
   :root-key
   :root-parent-key
   :header-labels
   :paginator
   :rows
   :collection
   :record
   :record-class
   :key
   :parent-key
   :children
   :index
   :table
   :start-index
   :delta
   :state
   :href
   :format-fn
   :disabled
   :css-disabled
   ;; --------------------
   ;; misc
   ;; --------------------
   ;;
   ;; --- classes ---
   :navbar
   :menu
   :messenger
   ;;
   ;; --- slots ---
   :spec
   ))
