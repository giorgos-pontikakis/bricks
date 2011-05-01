(in-package :cl-user)

(defpackage :bricks
  (:use :common-lisp
        :iterate
        :lisputils
        :hunchentoot
        :cl-who
        :cl-ppcre
        :veil
        :alexandria)
  (:export
   ;; --------------------
   ;; core
   ;; --------------------
   :with-html
   :defhtml
   :html
   :with-document
   :widget
   :id
   :css-class
   :css-style
   :display
   ;; --------------------
   ;; paths
   ;; --------------------
   :url
   :url*
   :path
   :url->path
   :path->url
   :path->url*
   ;; --------------------
   ;; utilities
   ;; --------------------
   :see-other
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
   :input-checkbox/radio
   :input-radio
   :input-checkbox
   :input-checkbox/radio-set
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
   :label-value-alist
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
   :read-records
   :read-items
   :insert-item
   :update-item
   :find-node
   :selected-p
   :enabled-p
   :controls-p
   :selector
   :payload
   :controls
   :page-start
   ;;
   ;; --- slots ---
   :op
   :filter
   :item-class
   :root
   :root-key
   :header-labels
   :paginator
   :rows
   :collection
   :record
   :key
   :parent-key
   :children
   :index
   :table
   :start-index
   :delta
   :urlfn
   :body-prev
   :body-next
   :body-prev-inactive
   :body-next-inactive
   :state
   :href
   :disabled
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
   :messages
   :parameters
   ))
