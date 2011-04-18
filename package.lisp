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
   ;; core
   :with-html
   :defhtml
   :html
   :with-document
   :widget
   :display
   ;; paths
   :url
   :url*
   :path
   :url->path
   :path->url
   :path->url*
   ;; utilities
   :see-other
   :css
   :js
   ;; --------------------
   ;; widgets
   ;; --------------------
   ;;
   ;; forms
   ;;
   :form
   :with-form
   :lazy-input-text
   :input-text
   :radio
   :lazy-radio
   :checkbox
   :lazy-checkbox
   :dropdown
   :input-radio-set
   :lazy-input-radio-set
   :input-checkbox-set
   :lazy-input-checkbox-set
   :dropdown
   :lazy-dropdown
   :button
   :lazy-button
   :submit
   :lazy-submit
   :label
   ;;
   ;; variations
   ;;
   :lazy-textbox
   :textbox
   ;;
   ;; collection
   ;;
   :collection
   :read-records
   :read-items
   :insert-item
   :update-item
   ;;
   :tree
   :table
   :header-labels
   :paginator
   :html-prev
   :html-next
   :html-prev-inactive
   :html-next-inactive
   :rows
   ;;
   :item
   :record
   :key
   ;;
   :node
   :parent-key
   :children
   :find-node
   ;;
   :row
   :index
   :cells
   ;;
   :crud-tree
   :crud-table
   :crud-node
   :crud-row
   ;;
   :delta
   :urlfn
   :page-start
   ;;
   :dropdown-cell
   :name
   :selected
   :alist
   ;;
   :textbox-cell
   :value
   ;;
   :selector
   :states
   :html-on
   :html-off
   ;;
   :radio-cell
   :content
   ;;
   :ok-cell
   :cancel-cell
   ;;
   ;; misc
   ;;
   :navbar
   :horizontal-navbar
   :vertical-navbar
   ;;
   :menu
   ;;
   :messenger
   :messages
   ))
