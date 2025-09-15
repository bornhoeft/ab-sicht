(in-package :ASDF)

(defsystem "AB93"

  :description "Patches for the composition AB93"
  :long-description ""
  :version ""
  :author "Achim Bornhoeft"
  :licence ""
  :maintainer ""

  ;; :serial t means that each component is only compiled, when the
  ;; predecessors are already loaded
  :serial t
  :components
  (
   (:FILE "package")
   (:FILE "AB93")))