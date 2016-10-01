(in-package :cl-user)
(defpackage cl-libworld.build
  (:use :cl :cffi)
  (:export :build-shared-libraries))
(in-package :cl-libworld.build)

(defun library-exists-p (name)
  (uiop:file-exists-p
   (merge-pathnames (concatenate 'string "lib/World/build/" name ".so")
                    (asdf:system-source-directory :cl-libworld))))

(defun build-shared-libraries (&key force-rebuild-when-exists-p)
  (let ((orig-dir (uiop:getcwd)))
    (uiop:chdir (merge-pathnames "lib/World/" (asdf:system-source-directory :cl-libworld)))
    (when (or (not (library-exists-p "audioio"))
              force-rebuild-when-exists-p)
      (lake:lake :target "audioio.so"))
    (when (or (not (library-exists-p "libworld"))
              force-rebuild-when-exists-p)
      (lake:lake :target "libworld.so"))
    (uiop:chdir orig-dir)))

;; (defun clean ()
;;   (let ((orig-dir (uiop:getcwd)))
;;     (uiop:chdir (merge-pathnames "lib/World/" (asdf:system-source-directory :cl-libworld)))
;;     (lake:lake :target "clean")
;;     (uiop:chdir orig-dir)))

;;; run build
(build-shared-libraries)
