;;; ~/dotfiles/environment/package.el -*- lexical-binding: t; -*-



(defun is-executable (f)
  "Is the execute bit flipped for any of the modes"

  (defun modes-list (n)
    "Split permission number N to (r w x)."
    (let* ((r (/ n 64))
           (w (/ (- n (* r 64)) 8))
           (x (- n (* r 64) (* w 8))))
       (list r w x)))

  (oddp (car (last (modes-list (file-modes f))))))

(cl-defgeneric is-installed (package))
(cl-defgeneric install (package))

(cl-defstruct Package
  binary
  installation-location
  url
  requires-sudo nil)

(cl-defmethod is-installed ((p Package))
  "Test that file both exists, and is executable"
  (let ((path (split-string
               (getenv "PATH")
               ":")))
    (seq-filter (lambda (directory)
                  (let ((full-path (concat (file-name-as-directory directory) p-binary)))
                    (and (file-exists-p full-path)
                         (is-executable full-path)))))))


(cl-defmethod install (()))

(if '() (message "wrong for me"))

(cl-defgeneric f (y))

(cl-defstruct X
  a nil)

(cl-defmethod f ((x X) y)
  (concat (X-a x) "-" y))

(let ((b (make-X :a "hello")))
  (f b "there"))
