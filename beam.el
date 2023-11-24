;;; -*- lexical-binding: t; -*-
;;; beam.el --- Minimal projectile replacement

;; Copyright (C) 2019-2020  Ryan Pavlik

;; Author: Ryan Pavlik <rpavlik@gmail.com>
;; URL: https://github.com/rpav/beam.el
;; Version: 1.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl-lib)

(defgroup beam ()
  "Beam, a laser-focused projectile alternative."
  :group 'tools)

(defcustom beam-local-projects-file
  (expand-file-name "beam-projects.el" user-emacs-directory)
  "Path to store project directories."
  :type 'string
  :group 'beam)

(defcustom beam-list-files-command "git ls-files --full-name"
  "Command to get a list of files for project"
  :type 'string
  :group 'beam)

(defcustom beam-list-files-with-beamignore-command "git ls-files --full-name -X $BEAM_IGNORE_FILE"
  "Command to get a list of files for project when BEAM_IGNORE_FILE environment
variable is set and a .beamignore file is found.  This file should be in the same
format as .gitignore, but specifies a different set of files for Beam to ignore."
  :type 'string
  :group 'beam)

(defcustom beam-project-landing-function 'beam-dired-landing
  "Function to run after switching to a project."
  :type 'symbol
  :group 'beam)

(defcustom beam-preferred-completion-method 'ivy
  "Method to use (if multiple are availabe) for completion.  Ido is the fallback."
  :type 'symbol
  :group 'beam
  :options '(helm ivy ido))

(defcustom beam-find-adds-to-file-history t
  "When finding a project file, add it to find-file-history."
  :type 'boolean
  :group 'beam)

(defcustom beam-write-txt-on-op nil
  "When performing operations that update the project list, automatically write ~/.emacs.d/beam-projects.txt"
  :type 'boolean
  :group 'beam)

;;;

(defvar beam-local-projects ()
  "List of paths which are projects.  They should have a .project-root file.")

(defvar beam--file-cache ()
  "Cache of listed files by project")

;;;

(cl-defmacro beam--with-file ((filename &key readp writep) &body body)
  (declare (indent 1))
  `(with-temp-buffer
     (prog1
         ,(if readp
              `(when (file-exists-p ,filename)
                 (insert-file-contents ,filename)
                 ,@body)
            `(progn ,@body))
       (when ,writep
         (write-region 1 (point-max) ,filename nil -1)))))

(cl-defmacro beam--with-projects-file ((&key readp writep) &body body)
  (declare (indent 1))
  `(beam--with-file (beam-local-projects-file :readp ,readp :writep ,writep) ,@body))

(defun beam--choose (prompt list)
  (cl-case beam-preferred-completion-method
    (helm (helm :sources `((name . ,prompt)
                           (candidates . ,list)
                           (action . ,(lambda (x) x)))))
    (ivy (ivy-read (concat prompt "> ") list :sort t))
    (t (ido-completing-read (concat prompt "> ") list))))

;;;

(defun beam--read-projects ()
  (beam--with-projects-file (:readp t)
    (setq beam-local-projects (read (buffer-string)))))

(defun beam--write-projects ()
  (beam--with-projects-file (:writep t)
    (print beam-local-projects (current-buffer)))
  (when beam-write-txt-on-op
    (beam-write-projects-txt)))

(defun beam-write-projects-txt ()
  (interactive)
  (beam--with-file ("~/.emacs.d/beam-projects.txt" :writep t)
    (loop for project in (sort (copy-list beam-local-projects)
                               (lambda (a b) (string-collate-lessp (car a) (car b))))
          do
          (princ (format "%s %s\n" (car project) (cadr project))
                 (current-buffer)))))

(defun beam--project-root-from-name (project)
  (cadr (assoc project beam-local-projects 'equal)))

(defun beam--find-project-root (dir)
  (let ((default-directory (expand-file-name (or dir default-directory))))
    (locate-dominating-file default-directory ".project-root")))

(defun beam-project-root (&optional dir)
  (let* ((root (beam--find-project-root dir)))
    (prog1 root
      (when root (beam--add-project root)))))

(defun beam--dir-project-name (dir)
  (file-name-nondirectory (directory-file-name dir)))

(defun beam-project-name (&optional project)
  (let ((default-directory (beam-project-root project)))
    (beam--dir-project-name (beam-project-root))))

(defun beam--add-project (dir)
  (let* ((name (beam--dir-project-name dir))
         (entry (list name (expand-file-name dir))))
    (cl-flet ((test-fn (a b) (equal (car a) (car b))))
      ;; Yes, there's pushnew; no, pushnew *doesn't* tell you if it didn't push
      (unless (find entry beam-local-projects :test #'test-fn)
        (push entry beam-local-projects)
        (beam--write-projects)
        (message "Added project %s (%s)." name dir)))))

(defun beam--ensure-project-root (dir)
  (or (file-exists-p (concat (file-name-as-directory dir) ".project-root"))
      (when (yes-or-no-p (format "No .project-root in '%s'.  Create? " dir))
        (let ((default-directory dir))
          (beam--with-file (".project-root" :writep t))
          t))))

(defun beam-add-project (&optional dir)
  (interactive "D")
  (when (beam--ensure-project-root dir)
    (unless (beam--add-project dir)
      (message "Already a project: %s" dir))))

(defun beam--list-files-to (dir buffer)
  (let* ((default-directory dir)
         (process-environment process-environment)
         (beamignore-p (file-exists-p ".beamignore")))
    (when beamignore-p
      (push (concat "BEAM_IGNORE_FILE=.beamignore") process-environment))
    (let ((s (if beamignore-p
                 (shell-command-to-string beam-list-files-with-beamignore-command)
               (shell-command-to-string beam-list-files-command))))
      (with-current-buffer (get-buffer-create buffer)
        (insert s)))))

(defun beam--list-files (dir)
  (let* ((default-directory dir)
         (process-environment process-environment)
         (beamignore-p (file-exists-p ".beamignore")))
    (when beamignore-p
      (push (concat "BEAM_IGNORE_FILE=.beamignore") process-environment))
    (if beamignore-p
        (shell-command-to-string beam-list-files-with-beamignore-command)
      (shell-command-to-string beam-list-files-command))))

;;; fixme use something that can detect errors
(defun beam--make-project-file-list (dir)
  (let* ((default-directory dir)
         (text (beam--list-files dir)))
    (split-string text "\n" t "^$")))

(defun beam--project-files-caching (clear-cache)
  (let* ((dir (expand-file-name default-directory))
         (cached (assoc dir beam--file-cache #'equal)))
    (if cached
        (when clear-cache
          (rplacd cached (beam--make-project-file-list dir)))
      (setq cached (cons dir (beam--make-project-file-list dir)))
      (push cached beam--file-cache))
    (cdr cached)))

(defun beam-find-files (&optional clear-cache)
  (interactive "p")
  (let* ((cached (beam--project-files-caching clear-cache))
         (filename (abbreviate-file-name
                    (expand-file-name (beam--choose "Project File" cached)))))
    (find-file filename)
    (when beam-find-adds-to-file-history
      (push filename file-name-history))))

(defun beam--make-project-list-x ()
  (let* ((longest (reduce (lambda (a b) (max a b))
                          (mapcar (lambda (x) (length (car x)))
                                  beam-local-projects)
                          :initial-value 0))
         (fmt (format "%%-%ss       %%s" longest)))
    (sort
     (mapcar (lambda (x)
               (message fmt (car x) (abbreviate-file-name (cadr x))))
             beam-local-projects)
     #'string<)))

(defun beam--make-project-list ()
  (sort (mapcar #'car beam-local-projects) #'string<))

(defun beam--select-project (title)
  (beam--choose title (beam--make-project-list)))

(defun beam-remove-project (&optional project-name)
  (interactive (list (beam--select-project "Remove Beam Project")))
  (setq beam-local-projects
        (delete-if (lambda (a) (equal (car a) project-name))
                   beam-local-projects))
  (beam--write-projects))

(defun beam-cleanup-projects ()
  "Remove projects whose paths no longer exist."
  (interactive)
  (let (deleted)
    (setq beam-local-projects
          (delete-if (lambda (x)
                       (when (not (file-exists-p (cadr x)))
                         (push (car x) deleted)))
                     beam-local-projects))
    (beam--write-projects)
    (if deleted
        (message "Removed: %s" deleted)
      (message "No projects removed"))))

(cl-defmacro beam--in-project-dir ((project-name) &body body)
  (declare (indent 1))
  (let ((d (gensym))
        (p (gensym)))
    `(let* ((,p ,project-name)
            (,d (beam--project-root-from-name ,p)))
       (if (null ,d)
           (message "%s is not a project" ,p)
         (let ((default-directory ,d))
           ,@body)))))

;;; Because writing simple functions is apparently hard
;;; This is not used now, but this is how you would
(defun beam--find-vc-for-dir (&optional dir)
  (let ((dir (or dir default-directory)))
    (find-if (lambda (x) (vc-call-backend x 'responsible-p dir))
             vc-handled-backends)))

(defun beam-dired-landing ()
  (dired default-directory))

(defun beam-switch-project (&optional project)
  (interactive (list (beam--select-project "Select Project")))
  (beam--in-project-dir (project)
    (beam--ensure-project-root (beam--project-root-from-name project))
    (funcall beam-project-landing-function)))

(defun beam-find-in-project (&optional clear-cache)
  (interactive "P")
  (let* ((root (or (beam-project-root)))
         (project (if root (beam-project-name) (beam--select-project "Select Project"))))
    (beam--in-project-dir (project)
      (beam-find-files clear-cache))))

(beam--read-projects)

(provide 'beam)

;;; Projectile Compatibility Layer
(defun projectile-project-root (&optional dir)
  (beam-project-root dir))

(defun projectile-project-name (&optional project)
  (beam-project-name project))

(provide 'projectile)


;;; project.el
(defun beam-project-find-function (dir)
  (list 'beam (beam-project-root dir)))

(pushnew #'beam-project-find-function project-find-functions)

(cl-defmethod project-root ((p (head beam)))
  (beam-project-root (cadr p)))
