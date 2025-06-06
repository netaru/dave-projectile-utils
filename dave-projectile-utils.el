;;; dave-projectile-utils.el --- Run commands in projectile-project-root  -*- lexical-binding: t -*-

;; Author:    David Jonsson <david.jonsson306@gmail.com>
;; URL:       N/A
;; Version:   0.0.4
;; Package-Requires: ((emacs "26.1") (projectile "2.9.1") (consult "0.32"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Just a lazy convenience package for me.
;; Projectile already supports running commands from the root directory I just wanted a list of predefined actions that could be taken.
;; The required version of projectile is higher than it should be it's just the version that was installed when this package was made.

;;; Code:
(defcustom dave-projectile-work-source-directory (concat(getenv "HOME") "/workspace/git")
  "Directory of work projects."
  :group 'dave-projectile
  :type 'directory)

(defcustom dave-projectile-node-script-prefix "npm run"
  "Prefix to use with node commands found in `package.json'"
  :group 'dave-projectile
  :type 'string)

(defcustom dave-projectile-generic-commands
  '(("java version" . "java -version")
    ("environment" . "env"))
  "List of commands that should always be available."
  :group 'dave-projectile
  :type 'alist)

(defcustom dave-projectile-mvn-commands
  '(("install skip test" . "mvn clean install -DskipTests")
    ("install" . "mvn clean install")
    ("eclipse" . "mvn eclipse:eclipse")
    ("eclipse clean" . "mvn eclipse:clean")
    ("effective pom" . "mvn help:effective-pom")
    ("dependency tree" . "mvn dependency:tree")
    ("clean" . "mvn clean")
    ("package skip test" . "mvn package -DskipTests")
    ("package" . "mvn package")
    ("compile" . "mvn clean compile")
    ("deploy" . "mvn clean deploy")
    ("javadocs" . "mvn dependency:sources dependency:resolve -Dclassifier=javadoc")
    ("sonar local" . "mvn sonar:sonar")
    ("maven version" . "mvn -version"))
  "List of available maven commands."
  :group 'dave-projectile
  :type 'alist)

(defcustom dave-projectile-docker-compose-commands
  '(("docker compose up" . "docker compose up -d")
    ("docker compose logs" . "docker compose logs")
    ("docker compose down" . "docker compose down"))
  "List of available docker commands."
  :group 'dave-projectile
  :type 'alist)

(defcustom dave-projectile-node-commands
  (let* ((prefix dave-projectile-node-script-prefix)
         (fn (lambda (k v) (cons (format "%s %s" prefix k) (format "%s %s" prefix v)))))
    `(,(apply fn '("install" "install"))
      ,(apply fn '("install legacy" "install --legacy-peer-deps"))))
  "List of available node commands."
  :group 'dave-projectile
  :type 'alist)

(defun dave-projectile--get-mvn-commands (&optional in)
  "Add maven commands if a `pom.xml' file exists in the `projectile-project-root' directory. Otherwise nil."
  (let ((folder (or in
                    (projectile-project-root))))
    (when (file-exists-p (concat folder "pom.xml")) dave-projectile-mvn-commands)))

(defun dave-projectile--get-docker-compose-commands (&optional in)
  "Add docker compose commands if a `docker-compose.ya?ml' file exists in the `projectile-project-root' directory. Otherwise nil."
  (let ((folder (or in
                    (projectile-project-root))))
    (when (or (file-exists-p (concat folder "docker-compose.yml"))
            (file-exists-p (concat folder "docker-compose.yaml"))) dave-projectile-docker-compose-commands)))

(defun dave-projectile--get-package-json-commands (&optional in)
  "Add scripts commands if a `package.json' file exists in the `projectile-project-root' directory. Otherwise nil."
  (let ((folder (or in
                    (projectile-project-root))))
    (when (file-exists-p (concat folder "package.json"))
      (append
       dave-projectile-node-commands
       (seq-map
        (lambda (elm)
          (let* ((key (concat dave-projectile-node-script-prefix " " (symbol-name (car elm))))
                 (value (propertize key 'display (cdr elm))))
            `(,key . ,value)))
        (alist-get 'scripts (json-read-file "package.json")))))))

(defun dave-projectile--get-commands (&optional folder)
  "Get all completions for the current projectile project."
  (let ((folder (or folder (projectile-project-root))))
    (append
     dave-projectile-generic-commands
     (dave-projectile--get-mvn-commands folder)
     (dave-projectile--get-docker-compose-commands folder)
     (dave-projectile--get-package-json-commands folder))))

;;;###autoload
(defun dave-projectile-execute (&optional folder in)
  "Execute a command from the `projectile-project-root' from a predefined list of available commands.

If the command is of type `string' execute the command directly with the `compile' command.

If the command is of type `function' execute the function as is. It's up to the function to decide
what will happen.

If the command is of type `cons' try to fetch the function from the cons otherwise print a message
telling the user that nothing has happend.

If the optional argument IN is set use that as the commands instead.
"
  (interactive)
  (let* ((default-directory (or folder (projectile-project-root)))
         (targets (dave-projectile--get-commands folder))
         (maxlen (+ 2 (apply #'max (seq-map (lambda (key) (length (car key))) targets))))
         (completion-extra-properties
          `(:annotation-function
            ,(lambda (key)
               (let ((fmt (concat "%" (int-to-string (- maxlen (length key))) "s '%s'")))
                 (format fmt "-" (cdr (assoc key targets)))))))
         (command (or in
                      (cdr (assoc (completing-read "Target: " targets nil t) targets)))))
    (pcase command
      ((pred functionp) (funcall command))
      ((pred consp) (let ((inner (car (cdr command))))
                      (if (functionp inner)
                          (funcall inner)
                        (message "Unable to execute: '%s'" command))))
      ((pred stringp) (compilation-start command t (lambda (&rest _) (generate-new-buffer-name (format "*%s*" command)))))
      (value (message "Unable to execute: '%s'" command)))))

;;;###autoload
(defun dave-projectile-rg-todo ()
  "Use `consult-ripgrep' to search project for FIXME and TODO."
  (interactive)
  (consult-ripgrep (projectile-project-root) "\\(FIXME\\|TODO\\)"))

;;;###autoload
(defun dave-projectile-find-notes ()
  "Function to quickly open `projectile-project-root'/notes.org"
  (interactive)
  (find-file (concat (projectile-project-root) "notes.org")))

;;;###autoload
(defun dave-projectile-rg-projects (&optional input)
  "Function to quickly open `projectile-project-root'/notes.org"
  (interactive)
  (consult-ripgrep dave-projectile-work-source-directory input))

(provide 'dave-projectile-utils)
;;; dave-projectile-utils.el ends here
