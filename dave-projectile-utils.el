;;; dave-projectile-utils.el --- Run commands in projectile-project-root  -*- lexical-binding: t -*-

;; Public domain.

;; Author:    David Jonsson <david.jonsson306@gmail.com>
;; URL:       N/A
;; Version:   0.0.1

;;; Commentary:

;; My first test package

;;; Code:
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

(defun dave-projectile--get-mvn-commands (&optional in)
  "Add maven commands if a `pom.xml' file exists in the `projectile-project-root' directory. Otherwise nil."
  (let ((folder (or in
                    (projectile-project-root))))
    (if (file-exists-p (concat folder "pom.xml")) dave-projectile-mvn-commands)))

(defun dave-projectile--get-docker-compose-commands (&optional in)
  "Add docker compose commands if a `docker-compose.ya?ml' file exists in the `projectile-project-root' directory. Otherwise nil."
  (let ((folder (or in
                    (projectile-project-root))))
    (if (or (file-exists-p (concat folder "docker-compose.yml"))
            (file-exists-p (concat folder "docker-compose.yaml"))) dave-projectile-docker-compose-commands)))

(defun dave-projectile--get-commands ()
  "Get all completions for the current projectile project."
  (let ((folder (projectile-project-root)))
    (append dave-projectile-generic-commands
            (dave-projectile--get-mvn-commands folder)
            (dave-projectile--get-docker-compose-commands folder))))

;;;###autoload
(defun dave-projectile-execute (&optional in)
  "Execute a command from the `projectile-project-root' from a predefined list of available commands.

If the command is of type `string' execute the command directly with the `compile' command.

If the command is of type `function' execute the function as is. It's up to the function to decide
what will happen.

If the command is of type `cons' try to fetch the function from the cons otherwise print a message
telling the user that nothing has happend.

If the optional argument IN is set use that as the commands instead.
"
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (targets (dave-projectile--get-commands))
         (command (or in
                      (cdr (assoc (completing-read "Target: " targets nil t) targets)))))
    (pcase command
      ((pred functionp) (funcall command))
      ((pred consp) (let ((inner (car (cdr command))))
                      (if (functionp inner)
                          (funcall inner)
                        (message "Unable to execute: '%s'" command))))
      ((pred stringp) (compile command))
      (value (message "Unable to execute: '%s'" command)))))

(provide 'dave-projectile-utils)
;;; dave-projectile-utils.el ends here
