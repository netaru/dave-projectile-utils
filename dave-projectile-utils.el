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

(defun dave-projectile--get-mvn-commands ()
  "Add maven commands if a `pom.xml' file exists in the `projectile-project-root' directory. Otherwise nil."
  (if (file-exists-p (concat (projectile-project-root) "pom.xml")) dave-projectile-mvn-commands))

(defun dave-projectile--get-docker-compose-commands ()
  "Add docker compose commands if a `docker-compose.ya?ml' file exists in the `projectile-project-root' directory. Otherwise nil."
  (let ((folder (projectile-project-root)))
    (if (or (file-exists-p (concat folder "docker-compose.yml"))
            (file-exists-p (concat folder "docker-compose.yaml"))) dave-projectile-docker-compose-commands)))

(defun dave-projectile--get-commands ()
  "Get all completions for the current projectile project."
  (append dave-projectile-generic-commands
          (dave-projectile--get-mvn-commands)
          (dave-projectile--get-docker-compose-commands)))
(provide 'dave-projectile-utils)
;;; dave-projectile-utils.el ends here
