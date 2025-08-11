;;; +lsp-tofu.el -*- lexical-binding: t; -*-

;;; lsp-tofu.el --- Tofu Client settings         -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Ross Donaldson, Sibi Prabakaran

;; Author: Ross Donaldson, Sibi Prabakaran
;; Keywords: tofu lsp

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

;; LSP client for Tofu

;;; Code:

(require 'lsp-mode)
(require 'lsp-semantic-tokens)
(require 'lsp-protocol)
(require 'dash)

;; tofu-ls

(defgroup lsp-tofu-ls nil
  "LSP support for Tofu, using tofu-ls from Hashicorp."
  :group 'lsp-mode
  :link '(url-link "https://github.com/opentofu/tofu-ls")
  :package-version `(lsp-mode . "9.0.0"))

(defcustom lsp-tofu-ls-server "tofu-ls"
  "Path to the `tofu-ls' binary."
  :group 'lsp-tofu-ls
  :risky t
  :type '(choice
          (file :tag "File")
          (repeat string))
  :package-version `(lsp-mode . "9.0.0"))

(defcustom lsp-tofu-ls-enable-show-reference nil
  "Enable reference counts.

Display reference counts above top level blocks and
attributes.  This is an experimental feature provided by the
language server."
  :group 'lsp-tofu-ls
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-tofu-ls-validate-on-save nil
  "Enable validating the current open file on save.

This is an experimental feature provided by the language server."
  :group 'lsp-tofu-ls
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-tofu-ls-prefill-required-fields nil
  "Enable completion of required fields.

Enable autocompletion for required fields when completing
Tofu blocks.  This is an experimental feature provided by the
language server."
  :group 'lsp-tofu-ls
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-tofu-ls-providers-position-params nil
  "The optional providers tree position params.
Defaults to side following treemacs default."
  :type 'alist
  :group 'lsp-tofu-ls
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-tofu-ls-module-calls-position-params nil
  "The optional module calls tree position params.
Defaults to side following treemacs default."
  :type 'alist
  :group 'lsp-tofu-ls
  :package-version '(lsp-mode . "9.0.0"))

(defun lsp-tofu-ls--make-launch-cmd ()
  `(,lsp-tofu-ls-server "serve"))

(lsp-defun lsp-tofu-ls--show-references ((&Command :arguments?))
  "Show references for command with ARGS."
  (lsp-show-xrefs
   (lsp--locations-to-xref-items
    (lsp-request "textDocument/references"
                 (lsp--make-reference-params
                  (lsp--text-document-position-params nil (elt arguments? 0)))))
   t
   t))

(defun lsp-tofu-ls--custom-capabilities ()
  "Construct custom capabilities for the language server."
  (when lsp-tofu-ls-enable-show-reference
    '((experimental . ((showReferencesCommandId . "client.showReferences"))))))

(defun lsp-tofu-ls--init-options ()
  "Construct initialization options for the lanague server."
  `((experimentalFeatures . ((validateOnSave . ,(lsp-json-bool lsp-tofu-ls-validate-on-save))
                             (prefillRequiredFields . ,(lsp-json-bool lsp-tofu-ls-prefill-required-fields))))))

(defcustom lsp-tofu-semantic-token-faces
  '(("namespace" . lsp-face-semhl-namespace)
    ("type" . lsp-face-semhl-type)
    ("class" . lsp-face-semhl-class)
    ("enum" . lsp-face-semhl-enum)
    ("interface" . lsp-face-semhl-interface)
    ("struct" . lsp-face-semhl-struct)
    ("typeParameter" . lsp-face-semhl-type-parameter)
    ("parameter" . lsp-face-semhl-parameter)
    ("variable" . lsp-face-semhl-variable)
    ("property" . lsp-face-semhl-property)
    ("enumMember" . lsp-face-semhl-constant)
    ("event" . lsp-face-semhl-event)
    ("function" . lsp-face-semhl-function)
    ("method" . lsp-face-semhl-method)
    ("macro" . lsp-face-semhl-macro)
    ("keyword" . lsp-face-semhl-keyword)
    ("modifier" . lsp-face-semhl-member)
    ("comment" . lsp-face-semhl-comment)
    ("string" . lsp-face-semhl-string)
    ("number" . lsp-face-semhl-number)
    ("regexp" . lsp-face-semhl-regexp)
    ("operator" . lsp-face-semhl-operator)
    ("hcl-attrName" . lsp-face-semhl-member)
    ("hcl-blockType" . lsp-face-semhl-struct)
    ("hcl-blockLabel" . lsp-face-semhl-member)
    ("hcl-bool" . lsp-face-semhl-constant)
    ("hcl-string" . lsp-face-semhl-string)
    ("hcl-number" . lsp-face-semhl-number)
    ("hcl-objectKey" . lsp-face-semhl-member)
    ("hcl-mapKey" . lsp-face-semhl-member)
    ("hcl-keyword" . lsp-face-semhl-keyword)
    ("hcl-traversalStep" . lsp-face-semhl-member)
    ("hcl-typeCapsule" . lsp-face-semhl-type)
    ("hcl-typePrimitive" . lsp-face-semhl-type))
  "Mapping between terrafom-ls tokens and fonts to apply."
  :group 'lsp-tofu
  :type '(alist :key-type string :value-type face)
  :package-version '(lsp-mode . "8.1"))

(defcustom lsp-tofu-semantic-token-modifier-faces
  '(("declaration" . lsp-face-semhl-class)
    ("definition" . lsp-face-semhl-definition)
    ("readonly" . lsp-face-semhl-constant)
    ("static" . lsp-face-semhl-static)
    ("deprecated" . lsp-face-semhl-deprecated)
    ("abstract" . lsp-face-semhl-keyword)
    ("async" . lsp-face-semhl-macro)
    ("modification" . lsp-face-semhl-operator)
    ("documentation" . lsp-face-semhl-comment)
    ("defaultLibrary" . lsp-face-semhl-default-library)
    ("hcl-dependent" . lsp-face-semhl-constant)
    ("tofu-data" . lsp-face-semhl-constant)
    ("tofu-locals" . lsp-face-semhl-variable)
    ("tofu-module" . lsp-face-semhl-namespace)
    ("tofu-output" . lsp-face-semhl-constant)
    ("tofu-provider" . lsp-face-semhl-class)
    ("tofu-resource" . lsp-face-semhl-interface)
    ("tofu-provisioner" . lsp-face-semhl-default-library)
    ("tofu-connection" . lsp-face-semhl-constant)
    ("tofu-variable" . lsp-face-semhl-variable)
    ("tofu-tofu" . lsp-face-semhl-constant)
    ("tofu-backend" . lsp-face-semhl-definition)
    ("tofu-name" . lsp-face-semhl-interface)
    ("tofu-type" . lsp-face-semhl-type)
    ("tofu-requiredProviders" . lsp-face-semhl-default-library))
  "Mapping between tofu-ls modifiers and fonts to apply."
  :group 'lsp-tofu
  :type '(alist :key-type string :value-type face)
  :package-version '(lsp-mode . "8.1"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection #'lsp-tofu-ls--make-launch-cmd)
                  :major-modes '(terraform-mode)
                  :priority 1
                  :server-id 'tofu-ls
                  :action-handlers (ht ("client.showReferences" #'lsp-tofu-ls--show-references))
                  :semantic-tokens-faces-overrides `(:discard-default-modifiers t
                                                     :discard-default-types t
                                                     :modifiers ,lsp-tofu-semantic-token-modifier-faces
                                                     :types ,lsp-tofu-semantic-token-faces)
                  :initialization-options (lsp-tofu-ls--init-options)
                  :custom-capabilities (lsp-tofu-ls--custom-capabilities)))

(defun lsp-tofu-ls-validate ()
  "Execute tofu validate on project root."
  (interactive)
  (lsp-request
   "workspace/executeCommand"
   (list :command "tofu-ls.tofu.validate"
         :arguments (vector (format "uri=%s" (lsp--path-to-uri (lsp-workspace-root))))
         )
   :no-wait t
   :no-merge t))

(defun lsp-tofu-ls-init ()
  "Execute tofu init on project root.

This is a synchronous action."
  (interactive)
  (lsp-request
   "workspace/executeCommand"
   (list :command "tofu-ls.tofu.init"
         :arguments (vector (format "uri=%s" (lsp--path-to-uri (lsp-workspace-root)))))
   :no-wait nil
   :no-merge t))

(defun lsp-tofu-ls-version ()
  "Get information about the tofu binary version for the current module."
  (interactive)
  (let ((tofu-data (lsp-request
                    "workspace/executeCommand"
                    (list :command "tofu-ls.module.tofu"
                          :arguments (vector (format "uri=%s" (lsp--path-to-uri (lsp-workspace-root))))))))
    (lsp--info "Required: %s, Current: %s"
               (lsp:tofu-ls-module-tofu-required-version tofu-data)
               (lsp:tofu-ls-module-tofu-discovered-version tofu-data))))

(lsp-consistency-check lsp-tofu)

(defvar treemacs-position)
(defvar treemacs-width)
(declare-function lsp-treemacs-render "ext:lsp-treemacs" (tree title expand-depth &optional buffer-name))

(defvar-local lsp-tofu-ls--providers-tree-data nil)
(defvar-local lsp-tofu-ls--modules-call-tree-data nil)
(defvar-local lsp-tf--modules-control-buffer nil)
(defconst lsp-tofu-ls--providers-buffer-name "*Tofu Providers*")
(defconst lsp-tofu-ls--modules-buffer-name "*Tofu Modules*")

(defvar lsp-tofu-modules-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g") 'lsp-tofu-ls--modules-refresh)
    m)
  "Keymap for `lsp-tofu-modules-mode'.")

(define-minor-mode lsp-tofu-modules-mode "LSP Treemacs mode for tofu modules."
  :keymap lsp-tofu-modules-mode-map
  :group 'lsp-tofu-ls)

(cl-defstruct tf-package display-name doc-link installed-version version-constraint)

(cl-defstruct tf-module name doc-link version source-type dependent-modules)

(defun construct-tf-package (provider installed-version)
  "Construct `TF-PACKAGE' using PROVIDER and INSTALLED-VERSION."
  (make-tf-package :display-name (lsp-get provider :display_name)
                   :doc-link (lsp-get provider :docs_link)
                   :installed-version installed-version
                   :version-constraint (lsp-get provider :version_constraint)))

(lsp-defun construct-tf-module ((&tofu-ls:Module :name :docs-link :version :source-type :dependent-modules))
  "Construct `TF-MODULE' using MODULE."
  (make-tf-module :name name
                  :doc-link docs-link
                  :version version
                  :source-type source-type
                  :dependent-modules dependent-modules))

(lsp-defun lsp-tofu-ls--providers-to-tf-package ((&tofu-ls:Providers :provider-requirements :installed-providers))
  "Convert PROVIDERS-TREE-DATA to list of `tf-package'."
  (let* ((provider-requirements-keys (hash-table-keys provider-requirements))
         (installed-versions (mapcar (lambda (x) (lsp-get installed-providers (make-symbol (format ":%s" x)))) provider-requirements-keys))
         (providers (mapcar (lambda (x) (lsp-get provider-requirements (make-symbol (format ":%s" x)))) provider-requirements-keys))
         (tf-packages (-zip-with (lambda (x y) (construct-tf-package x y)) providers installed-versions)))
    tf-packages))

(lsp-defun lsp-tofu-ls--modules-to-tf-module ((&tofu-ls:ModuleCalls :module-calls))
  "Convert MODULES-TREE-DATA to list of `TF-MODULE'."
  (let* ((modules (-map (lambda (x) (construct-tf-module x)) module-calls)))
    modules))

(defun lsp-tofu-ls--fetch-modules-data (project-root)
  "Fetch modules data and set it in `lsp-tofu-ls--modules-call-tree-data'."
  (let* ((tree-data (lsp-request
                     "workspace/executeCommand"
                     (list :command "tofu-ls.module.calls"
                           :arguments (vector (format "uri=%s" (lsp--path-to-uri project-root))))
                     :no-wait nil
                     :no-merge nil))
         (modules (lsp-tofu-ls--modules-to-tf-module tree-data)))
    (setq-local lsp-tofu-ls--modules-call-tree-data modules)))

(defun lsp-tofu-ls--fetch-providers ()
  "Fetch modules call data and set it in `lsp-tofu-ls--providers-tree-data'."
  (let* ((tree-data (lsp-request
                     "workspace/executeCommand"
                     (list :command "tofu-ls.module.providers"
                           :arguments (vector (format "uri=%s" (lsp--path-to-uri (lsp-workspace-root)))))
                     :no-wait nil
                     :no-merge nil))
         (tf-packages (lsp-tofu-ls--providers-to-tf-package tree-data)))
    (setq-local lsp-tofu-ls--providers-tree-data tf-packages)))

(defun lsp-tofu-ls--tf-packages-to-treemacs (tf-packages)
  "Convert list of `TF-PACKAGES' to treemacs compatible data."
  (mapcar (lambda (package) (list :label (format "%s %s" (tf-package-display-name package) (tf-package-installed-version package))
                                  :icon 'package
                                  :key (tf-package-display-name package)
                                  :children (list (list
                                                   :icon 'library
                                                   :label (tf-package-version-constraint package)))
                                  :ret-action (lambda (&rest _) (browse-url (tf-package-doc-link package))))) tf-packages))

(defun lsp-tofu-ls--tf-modules-to-treemacs (tf-modules)
  "Convert list of `TF-MODULES' to treemacs compatible data."
  (mapcar (lambda (module) (list :label (format "%s %s" (tf-module-name module) (tf-module-version module))
                                 :icon 'package
                                 :key (tf-module-name module)
                                 :ret-action (lambda (&rest _) (browse-url (tf-module-doc-link module)))
                                 )) tf-modules))

(defun lsp-tofu-ls--show-providers (ignore-focus?)
  "Show tofu providers and focus on it if IGNORE-FOCUS? is nil."
  (unless lsp-tofu-ls--providers-tree-data
    (lsp-tofu-ls--fetch-providers))
  (let* ((lsp-tofu-treemacs
          (lsp-tofu-ls--tf-packages-to-treemacs lsp-tofu-ls--providers-tree-data))
         (buffer (lsp-treemacs-render lsp-tofu-treemacs
                                      lsp-tofu-ls--providers-buffer-name
                                      t
                                      "Tofu Providers"))
         (position-params (or lsp-tofu-ls-providers-position-params
                              `((side . ,treemacs-position)
                                (slot . 2)
                                (window-width . ,treemacs-width))))
         (window
          (display-buffer-in-side-window buffer position-params)))
    (unless ignore-focus?
      (select-window window)
      (set-window-dedicated-p window t))))

(defun lsp-tofu-ls--show-module-calls (ignore-focus? project-root)
  "Show tofu modules and focus on it if IGNORE-FOCUS? is nil."
  (unless lsp-tofu-ls--modules-call-tree-data
    (lsp-tofu-ls--fetch-modules-data project-root))
  (unless lsp-tofu-ls--modules-call-tree-data
    (error "Modules data is empty"))
  (let* ((lsp-tofu-treemacs
          (lsp-tofu-ls--tf-modules-to-treemacs lsp-tofu-ls--modules-call-tree-data))
         (buffer (lsp-treemacs-render lsp-tofu-treemacs
                                      lsp-tofu-ls--modules-buffer-name
                                      t
                                      "Tofu Modules"))
         (modules-buffer (current-buffer))
         (position-params (or lsp-tofu-ls-module-calls-position-params
                              `((side . ,treemacs-position)
                                (slot . 1)
                                (window-width . ,treemacs-width))))
         (window
          (display-buffer-in-side-window buffer position-params)))
    (select-window window)
    (setq-local lsp-tf--modules-control-buffer modules-buffer)
    (lsp-tofu-modules-mode t)
    (set-window-dedicated-p window t)
    (when ignore-focus?
      (select-window (previous-window)))))

(defun lsp-tofu-ls--refresh-module-calls ()
  "Refresh tofu modules."
  (lsp-tofu-ls--fetch-modules-data (lsp-workspace-root))
  (unless lsp-tofu-ls--modules-call-tree-data
    (error "Modules data is empty"))
  (let* ((lsp-tofu-treemacs
          (lsp-tofu-ls--tf-modules-to-treemacs lsp-tofu-ls--modules-call-tree-data))
         (buffer (lsp-treemacs-render lsp-tofu-treemacs
                                      lsp-tofu-ls--modules-buffer-name
                                      t
                                      "Tofu Modules"))
         (position-params (or lsp-tofu-ls-module-calls-position-params
                              `((side . ,treemacs-position)
                                (slot . 1)
                                (window-width . ,treemacs-width))))
         (window
          (display-buffer-in-side-window buffer position-params)))
    (select-window window)
    (lsp-tofu-modules-mode t)
    (set-window-dedicated-p window t)
    (lsp--info "Refresh completed")))

(defun lsp-tofu-ls-providers (&optional ignore-focus?)
  "Show tofu providers with focus on it if IGNORE-FOCUS? is nil."
  (interactive)
  (if (require 'lsp-treemacs nil t)
      (lsp-tofu-ls--show-providers ignore-focus?)
    (error "The package lsp-treemacs is not installed")))

(defun lsp-tofu-ls-module-calls (&optional ignore-focus?)
  "Show tofu modules with focus on it if IGNORE-FOCUS? is nil."
  (interactive)
  (if (require 'lsp-treemacs nil t)
      (lsp-tofu-ls--show-module-calls ignore-focus? (lsp-workspace-root))
    (error "The package lsp-treemacs is not installed")))

(defun lsp-tofu-ls--modules-refresh ()
  "Refresh tofu modules data."
  (interactive)
  (unless (buffer-live-p lsp-tf--modules-control-buffer)
    (error "Original buffer not present.  Do M-x lsp-tofu-ls-module-calls"))
  (with-current-buffer lsp-tf--modules-control-buffer
    (lsp-tofu-ls--refresh-module-calls)))

(provide 'lsp-tofu)
