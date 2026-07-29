;;; emacs-agent-schema.el --- Tool registry and schema checks  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tool schemas are advertised verbatim.  A deliberately small validator checks
;; the subset used by this package before a handler is called.

;;; Code:

(require 'cl-lib)
(require 'seq)

(define-error 'emacs-agent-schema-error "Tool schema validation failed")
(define-error 'emacs-agent-tool-error "Agent editor tool failed")

(cl-defstruct (emacs-agent-tool (:constructor emacs-agent-tool-create))
  name description input-schema output-schema handler class)

(defvar emacs-agent--tools (make-hash-table :test #'equal)
  "Registered MCP tools, indexed by wire name.")

(defun emacs-agent-tool-register
    (name description input-schema output-schema handler &optional class)
  "Register a tool named NAME.
DESCRIPTION, INPUT-SCHEMA, and OUTPUT-SCHEMA are advertised over MCP.
HANDLER is called with (ARGUMENTS CONTEXT).  CLASS is a policy hint."
  (unless (and (stringp name) (not (string-empty-p name))
               (stringp description) (functionp handler))
    (error "Invalid tool registration"))
  (puthash name
           (emacs-agent-tool-create
            :name name :description description
            :input-schema input-schema :output-schema output-schema
            :handler handler :class (or class 'read-only))
           emacs-agent--tools)
  name)

(defun emacs-agent-tool-clear ()
  "Remove all registered tools."
  (clrhash emacs-agent--tools))

(defun emacs-agent-tool-get (name)
  "Return the tool registered as NAME, or nil."
  (gethash name emacs-agent--tools))

(defun emacs-agent-tool-list ()
  "Return all registered tools sorted deterministically by name."
  (let (tools)
    (maphash (lambda (_name tool) (push tool tools)) emacs-agent--tools)
    (sort tools (lambda (left right)
                  (string< (emacs-agent-tool-name left)
                           (emacs-agent-tool-name right))))))

(defun emacs-agent-schema-tool-descriptor (tool)
  "Return the MCP descriptor for TOOL."
  `((name . ,(emacs-agent-tool-name tool))
    (description . ,(emacs-agent-tool-description tool))
    (inputSchema . ,(emacs-agent-tool-input-schema tool))
    (outputSchema . ,(emacs-agent-tool-output-schema tool))))

(defun emacs-agent-schema--type-p (value type)
  "Return whether VALUE has JSON schema TYPE."
  (if (or (listp type) (vectorp type))
      (seq-some
       (lambda (candidate)
         (emacs-agent-schema--type-p value candidate))
       type)
    (pcase type
      ("object" (and (listp value)
                     (cl-every (lambda (item)
                                 (and (consp item) (symbolp (car item))))
                               value)))
      ("array" (or (listp value) (vectorp value)))
      ("string" (stringp value))
      ("integer" (integerp value))
      ("number" (numberp value))
      ("boolean" (memq value '(t :false)))
      ("null" (eq value :null))
      (_ t))))

(defun emacs-agent-schema-validate (value schema &optional path)
  "Validate VALUE against the package's supported subset of SCHEMA.
PATH is used only to make errors actionable.  Return VALUE on success."
  (let* ((path (or path "$"))
         (type (alist-get 'type schema))
         (enum (alist-get 'enum schema))
         (required (alist-get 'required schema))
         (properties (alist-get 'properties schema))
         (items (alist-get 'items schema)))
    (when (and type (not (emacs-agent-schema--type-p value type)))
      (signal 'emacs-agent-schema-error
              (list `((path . ,path) (reason . "type")
                      (expected . ,type)))))
    (when (and enum (not (seq-contains-p enum value #'equal)))
      (signal 'emacs-agent-schema-error
              (list `((path . ,path) (reason . "enum")
                      (expected . ,enum)))))
    (when (equal type "object")
      (seq-doseq (key required)
        (unless (assq (intern key) value)
          (signal 'emacs-agent-schema-error
                  (list `((path . ,path) (reason . "required")
                          (property . ,key))))))
      (dolist (entry value)
        (if-let* ((property (assq (car entry) properties)))
            (emacs-agent-schema-validate
             (cdr entry) (cdr property)
             (concat path "." (symbol-name (car entry))))
          (when (eq (alist-get 'additionalProperties schema) :false)
            (signal 'emacs-agent-schema-error
                    (list `((path . ,path)
                            (reason . "additionalProperties")
                            (property
                             . ,(symbol-name (car entry))))))))))
    (when (and (equal type "array") items)
      (let ((index 0))
        (seq-doseq (item value)
          (emacs-agent-schema-validate
           item items (format "%s[%d]" path index))
          (setq index (1+ index)))))
    value))

(provide 'emacs-agent-schema)
;;; emacs-agent-schema.el ends here
