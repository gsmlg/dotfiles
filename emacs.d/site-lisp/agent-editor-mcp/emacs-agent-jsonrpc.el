;;; emacs-agent-jsonrpc.el --- JSON-RPC 2.0 helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Pure parsing and envelope construction for the protocol adapters.

;;; Code:

(require 'json)

(define-error 'emacs-agent-jsonrpc-error "JSON-RPC error")

(defconst emacs-agent-jsonrpc-parse-error -32700)
(defconst emacs-agent-jsonrpc-invalid-request -32600)
(defconst emacs-agent-jsonrpc-method-not-found -32601)
(defconst emacs-agent-jsonrpc-invalid-params -32602)
(defconst emacs-agent-jsonrpc-internal-error -32603)
(defconst emacs-agent-jsonrpc-unsupported-protocol-version -32022)

(defun emacs-agent-jsonrpc--field (object key)
  "Return KEY from alist OBJECT."
  (alist-get key object))

(defun emacs-agent-jsonrpc-parse (text)
  "Parse TEXT as a validated JSON-RPC request alist.
Signal `emacs-agent-jsonrpc-error' with an error code and data on failure."
  (let ((object
         (condition-case error
             (json-parse-string text
                                :object-type 'alist
                                :array-type 'list
                                :null-object :null
                                :false-object :false)
           (json-parse-error
            (signal 'emacs-agent-jsonrpc-error
                    (list emacs-agent-jsonrpc-parse-error
                          "Parse error"
                          `((detail . ,(error-message-string error)))))))))
    (unless (and (listp object)
                 (equal (emacs-agent-jsonrpc--field object 'jsonrpc) "2.0")
                 (stringp (emacs-agent-jsonrpc--field object 'method))
                 (let ((id-cell (assq 'id object)))
                   (or (null id-cell)
                       (stringp (cdr id-cell))
                       (integerp (cdr id-cell))))
                 (let ((params (assq 'params object)))
                   (or (null params)
                       (listp (cdr params)))))
      (signal 'emacs-agent-jsonrpc-error
              (list emacs-agent-jsonrpc-invalid-request
                    "Invalid Request" nil)))
    object))

(defun emacs-agent-jsonrpc-notification-p (request)
  "Return non-nil when REQUEST is a JSON-RPC notification."
  (null (assq 'id request)))

(defun emacs-agent-jsonrpc-result (id result)
  "Construct a successful JSON-RPC envelope for ID and RESULT."
  `((jsonrpc . "2.0") (id . ,id) (result . ,result)))

(defun emacs-agent-jsonrpc-error-result (id code message &optional data)
  "Construct a JSON-RPC error envelope.
ID, CODE, MESSAGE, and optional DATA have their usual JSON-RPC meanings."
  `((jsonrpc . "2.0")
    (id . ,id)
    (error . ((code . ,code)
              (message . ,message)
              ,@(when data `((data . ,data)))))))

(defun emacs-agent-jsonrpc-condition-result (id condition)
  "Turn CONDITION into a JSON-RPC error envelope associated with ID."
  (let ((values (cdr condition)))
    (emacs-agent-jsonrpc-error-result
     id (or (nth 0 values) emacs-agent-jsonrpc-internal-error)
     (or (nth 1 values) "Internal error")
     (nth 2 values))))

(defun emacs-agent-jsonrpc-encode (object)
  "Encode OBJECT as compact UTF-8 JSON."
  (encode-coding-string
   (json-serialize object :null-object :null :false-object :false)
   'utf-8 t))

(provide 'emacs-agent-jsonrpc)
;;; emacs-agent-jsonrpc.el ends here
