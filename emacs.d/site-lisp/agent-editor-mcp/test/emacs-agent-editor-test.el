;;; emacs-agent-editor-test.el --- Entrypoint integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Lifecycle and composed tool-surface tests.

;;; Code:

(require 'ert)
(require 'emacs-agent-editor)

(defmacro emacs-agent-editor-test--with-server (&rest body)
  "Run BODY with a temporary Agent Editor server."
  (declare (indent 0) (debug t))
  `(let ((root (make-temp-file "emacs-agent-editor-" t))
         (state (make-temp-file "emacs-agent-editor-state-" t))
         (emacs-agent-editor-state-directory nil)
         (emacs-agent-editor-bearer-token "test-bearer-token")
         (emacs-agent-editor-access-mode 'autonomous)
         (emacs-agent-editor-save-policy 'immediate))
     (setq emacs-agent-editor-state-directory state)
     (unwind-protect
         (progn
           (emacs-agent-editor-start root)
           ,@body)
       (ignore-errors (emacs-agent-editor-stop))
       (delete-directory root t)
       (delete-directory state t))))

(ert-deftest emacs-agent-editor-start-publishes-private-connection ()
  (emacs-agent-editor-test--with-server
    (should (emacs-agent-editor-running-p))
    (should (= (length (emacs-agent-tool-list)) 12))
    (should (file-exists-p emacs-agent-editor--connection-file))
    (should (= (logand (file-modes emacs-agent-editor--connection-file)
                       #o777)
               #o600))
    (should
     (string-match-p
      "\"tools\":\\["
      (decode-coding-string
       (emacs-agent-jsonrpc-encode
       (emacs-agent-protocol-tool-list-result t))
       'utf-8)))))

(ert-deftest emacs-agent-editor-serves-modern-discovery-over-http ()
  (emacs-agent-editor-test--with-server
    (let* ((body
            (encode-coding-string
             (json-serialize
              '((jsonrpc . "2.0")
                (id . 1)
                (method . "server/discover")
                (params
                 . ((_meta
                     . ((io\.modelcontextprotocol/protocolVersion
                         . "2026-07-28")
                        (io\.modelcontextprotocol/clientInfo
                         . ((name . "ert") (version . "1")))
                        (io\.modelcontextprotocol/clientCapabilities
                         . ())))))))
             'utf-8 t))
           (response "")
           (client
            (make-network-process
             :name "emacs-agent-editor-test-client"
             :host "127.0.0.1"
             :service
             (emacs-agent-http-server-port
              emacs-agent-editor--http-server)
             :coding 'binary
             :noquery t
             :filter
             (lambda (_process chunk)
               (setq response (concat response chunk))))))
      (process-send-string
       client
       (concat
        "POST /mcp HTTP/1.1\r\n"
        "Host: 127.0.0.1\r\n"
        "Authorization: Bearer test-bearer-token\r\n"
        "Content-Type: application/json\r\n"
        "Accept: application/json, text/event-stream\r\n"
        "MCP-Protocol-Version: 2026-07-28\r\n"
        "Mcp-Method: server/discover\r\n"
        (format "Content-Length: %d\r\n\r\n" (length body))
        body))
      (process-send-eof client)
      (let ((deadline (+ (float-time) 2)))
        (while (and (< (float-time) deadline)
                    (not (string-match-p "supportedVersions" response)))
          (accept-process-output nil 0.05)))
      (should (string-prefix-p "HTTP/1.1 200" response))
      (should (string-match-p "\"supportedVersions\":\\[\"2026-07-28\"\\]"
                              response)))))

(ert-deftest emacs-agent-editor-create-edit-and-rollback ()
  (emacs-agent-editor-test--with-server
    (let* ((context
            (emacs-agent-request-create
             :id 1 :protocol-version "2026-07-28"
             :client-info '((name . "ert"))))
           (created
            (emacs-agent-editor--document-create
             '((path . "sample.txt") (content . "hello\n")) context))
           (read
            (emacs-agent-editor--document-read
             '((path . "sample.txt")) context))
           (revision (alist-get 'revision read))
           (edited
            (emacs-agent-editor--document-apply-edits
             `((path . "sample.txt")
               (expected_revision . ,revision)
               (edits
                . (((start . ((line . 1) (column . 0)))
                    (end . ((line . 1) (column . 5)))
                    (new_text . "goodbye")))))
             context))
           (changeset-id (alist-get 'changeset_id edited)))
      (should (stringp (alist-get 'changeset_id created)))
      (should (equal
               (with-temp-buffer
                 (insert-file-contents
                  (expand-file-name
                   "sample.txt"
                   (emacs-agent-workspace-root
                    emacs-agent-editor--workspace)))
                 (buffer-string))
               "goodbye\n"))
      (emacs-agent-editor--changeset-rollback
       `((changeset_id . ,changeset-id)) context)
      (should
       (equal
        (with-current-buffer
            (find-buffer-visiting
             (expand-file-name
              "sample.txt"
              (emacs-agent-workspace-root
               emacs-agent-editor--workspace)))
          (buffer-substring-no-properties (point-min) (point-max)))
        "hello\n")))))

(ert-deftest emacs-agent-editor-move-delete-and-rollback ()
  (emacs-agent-editor-test--with-server
    (let* ((context
            (emacs-agent-request-create
             :id 2 :protocol-version "2026-07-28"
             :client-info '((name . "ert"))))
           (_created
            (emacs-agent-editor--document-create
             '((path . "old.txt") (content . "content\n")) context))
           (revision
            (alist-get
             'revision
             (emacs-agent-editor--document-read
              '((path . "old.txt")) context)))
           (moved
            (emacs-agent-editor--document-move
             `((path . "old.txt") (new_path . "new.txt")
               (expected_revision . ,revision))
             context))
           (move-id (alist-get 'changeset_id moved))
           (root (emacs-agent-workspace-root
                  emacs-agent-editor--workspace)))
      (should-not (file-exists-p (expand-file-name "old.txt" root)))
      (should (file-exists-p (expand-file-name "new.txt" root)))
      (emacs-agent-editor--changeset-rollback
       `((changeset_id . ,move-id)) context)
      (should (file-exists-p (expand-file-name "old.txt" root)))
      (should-not (file-exists-p (expand-file-name "new.txt" root)))
      (let* ((restored-revision
              (alist-get
               'revision
               (emacs-agent-editor--document-read
                '((path . "old.txt")) context)))
             (deleted
              (emacs-agent-editor--document-delete
               `((path . "old.txt")
                 (expected_revision . ,restored-revision))
               context))
             (delete-id (alist-get 'changeset_id deleted)))
        (should-not (file-exists-p (expand-file-name "old.txt" root)))
        (emacs-agent-editor--changeset-rollback
         `((changeset_id . ,delete-id)) context)
        (should (file-exists-p (expand-file-name "old.txt" root)))))))

(ert-deftest emacs-agent-editor-manual-create-checkpoints-with-changeset ()
  (emacs-agent-editor-test--with-server
    (let* ((context
            (emacs-agent-request-create
             :id 3 :protocol-version "2026-07-28"
             :client-info '((name . "ert"))))
           (workspace emacs-agent-editor--workspace)
           (root (emacs-agent-workspace-root workspace)))
      (setf (emacs-agent-workspace-save-policy workspace) 'manual)
      (emacs-agent-editor--document-create
       '((path . "manual.txt") (content . "buffer only\n")) context)
      (should-not (file-exists-p (expand-file-name "manual.txt" root)))
      (let* ((revision
              (alist-get
               'revision
               (emacs-agent-editor--document-read
                '((path . "manual.txt")) context)))
             (result
              (emacs-agent-editor--workspace-checkpoint
               `((documents
                  . (((path . "manual.txt")
                      (expected_revision . ,revision)))))
               context)))
        (should (stringp (alist-get 'changeset_id result)))
        (should (file-exists-p (expand-file-name "manual.txt" root)))))))

(ert-deftest emacs-agent-editor-move-reconciles-external-change ()
  (emacs-agent-editor-test--with-server
    (let* ((context
            (emacs-agent-request-create
             :id 4 :protocol-version "2026-07-28"
             :client-info '((name . "ert"))))
           (_created
            (emacs-agent-editor--document-create
             '((path . "old.txt") (content . "original\n")) context))
           (revision
            (alist-get
             'revision
             (emacs-agent-editor--document-read
              '((path . "old.txt")) context)))
           (root (emacs-agent-workspace-root
                  emacs-agent-editor--workspace)))
      (write-region "external\n" nil (expand-file-name "old.txt" root))
      (should-error
       (emacs-agent-editor--document-move
        `((path . "old.txt") (new_path . "new.txt")
          (expected_revision . ,revision))
        context)
       :type 'emacs-agent-tool-error)
      (should (file-exists-p (expand-file-name "old.txt" root)))
      (should-not (file-exists-p (expand-file-name "new.txt" root))))))

(ert-deftest emacs-agent-editor-rejected-binary-edit-does-not-degrade ()
  (emacs-agent-editor-test--with-server
    (let* ((context
            (emacs-agent-request-create
             :id 5 :protocol-version "2026-07-28"
             :client-info '((name . "ert"))))
           (_created
            (emacs-agent-editor--document-create
             '((path . "safe.txt") (content . "safe\n")) context))
           (document
            (emacs-agent-document-open
             emacs-agent-editor--workspace "safe.txt"))
           (revision (emacs-agent-document-revision document)))
      (should-error
       (emacs-agent-editor--document-apply-edits
        `((path . "safe.txt")
          (expected_revision . ,revision)
          (edits
           . (((start . ((line . 1) (column . 0)))
               (end . ((line . 1) (column . 0)))
               (new_text . ,(string 0))))))
        context)
       :type 'emacs-agent-tool-error)
      (should-not (emacs-agent-document-degraded document))
      (should
       (eq (emacs-agent-workspace-health-state
            emacs-agent-editor--workspace)
           'healthy)))))

(ert-deftest emacs-agent-editor-move-save-failure-degrades-document ()
  (emacs-agent-editor-test--with-server
    (let* ((context
            (emacs-agent-request-create
             :id 6 :protocol-version "2026-07-28"
             :client-info '((name . "ert"))))
           (_created
            (emacs-agent-editor--document-create
             '((path . "old.txt") (content . "safe\n")) context))
           (document
            (emacs-agent-document-open
             emacs-agent-editor--workspace "old.txt"))
           (revision (emacs-agent-document-revision document)))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest _) (error "test save failure"))))
        (should-error
         (emacs-agent-editor--document-move
          `((path . "old.txt") (new_path . "new.txt")
            (expected_revision . ,revision))
          context)
         :type 'emacs-agent-tool-error))
      (should (emacs-agent-document-degraded document))
      (should
       (eq (emacs-agent-workspace-health-state
            emacs-agent-editor--workspace)
           'degraded)))))

(provide 'emacs-agent-editor-test)
;;; emacs-agent-editor-test.el ends here
