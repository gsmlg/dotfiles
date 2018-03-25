;;; Basic ruby setup
(use-package ruby-mode
  :ensure t
  :init (setq-default
         ruby-use-encoding-map nil
         ruby-insert-encoding-magic-comment nil)
  :config
  (progn
    (add-auto-mode 'ruby-mode
                   "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
                   "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
                   "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'")
    ))

(use-package ruby-hash-syntax
  :ensure t)

(add-auto-mode 'conf-mode "Gemfile\\.lock\\'")


(provide 'init-ruby)
