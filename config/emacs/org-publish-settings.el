
;;;; Switches off use of time-stamps when publishing. I would prefer to publish everything every time
  (setq org-publish-use-timestamps-flag nil)
  (setq org-publish-project-alist
        '( ("org" :components ("invoices-pdf" "orgfull-html" "org-styles" "orgfull-pdf" "green-dotfiles-html"))
          ("green-dotfiles-html"
           :base-directory "~/.dotfiles"
           :publishing-directory "~/placemarks/org_published/dotfiles/html"
           :base-extension "org"
           :recursive t
           :exclude "^_[a-z]"
           :section-numbers 3
           :with-toc t
           :toc t
           :n t
           :H 6
           :auto-sitemap t
           :h 6
           :toc 6
           :sitemap-filename "sitemap.org"
           :sitemap-title "Sitemap"
           :publishing-function org-html-publish-to-html)
          ("org-styles"
           :base-directory "~/org/styles"
           :recursive t
           :base-extension "css\\|js"
           :publishing-directory "~/placemarks/org_published/"
           :publishing-function org-publish-attachment)
          ("orgfull-pdf"
           :base-directory "~/org/"
           :base-extension "org"
           :publishing-directory "~/placemarks/org_published/full/pdf"
           :section-numbers nil
           :with-toc nil
           :exclude "//^_.org$"
           :recursive t
           :publishing-function org-latex-publish-to-pdf)
          ("invoices-pdf"
           :base-directory "~/org/07-needs"
           :base-extension "org"
           :publishing-directory "~/placemarks/org_published/full/pdf/invoices"
           :section-numbers nil
           :with-toc nil
           :exclude "//^_.org$"
           :recursive t
           :publishing-function org-latex-publish-to-pdf)
          ("orgfull-html"
           :base-directory "~/org/08-system"
           :publishing-directory "~/placemarks/org_published/full/html"
           :base-extension "org"
           :recursive t
           :exclude "^_[a-z]"
           :section-numbers 3
           :with-toc t
           :toc t
           :n t
           :H 6
           :auto-sitemap t
           :h 6
           :toc 6
           :sitemap-filename "sitemap.org"
           :sitemap-title "Sitemap"
           :publishing-function org-html-publish-to-html)))
