;;; +company.el --- provide just enough of company for backends -*- lexical-binding: t; -*-
;;; 
;;; Commentary:
;;; some company backends attempt to pull in company preventing their use with #'cape-company-to-capf
;;; they don't have a true, deep dependency on company so we can substitute a micro-company package
;;; that does nothing but provide the minimum necessary to run these backends as capfs.
;;;
;;; Code:
(defun company-grab (regexp &optional expression limit)
  "Copied from company package.
\(company-grab REGEXP EXPRESSION LIMIT)"
  (when (looking-back regexp limit)
    (or (match-string-no-properties (or expression 0)) "")))

(defun company-begin-backend (&rest _))
(provide 'company)

(provide '+company)
;;; +company.el ends here
