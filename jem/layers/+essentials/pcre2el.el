;; pcre2el-boot.el

(defun jem-essentials-pcre2el|init ()
  (use-package pcre2el
    :ensure t
    :defer t
    :init
    (jem-declare-prefix "xr" "regular expressions")
    (jem-declare-prefix "xre" "elisp")
    (jem-declare-prefix "xrp" "pcre")
    (jem-set-leader-keys
      "tr/" 'rxt-explain
      "tr'" 'rxt-convert-to-strings
      "trt" 'rxt-toggle-elisp-rx
      "trx" 'rxt-convert-to-rx
      "trc" 'rxt-convert-syntax
      "tre/" 'rxt-explain-elisp
      "tre'" 'rxt-elisp-to-strings
      "trep" 'rxt-elisp-to-pcre
      "tret" 'rxt-toggle-elisp-rx
      "trex" 'rxt-elisp-to-rx
      "trp/" 'rxt-explain-pcre
      "trp'" 'rxt-pcre-to-strings
      "trpe" 'rxt-pcre-to-elisp
      "trpx" 'rxt-pcre-to-rx)))
