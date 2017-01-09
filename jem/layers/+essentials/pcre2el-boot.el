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
      "xr/" 'rxt-explain
      "xr'" 'rxt-convert-to-strings
      "xrt" 'rxt-toggle-elisp-rx
      "xrx" 'rxt-convert-to-rx
      "xrc" 'rxt-convert-syntax
      "xre/" 'rxt-explain-elisp
      "xre'" 'rxt-elisp-to-strings
      "xrep" 'rxt-elisp-to-pcre
      "xret" 'rxt-toggle-elisp-rx
      "xrex" 'rxt-elisp-to-rx
      "xrp/" 'rxt-explain-pcre
      "xrp'" 'rxt-pcre-to-strings
      "xrpe" 'rxt-pcre-to-elisp
      "xrpx" 'rxt-pcre-to-rx)))
