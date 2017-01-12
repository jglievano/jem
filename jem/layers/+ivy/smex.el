;; swiper-boot.el

(defun jem-ivy-swiper|init ()
  (use-package swiper
    :ensure t
    :config
    (jem-set-leader-keys
      "ss" 'swiper
      "sS" 'jem-swiper-region-or-symbol
      "sb" 'swiper-all
      "sB" 'jem-swiper-all-region-or-symbol)
    (global-set-key "\C-s" 'swiper)))
