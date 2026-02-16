(use-modules (guix)
             (guix packages)
             (gnu packages base)
             (gnu packages guile)
             (gnu packages guile-xyz))

(packages->manifest (list guile-next guile-hoot gnu-make))
