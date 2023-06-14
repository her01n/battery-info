(use-modules (g-golf))

(gi-import "Gtk")
(gi-import "Adw")

(gtk-init)

(define window (make <adw-window>))
(set-content window (make <adw-status-page> #:title "loading"))
(set-default-size window 400 400)
(present window)

(define label (make <gtk-label> #:label "ahoj" #:selectable #t))

(g-timeout-add 1000
  (lambda ()
    ; This does not work correctly - the text "ahoj" is selected
    #;(set-content window label)
    ; workaround, first set content to nothing
    (set-content window #f)
    ; and we need to set the correct content later,
    ; setting it immediately does not work
    (g-idle-add (lambda () (set-content window label) #f))
    #f))


(g-main-loop-run (g-main-loop-new))
