(use-modules
  (ice-9 match) (ice-9 threads))

(use-modules (g-golf))
(gi-import "Adw")
(gi-import "Gtk")

(use-modules (hdt hdt))

(use-modules (battery info) (gtk))

(define (test-battery-info info)
  (define get-info (if (procedure? info) info (lambda () info)))
  (define app (battery-info-app get-info))
  (define thread (begin-thread (run app '())))
  (hook
    (define window (get-active-window app))
    (if window (close window))
    (join-thread thread))
  (sleep 1)
  app)

(test no-battery
  (define app (test-battery-info #f))
  (assert (string-contains (text app) "No battery")))

(test model
  (define app
    (test-battery-info
      '((vendor . "ACME") (model . "VTX-3000") (energy-full-design . 99))))
  (assert (string-contains (text app) "ACME"))
  (assert (string-contains (text app) "VTX-3000"))
  (assert (string-contains (text app) "99 Wh")))

(test technology
  (define app (test-battery-info '((technology . 2))))
  (assert (string-contains (text app) "Li"))
  (assert (string-contains (text app) "po")))

(test capacity
  (define app
    (test-battery-info '((energy-full-design . 50) (energy-full . 48) (capacity . 96))))
  (assert (string-contains (text app) "48 Wh"))
  (assert (string-contains (text app) "96%")))

(test error
  (define app (test-battery-info (lambda () (error "test error"))))
  (sleep 1)
  (assert (string-contains (text app) "Error")))

(test copy
  (define app (test-battery-info '((vendor . "ACME") (energy-full-design . 99))))
  (click app "Copy to clipboard")
  (sleep 1)
  (define clipboard (get-clipboard (get-display (get-active-window app))))
  (define copied-text (get-value (get-content clipboard)))
  (assert (string-contains copied-text "ACME"))
  (assert (string-contains copied-text "99 Wh")))

(test spinner
  (define app (test-battery-info (lambda () (sleep 2) '((vendor . "ACME")))))
  (define window (get-active-window app))
  (assert window)
  (assert (find-child window (lambda (widget) (is-a? widget <gtk-spinner>))))
  (sleep 2)
  (assert (not (find-child window (lambda (widget) (is-a? widget <gtk-spinner>)))))
  (assert (string-contains (text window) "ACME")))

