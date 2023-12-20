(use-modules
  (ice-9 match) (ice-9 threads))

(use-modules (g-golf))
(gi-import "Adw")
(gi-import "Gtk")

(use-modules (hdt hdt))

(use-modules (battery info) (gtk))

(define (test-battery-info get-info)
  (define window (show-battery-info get-info))
  (hook (close window))
  (gtk-timed-loop 1)
  window)

(test no-battery
  (define window (test-battery-info '()))
  (assert (string-contains (text window) "No battery")))

(test model
  (define window
    (test-battery-info
      '(((vendor . "ACME") (model . "VTX-3000") (energy-full-design . 99)))))
  (assert (string-contains (text window) "ACME"))
  (assert (string-contains (text window) "VTX-3000"))
  (assert (string-contains (text window) "99 Wh")))

(test technology
  (define window (test-battery-info '(((technology . 2)))))
  (assert (string-contains (text window) "Li"))
  (assert (string-contains (text window) "po")))

(test capacity
  (define window
    (test-battery-info '(((energy-full-design . 50) (energy-full . 48) (capacity . 96.4)))))
  (assert (string-contains (text window) "48 Wh"))
  (assert (string-contains (text window) "96%")))

(test error
  (define window (test-battery-info (lambda () (error "test error"))))
  (assert (string-contains (text window) "Error")))

(test copy
  (define window (test-battery-info '(((vendor . "ACME") (energy-full-design . 99)))))
  (click window "Copy")
  (gtk-timed-loop 1)
  (define clipboard (get-clipboard (get-display window)))
  (define copied-text (get-value (get-content clipboard)))
  (assert (string-contains copied-text "ACME"))
  (assert (string-contains copied-text "99 Wh")))

(test spinner
  (define window (test-battery-info (lambda () (sleep 2) '(((vendor . "ACME"))))))
  (assert window)
  (assert (find-child window (lambda (widget) (is-a? widget <gtk-spinner>))))
  (gtk-timed-loop 2)
  (assert (not (find-child window (lambda (widget) (is-a? widget <gtk-spinner>)))))
  (assert (string-contains (text window) "ACME")))

(test rounding
  (define window (test-battery-info '(((energy-full-design . 56.160000000000004)))))
  (assert (string-contains (text window) "56.16 Wh")))

(test computer-vendor-model
  (define window
    (test-battery-info '(((sys-vendor . "ACME") (sys-model . "ASDF-1")))))
  (assert (string-contains (text window) "ACME ASDF-1")))

(test more-batteries
  (define window
    (test-battery-info '(((model . "battery-one")) ((model . "battery-two")))))
  (assert (string-contains (text window) "battery-two")))

