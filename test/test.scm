(use-modules (g-golf))
(gi-import "Gtk")

(use-modules (hdt hdt))

(use-modules (battery info))

(define (test-battery-info info)
  (hook (close-battery-info))
  (show-battery-info (lambda () info))
  (sleep 1))

(test no-battery
  (test-battery-info #f)
  (assert (string-contains (battery-info-text) "No battery")))

(test model
  (test-battery-info
    '((vendor . "ACME") (model . "VTX-3000") (energy-full-design . 99)))
  (assert (string-contains (battery-info-text) "ACME"))
  (assert (string-contains (battery-info-text) "VTX-3000"))
  (assert (string-contains (battery-info-text) "99 Wh")))

(test technology
  (test-battery-info '((technology . 2)))
  (assert (string-contains (battery-info-text) "Li"))
  (assert (string-contains (battery-info-text) "po")))

