(use-modules (ice-9 match))

(use-modules (g-golf))
(gi-import "Adw")
(gi-import "Gtk")

(use-modules (hdt hdt))

(use-modules (battery info))

(define (get-children component)
  (define (siblings component)
    (if component (cons component (siblings (get-next-sibling component))) (list)))
  (siblings (get-first-child component)))

(define (click component text)
  (cond
    ((is-a? component <adw-application-window>) (click (get-content component) text))
    ((is-a? component <gtk-box>) (find (lambda (child) (click child text)) (get-children component)))
    ((is-a? component <gtk-window>) (click (get-child component) text))
    ((is-a? component <gtk-button>)
     (and
       (string-contains (get-label component) text)
       (begin (activate component) #t)))
    (else #f)))

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

(test capacity
  (test-battery-info '((energy-full-design . 50) (energy-full . 48) (capacity . 96)))
  (assert (string-contains (battery-info-text) "48 Wh"))
  (assert (string-contains (battery-info-text) "96%")))

(test error
  (hook (close-battery-info))
  (show-battery-info (lambda () (error "test error")))
  (sleep 1)
  (assert (string-contains (battery-info-text) "Error")))

(test copy
  (test-battery-info '((vendor . "ACME") (energy-full-design . 99)))
  (define window (battery-info-window))
  (assert (click window "Copy to clipboard"))
  (sleep 1)
  (define clipboard (get-clipboard (get-display window)))
  (define copied-text (get-value (get-content clipboard)))
  (assert (string-contains copied-text "ACME"))
  (assert (string-contains copied-text "99 Wh")))


