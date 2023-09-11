(define-module (gtk))

(use-modules (g-golf))

(gi-import-by-name "Adw" "ApplicationWindow")
(gi-import-by-name "Adw" "StatusPage")
(gi-import-by-name "Adw" "ToastOverlay")
(gi-import-by-name "Gtk" "Application")
(gi-import-by-name "Gtk" "Box")
(gi-import-by-name "Gtk" "Label")
(gi-import-by-name "Gtk" "Window")

(define-public (children widget)
  (define (siblings widget)
    (if widget (cons widget (siblings (get-next-sibling widget))) (list)))
  (siblings (get-first-child widget)))

(define-public (text widget)
  (cond
    ((is-a? widget <gtk-application>) (text (get-active-window widget)))
    ((is-a? widget <adw-application-window>) (text (get-content widget)))
    ((is-a? widget <adw-status-page>)
     (format #f "~a ~a" (get-title widget) (get-description widget)))
    ((is-a? widget <adw-toast-overlay>) (text (get-child widget)))
    ((is-a? widget <gtk-box>) (string-join (map text (children widget)) "\n"))
    ((is-a? widget <gtk-window>) (text (get-child widget)))
    ((is-a? widget <gtk-label>) (get-text widget))
    (else (format #f "~a" widget))))

(define-public (find-child widget test)
  (define (find widgets)
    (match widgets
      ((widget . others) (or (and (test widget) widget) (find (children widget)) (find others)))
      (else #f)))
  (find (list widget)))

(define-public (click widget text)
  (define (should-click widget)
    (and (is-a? widget <gtk-button>) (get-label widget) (string-contains (get-label widget) text)))
  (if
    (is-a? widget <gtk-application>)
    (click (get-active-window widget) text)
    (activate (find-child widget should-click))))

(define-public (gtk-main)
  (when (< 0 (get-n-items (gtk-window-get-toplevels)))
    (g-main-context-iteration #nil #t)
    (gtk-main)))

(define-public (gtk-timed-loop time)
  (define timed-out #f)
  (define (loop)
    (when (not timed-out)
      (g-main-context-iteration #nil #t)
      (loop)))
  (g-timeout-add (* time 1000) (lambda () (set! timed-out #t) #f))
  (loop))

