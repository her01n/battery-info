(define-module (battery info))

(use-modules (ice-9 threads) (sxml simple) (sxml xpath))

(use-modules (g-golf))

(gi-import "Gio")
(g-irepository-require "Gtk" #:version "4.0")
(gi-import "Gtk")
(gi-import "Adw")

(define (no-battery) (make <adw-status-page> #:title "No battery detected."))

(define (vertical-box . children)
  (define box (make <gtk-box> #:orientation 'vertical))
  (for-each (lambda (child) (append box child)) children)
  box)

(define (hx class margin)
  (lambda (title)
    (make <gtk-label> #:label title #:css-classes (list class) #:margin-top margin #:margin-bottom margin)))

(define h1 (hx "title-1" 5))
(define h2 (hx "title-2" 5))

(define (info-line description value)
  (define label (make <gtk-label>))
  (set-markup label (format #f "~a <b>~a</b>" description value))
  label)
  
(define (battery-info info)
  (vertical-box
    (h1
      (let ((vendor (or (assoc-ref info 'vendor) ""))
            (model (or (assoc-ref info 'model) "")))
        (if (or (not (equal? "" vendor)) (not (equal? "" model)))
          (format #f "~a ~a" vendor model)
          "Unknown Battery")))
    (info-line "Nominal capacity"
      (match (assoc-ref info 'energy-full-design)
        ((and (? real?) (not 0.0) energy-full-design)
         (format #f "~a Wh" energy-full-design))
        (else "Unknown")))
    (info-line "Technology"
      (match (assoc-ref info 'technology)
        (1 "Lithium ion")
        (2 "Lithium polymer")
        (3 "Lithium iron phosphate")
        (4 "Lead acid")
        (5 "Nickel cadmium")
        (6 "Nickel metal hydride")
        (else "Unknown")))
    (h2 "Health")
    (info-line "Actual capacity"
      (match (assoc-ref info 'energy-full)
        ((and (? real?) (not 0.0) energy-full) (format #f "~a Wh" energy-full))
        (else "Unknown")))
    (info-line "Capacity percentage"
      (match (assoc-ref info 'capacity)
        ((and (? real?) (not 0.0) capacity) (format #f "~a%" (round capacity)))
        (else "Unknown"))))) 

(define (present-window app info)
  (define window (make <gtk-application-window> #:application app #:title "Battery Info"))
  (define content (if info (battery-info info) (no-battery)))
  (define wrapper (make <gtk-box> #:orientation 'vertical #:margin-top 10 #:margin-bottom 10 #:margin-left 10 #:margin-right 10))
  (append wrapper content)
  (set-child window wrapper)
  (set-default-size window 400 400)
  (present window))

(define app #f)

(define-public (show-battery-info get-info)
  (set! app (make <gtk-application> #:application-id "com.her01n.Battery-Info"))
  (connect app 'activate (lambda (app) (present-window app (get-info))))
  (begin-thread (run app '())))

(define-public (close-battery-info)
  (and app (get-active-window app) (gtk-window-close (get-active-window app)))
  (sleep 1))

(define (get-children component)
  (define (siblings component)
    (if component (cons component (siblings (get-next-sibling component))) (list)))
  (siblings (get-first-child component)))

(define (text component)
  (cond
    ((is-a? component <gtk-box>) (string-join (map text (get-children component)) "\n"))
    ((is-a? component <gtk-window>) (text (get-child component)))
    ((is-a? component <gtk-label>) (get-text component))
    ((is-a? component <adw-status-page>) (get-title component))
    (else (format #f "~a" component))))

(define-public (battery-info-text)
  (text (get-active-window app)))

(gi-import "Gio")

(define (g-variant->scm variant)
  (define (g-variant->list)
    (map
      (lambda (i) (g-variant->scm (g-variant-get-child-value variant i)))
      (iota (g-variant-n-children variant))))  
  (match (g-variant-classify variant)
    ('boolean (g-variant-get-boolean variant))
    ('uint32 (g-variant-get-uint32 variant))
    ('double (g-variant-get-double variant))
    ('string (g-variant-get-string variant))
    ('object-path (g-variant-get-string variant))
    ('signature (g-variant-get-string variant))
    ('variant (g-variant->scm (g-variant-get-child-value variant 0)))
    ('array (list->vector (g-variant->list)))
    ('tuple (g-variant->list))))
       
(define timeout 1000)

(define (get-property proxy name)
  (car
    (g-variant->scm
      (call-sync
        proxy "Get"
        (g-variant-parse #f (format #f "(\"~a\", \"~a\")" "org.freedesktop.UPower.Device" name) #f #f)
        '() timeout #f))))

(define-public (get-battery-info)
  ; list objects of system org.freedesktop.UPower service
  (define upower-proxy
    (gd-bus-proxy-new-for-bus-sync
      'system '() #f "org.freedesktop.UPower" "/org/freedesktop/UPower" "org.freedesktop.UPower" #f))
  (define devices-names
    (vector->list
      (car (g-variant->scm (call-sync upower-proxy "EnumerateDevices" #f '() timeout #f)))))
  ; find a battery and query the information
  (find identity
    (map
      (lambda (device)
        (define proxy
          (gd-bus-proxy-new-for-bus-sync
            'system '() #f "org.freedesktop.UPower" device "org.freedesktop.DBus.Properties" #f))
        (and
          (equal? (get-property proxy "Type") 2)
          (get-property proxy "PowerSupply")
          `((vendor . ,(get-property proxy "Vendor"))
            (model . ,(get-property proxy "Model"))
            (energy-full-design . ,(get-property proxy "EnergyFullDesign"))
            (technology . ,(get-property proxy "Technology"))
            (energy-full . ,(get-property proxy "EnergyFull"))
            (capacity . ,(get-property proxy "Capacity")))))
      devices-names)))

(define-public (main args)
  (join-thread (show-battery-info get-battery-info)))

