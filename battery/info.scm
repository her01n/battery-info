(define-module (battery info))

(use-modules
  (ice-9 atomic) (ice-9 exceptions) (ice-9 threads)
  (sxml simple) (sxml xpath))

(use-modules (g-golf))

(use-modules (gtk))

(g-irepository-require "Gtk" #:version "4.0")

(gi-import-by-name "Gdk" "Clipboard")
(gi-import-by-name "Gdk" "Display")
(gi-import-by-name "Gio" "DBusProxy")
(gi-import-by-name "Gtk" "Box")
(gi-import-by-name "Gtk" "Button")
(gi-import-by-name "Gtk" "Label")
(gi-import-by-name "Gtk" "Spinner")
(gi-import-by-name "Adw" "HeaderBar")
(gi-import-by-name "Adw" "Application")
(gi-import-by-name "Adw" "ApplicationWindow")
(gi-import-by-name "Adw" "StatusPage")
(gi-import-by-name "Adw" "ToastOverlay")

(define (vertical-box . children)
  (define box (make <gtk-box> #:orientation 'vertical))
  (for-each (lambda (child) (append box child)) children)
  box)

(define* (h1 title #:key selectable)
  (make <gtk-label> #:css-classes (list "title-1") #:margin-bottom 5
    #:label title #:selectable selectable))

(define (loading)
  (define spinner (make <gtk-spinner> #:margin-top 40 #:margin-bottom 40
    #:width-request 40 #:height-request 40))
  (start spinner)
  (vertical-box spinner (h1 "Loading")))

(define (no-battery)
  (make <adw-status-page>
    #:icon-name "system-search-symbolic"
    #:title "No battery detected."))

(define (info-error exception)
  (make <adw-status-page>
    #:icon-name "dialog-error-symbolic"
    #:title "Error reading battery info"
    #:description (exception-message exception)))

(define (info-line description value)
  (define label (make <gtk-label> #:selectable #t))
  (set-markup label (format #f "~a <b>~a</b>" description value))
  label)

(define (title info)
  (define vendor (or (assoc-ref info 'vendor) ""))
  (define model (or (assoc-ref info 'model) ""))
  (if (or (not (equal? "" vendor)) (not (equal? "" model)))
    (format #f "~a ~a" vendor model)
    "Unknown Battery"))

(define (layout info)
  (vertical-box
    (h1 (title info) #:selectable #t)
    (info-line "Technology"
      (match (assoc-ref info 'technology)
        (1 "Lithium ion")
        (2 "Lithium polymer")
        (3 "Lithium iron phosphate")
        (4 "Lead acid")
        (5 "Nickel cadmium")
        (6 "Nickel metal hydride")
        (else "Unknown")))
    (info-line "Nominal capacity"
      (match (assoc-ref info 'energy-full-design)
        ((and (? real?) (not 0.0) energy-full-design)
         (format #f "~a Wh" energy-full-design))
        (else "Unknown")))
    (info-line "Actual capacity"
      (match (assoc-ref info 'energy-full)
        ((and (? real?) (not 0.0) energy-full) (format #f "~a Wh" energy-full))
        (else "Unknown")))
    (info-line "Capacity percentage"
      (match (assoc-ref info 'capacity)
        ((and (? real?) (not 0.0) capacity) (format #f "~a%" (round capacity)))
        (else "Unknown")))))

(define (copy-to-clipboard text)
  (define clipboard (get-clipboard (gdk-display-get-default)))
  (set clipboard text))

(define-syntax define-traced
  (syntax-rules ()
    ((define-traced (name ...) exp ...)
     (define (name ...)
       (format #t "trace ~a\n" (list name ...))
       ((lambda () exp ...))))))

(define (battery-info info)
  (define view (layout info))
  (define copy-button (make <gtk-button> #:label "Copy to clipboard" #:margin-top 15 #:halign 'center))
  (define toast-overlay (make <adw-toast-overlay> #:child view))
  (define previous-toast #f)
  (connect copy-button 'clicked
    (lambda (b)
      (copy-to-clipboard (text view))
      (if previous-toast (dismiss previous-toast))
      (let ((toast (make <adw-toast> #:title "Info copied to clipboard.")))
        (add-toast toast-overlay toast)
        (if previous-toast (unref previous-toast))
        (set! previous-toast (ref toast)))))
  (vertical-box toast-overlay copy-button))

(define (window-content state)
  (define content
    (match (atomic-box-ref state)
      ('loading (loading))
      ('no-battery (no-battery))
      ((? exception? exception) (info-error exception))
      (info (battery-info info))))
  (define bordered
    (make <gtk-box> #:orientation 'vertical #:margin-top 10 #:margin-bottom 10
      #:margin-left 10 #:margin-right 10))
  (append bordered content)
  (vertical-box (make <adw-header-bar>) bordered))

(define (present-window app)
  (define window
    (make <adw-application-window> #:application app #:title "Battery Info"))
  (set-default-size window 400 400)
  (present window))

(define (update app state)
  (define window (get-active-window app))
  (define content (window-content state))
  ; workaround to avoid selecting the battery name
  (set-content window #f)
  (g-idle-add (lambda () (set-content window content) #f)))

(define (load app state get-info)
  (with-exception-handler
    (lambda (exception) (atomic-box-set! state exception))
    (lambda () (atomic-box-set! state (or (get-info) 'no-battery)))
    #:unwind? #t)
  (g-idle-add (lambda () (update app state) #f)))

(define-public (battery-info-app get-info)
  (define state (make-atomic-box 'loading))
  (define app (make <adw-application> #:application-id "com.her01n.Battery-Info"))
  (connect app 'activate
    (lambda (app)
      (present-window app)
      ; delay displaying the spinner a little,
      ; so it does not blink if the loading is fast
      (g-timeout-add 200 (lambda () (update app state) #f))
      (begin-thread (load app state get-info))))
  app)

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

(define* (run-battery-info #:optional (get-info get-battery-info) #:key (args '()))
  (run (battery-info-app get-info) args))

(export run-battery-info)

(define-public (main args) (run-battery-info #:args args))

