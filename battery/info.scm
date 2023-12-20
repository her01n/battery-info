(define-module (battery info))

(use-modules
  (ice-9 exceptions) (ice-9 match) (ice-9 textual-ports)
  (sxml simple)
  (srfi srfi-2))

(use-modules (g-golf))

(use-modules (dbus) (gtk))

(g-irepository-require "Gtk" #:version "4.0")

(gi-import-by-name "Gdk" "Clipboard")
(gi-import-by-name "Gdk" "Display")
(gi-import-by-name "Gio" "Application")
(gi-import-by-name "Gio" "DBusProxy")
(gi-import-by-name "Gtk" "Box")
(gi-import-by-name "Gtk" "Button")
(gi-import-by-name "Gtk" "Label")
(gi-import-by-name "Gtk" "ScrolledWindow")
(gi-import-by-name "Gtk" "Spinner")
(gi-import-by-name "Adw" "AboutWindow")
(gi-import-by-name "Adw" "HeaderBar")
(gi-import-by-name "Adw" "Application")
(gi-import-by-name "Adw" "ApplicationWindow")
(gi-import-by-name "Adw" "StatusPage")
(gi-import-by-name "Adw" "ToastOverlay")

(bindtextdomain "battery-info" "locale")
(textdomain "battery-info")

(define (vertical-box . children)
  (define box (make <gtk-box> #:orientation 'vertical))
  (for-each (lambda (child) (append box child)) children)
  box)

(define* (h1 title #:key selectable)
  (make <gtk-label> #:css-classes (list "title-1") #:margin-top 10 #:margin-bottom 10
    #:label title #:selectable selectable))

(define (loading)
  (define spinner (make <gtk-spinner> #:margin-top 40 #:margin-bottom 40
    #:width-request 40 #:height-request 40))
  (start spinner)
  (vertical-box spinner (h1 (gettext "Loading"))))

(define (no-battery)
  (make <adw-status-page>
    #:icon-name "system-search-symbolic"
    #:title (gettext "No battery detected.")))

; TODO this should be exported by g-golf
(define (markup-escape-text text)
  ; just do the xml escape
  (with-output-to-string (lambda () (sxml->xml text))))

(define (info-error exception)
  (make <adw-status-page>
    #:icon-name "dialog-error-symbolic"
    #:title (gettext "Error reading battery info.")
    #:description (markup-escape-text (exception-message exception))))

(define (info-line description value)
  (define label (make <gtk-label> #:selectable #t))
  (set-markup label (format #f "~a <b>~a</b>" description value))
  label)

(define (vertical-glue) (make <gtk-label> #:valign 'fill #:vexpand #t))

(define (join null a b)
  (define s
    (filter (lambda (value) (and value (not (equal? "" value)))) (list a b)))
  (if (null? s) null (string-join s)))

(define (capacity->string value)
  (match value
    ((and (? real?) (not 0.0))
     (format #f "~a Wh" (/ (round (* value 100)) 100)))
    (else (gettext "Unknown"))))

(define (layout info)
  (vertical-box
    (h1
      (join (gettext "Unknown Battery") (assoc-ref info 'vendor) (assoc-ref info 'model))
      #:selectable #t)
    (info-line (gettext "System")
      (join (gettext "Unknown") (assoc-ref info 'sys-vendor) (assoc-ref info 'sys-model)))
    (info-line (gettext "Technology")
      (match (assoc-ref info 'technology)
        (1 (gettext "Lithium ion"))
        (2 (gettext "Lithium polymer"))
        (3 (gettext "Lithium iron phosphate"))
        (4 (gettext "Lead acid"))
        (5 (gettext "Nickel cadmium"))
        (6 (gettext "Nickel metal hydride"))
        (else (gettext "Unknown"))))
    (info-line (gettext "Nominal capacity")
      (capacity->string (assoc-ref info 'energy-full-design)))
    (info-line (gettext "Actual capacity")
      (capacity->string (assoc-ref info 'energy-full)))
    (info-line (gettext "Capacity percentage")
      (match (assoc-ref info 'capacity)
        ((and (? real?) (not 0.0) capacity)
         (format #f "~a%" (inexact->exact (round capacity))))
        (else (gettext "Unknown"))))))

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
  (define view (apply vertical-box (map layout info)))
  (define scrolled
    (make <gtk-scrolled-window> #:propagate-natural-height #t #:width-request 120 #:height-request 100
      #:overlay-scrolling #f #:child view))
  (define copy-button
    (make <gtk-button> #:label (gettext "Copy") #:margin-top 20 #:halign 'center))
  (define toast-overlay (make <adw-toast-overlay> #:child scrolled))
  (define previous-toast #f)
  (connect copy-button 'clicked
    (lambda (b)
      (copy-to-clipboard (text view))
      (if previous-toast (dismiss previous-toast))
      (let ((toast (make <adw-toast> #:title (gettext "Info copied to clipboard."))))
        (add-toast toast-overlay toast)
        (if previous-toast (unref previous-toast))
        (set! previous-toast (ref toast)))))
  (vertical-box toast-overlay copy-button))

(define (show-about main-window)
  (define about-window
    (make <adw-about-window>
      #:application-icon "com.her01n.BatteryInfo" #:application-name "Battery Info"
      #:copyright "© 2023 Michal Herko" #:developer-name "Michal Herko"
      #:issue-url "https://github.com/her01n/battery-info/issues"
      #:license-type 'gpl-3-0 #:version "0.1.1" #:website "https://herko.it/battery-info"
      #:transient-for main-window))
  (present about-window))

(define (content state)
  (match state
    ('loading (loading))
    ('() (no-battery))
    ((? exception? exception) (info-error exception))
    (info (battery-info info))))

(define-public battery-info-app
  (make <adw-application> #:application-id "com.her01n.BatteryInfo"))

(register battery-info-app #f)

(define (make-window container)
  (define header (make <adw-header-bar>))
  (define about-button
    (make <gtk-button> #:label (gettext "About") #:icon-name "help-about-symbolic"))
  (pack-start header about-button)
  (define window
    (make <adw-application-window> #:application battery-info-app
      #:title (gettext "Battery Info")))
  (set-default-size window 440 440)
  (set-content window (vertical-box header container (vertical-glue)))
  (connect about-button 'clicked (lambda args (show-about window)))
  window)
  
(define (make-container) (make <gtk-box> #:orientation 'vertical))

(define (update window container state)
  (and-let*
    ((child (get-first-child container)))
    (remove container child))
  (append container (content state))
  (if (not (is-visible window)) (present window)))

(define-public (show-battery-info get-info)
  (define container (make-container))
  (define window (make-window container))
  ; delay displaying the spinner a little,
  ; so it does not blink if the loading is fast
  (g-timeout-add 200
    (lambda () (if (not (is-visible window)) (update window container 'loading)) #f))
  (work
    (if (procedure? get-info) get-info (lambda () get-info))
    (lambda (result)
      (update window container result))
    (lambda args
      (update window container
        (make-exception-with-message
          (match args
            ((key function (? string? message-format) (? list? message-args) values)
             (format #f "~a ~a: ~a" key function (apply format #f message-format message-args)))
            (else (format #f "~S" args)))))))
  window)

(define (device-property device property)
  (dbus-call
    'system "org.freedesktop.UPower" device "org.freedesktop.DBus.Properties"
    "Get" "org.freedesktop.UPower.Device" property))

(define-public (get-upower-info)
  (define device-names
    (dbus-call
      'system "org.freedesktop.UPower" "/org/freedesktop/UPower" "org.freedesktop.UPower" "EnumerateDevices"))
  (filter identity
    (map
      (lambda (device)
        (and
          (equal? (device-property device "Type") 2)
          (device-property device "PowerSupply")
          `((vendor . ,(device-property device "Vendor"))
            (model . ,(device-property device "Model"))
            (energy-full-design . ,(device-property device "EnergyFullDesign"))
            (technology . ,(device-property device "Technology"))
            (energy-full . ,(device-property device "EnergyFull"))
            (capacity . ,(device-property device "Capacity")))))
      device-names)))

(define (read-dmi key)
  (define string
    (call-with-input-file
      (string-append "/sys/devices/virtual/dmi/id/" key)
      (@ (ice-9 textual-ports) get-line)))
  (match string
    ("" #f)
    ("Default String" #f)
    ("To Be Filled By O.E.M." #f)
    (value value)))

(define (get-dmi-info)
  (with-exception-handler
    (lambda (exception)
      (format #t "Failed to get dmi info: ~a\n" (exception-message exception))
      '())
    (lambda ()
      `((sys-vendor . ,(read-dmi "sys_vendor"))
        (sys-model . ,(read-dmi "product_name"))))
    #:unwind? #t))

(define-public (get-battery-info)
  (define dmi-info (get-dmi-info))
  (map
    (lambda (upower-info) (append upower-info dmi-info))
    (get-upower-info)))

(define-public (main args)
  (connect battery-info-app 'activate
    (lambda (app)
      (show-battery-info get-battery-info)))
  (run battery-info-app args))

