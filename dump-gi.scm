(use-modules (g-golf))

(map
  (lambda (repository)
    (gi-import repository)
    (with-output-to-file
      (string-append repository ".txt")
      (lambda ()
        (let ((n (g-irepository-get-n-infos repository)))
          (map
            (lambda (i)
              (let ((info (g-irepository-get-info repository i)))
                (format #t
                  "~a, ~a\n"
                  (g-base-info-get-type info)
                  (g-base-info-get-name info))))
            (iota n))))))
  (list "GLib" "Gio" "Gtk" "Adw"))

