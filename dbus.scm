(define-module (dbus))

(import (ice-9 match))
(import (rnrs bytevectors))
(import (system foreign) (system foreign-library))
  
(define gio (load-foreign-library "libgio-2.0"))

(define g_quark_to_string
  (foreign-library-function gio "g_quark_to_string" #:return-type '* #:arg-types (list uint32)))

(define (gerror-message gerror)
  (pointer->string (dereference-pointer (make-pointer (+ 8 (pointer-address gerror))))))

(define (gerror-code gerror)
  (bytevector-u32-native-ref (pointer->bytevector gerror (+ 4 4 8)) 4))

(define (handle-error function-name function-proc . args)
  (define *gerror (bytevector->pointer (make-bytevector 8 0)))
  (define result (apply function-proc (append args (list *gerror))))
  (define gerror (dereference-pointer *gerror))
  (if (not (null-pointer? gerror))
    (error function-name (gerror-message gerror) (gerror-code gerror)))
  result)

(define g_variant_classify
  (foreign-library-function gio "g_variant_classify" #:return-type int #:arg-types (list '*)))

(define g_variant_get_boolean
  (foreign-library-function gio "g_variant_get_boolean" #:return-type int #:arg-types (list '*)))

(define g_variant_get_byte
  (foreign-library-function gio "g_variant_get_byte" #:return-type uint8 #:arg-types (list '*)))

(define g_variant_get_child_value
  (foreign-library-function gio "g_variant_get_child_value" #:return-type '* #:arg-types (list '* int)))

(define g_variant_get_double
  (foreign-library-function gio "g_variant_get_double" #:return-type double #:arg-types (list '*)))

(define g_variant_get_handle
  (foreign-library-function gio "g_variant_get_handle" #:return-type uint32 #:arg-types (list '*)))

(define g_variant_get_maybe
  (foreign-library-function gio "g_variant_get_maybe" #:return-type '* #:arg-types (list '*)))

(define g_variant_get_string
  (foreign-library-function gio "g_variant_get_string" #:return-type '* #:arg-types (list '* '*)))

(define g_variant_n_children
  (foreign-library-function gio "g_variant_n_children" #:return-type int #:arg-types (list '*)))

(define g_variant_get_int16
  (foreign-library-function gio "g_variant_get_int16" #:return-type int16 #:arg-types (list '*)))

(define g_variant_get_int32
  (foreign-library-function gio "g_variant_get_int32" #:return-type int32 #:arg-types (list '*)))

(define g_variant_get_int64
  (foreign-library-function gio "g_variant_get_int64" #:return-type int64 #:arg-types (list '*)))

(define g_variant_get_uint16
  (foreign-library-function gio "g_variant_get_uint16" #:return-type uint16 #:arg-types (list '*)))

(define g_variant_get_uint32
  (foreign-library-function gio "g_variant_get_uint32" #:return-type uint32 #:arg-types (list '*)))

(define g_variant_get_uint64
  (foreign-library-function gio "g_variant_get_uint64" #:return-type uint64 #:arg-types (list '*)))

(define (variant->scm variant)
  (define (variant->list)
    (map
      (lambda (i) (variant->scm (g_variant_get_child_value variant i)))
      (iota (g_variant_n_children variant))))  
  (define (maybe)
    (define value (g_variant_get_maybe variant))
    (if (not (null-pointer? value)) (variant->scm value) #f))
  (match (string (integer->char (g_variant_classify variant)))
    ("a" (variant->list)) ; array
    ("b" (not (equal? 0 (g_variant_get_boolean variant)))) ; boolean
    ("d" (g_variant_get_double variant)) ; double
    ("h" (g_variant_get_handle variant)) ; handle
    ("i" (g_variant_get_int32 variant)) ; int32
    ("m" (maybe)) ; maybe
    ("n" (g_variant_get_int16 variant)) ; int16
    ("g" (pointer->string (g_variant_get_string variant %null-pointer))) ; signature
    ("o" (pointer->string (g_variant_get_string variant %null-pointer))) ; object-path
    ("q" (g_variant_get_uint16 variant)) ; uint16
    ("s" (pointer->string (g_variant_get_string variant %null-pointer))) ; string
    ("t" (g_variant_get_uint64 variant)) ; uint64
    ("u" (g_variant_get_uint32 variant)) ; uint32
    ("v" (variant->scm (g_variant_get_child_value variant 0))) ; variant
    ("x" (g_variant_get_int64 variant)) ; int64
    ("y" (g_variant_get_byte variant)) ; byte
    ("(" (variant->list)))) ; tuple ")"

(define g_variant_new_array
  (foreign-library-function gio "g_variant_new_array" #:return-type '* #:arg-types (list '* '* int)))

(define g_variant_new_int64
  (foreign-library-function gio "g_variant_new_int64" #:return-type '* #:arg-types (list int64)))

(define g_variant_new_string
  (foreign-library-function gio "g_variant_new_string" #:return-type '* #:arg-types (list '*)))

(define g_variant_new_tuple
  (foreign-library-function gio "g_variant_new_tuple" #:return-type '* #:arg-types (list '* int)))
  
(define g_variant_type_new
  (foreign-library-function gio "g_variant_type_new" #:return-type '* #:arg-types (list '*)))

(define (scm->variant value)
  (cond
    ((string? value) (g_variant_new_string (string->pointer value)))
    ((integer? value) (g_variant_new_int64 value))
    ((null? value) (g_variant_new_array (g_variant_type_new (string->pointer "s")) %null-pointer 0))))

(define (pack pointers)
  (define array (make-bytevector (* 8 (length pointers))))
  (for-each
    (lambda (i pointer)
      (bytevector-s64-native-set! array (* 8 i) (pointer-address pointer)))
    (iota (length pointers)) pointers)
  (bytevector->pointer array))

(define (list->tuple values)
  (g_variant_new_tuple (pack (map scm->variant values)) (length values)))

(define g_bus_get_sync
  (foreign-library-function
    gio "g_bus_get_sync"
    #:return-type '*
    #:arg-types (list int '* '*)))

(define g_dbus_connection_call_sync
  (foreign-library-function
    gio "g_dbus_connection_call_sync"
    #:return-type '*
    #:arg-types (list '* '* '* '* '* '* '* int int '* '*)))

(define-public (dbus-call bus bus-name object-path interface-name method-name . args)
  (define bus-type
    (match bus
      ('system 1)
      ('session 2)))
  (define connection (handle-error "g_bus_get_sync" g_bus_get_sync bus-type %null-pointer))
  (define parameters (list->tuple args))
  (define variant
    (handle-error "g_dbus_connection_call_sync" g_dbus_connection_call_sync connection
      (string->pointer bus-name) (string->pointer object-path) (string->pointer interface-name)
      (string->pointer method-name) parameters %null-pointer 0 10000 %null-pointer))
  (match (variant->scm variant)
    (() #f)
    ((single) single)
    (tuple tuple)))

