(import (srfi srfi-1))

(import (hdt hdt))
(import (dbus))

(test system-bus
  (define id (dbus-call 'system "org.freedesktop.DBus" "/org/freedesktop/DBus" "org.freedesktop.DBus" "GetId"))
  (assert (string? id))
  (assert (not (equal? "" id))))

(test session-bus
  (define server-information
    (dbus-call
      'session "org.freedesktop.Notifications" "/org/freedesktop/Notifications"
      "org.freedesktop.Notifications" "GetServerInformation"))
  (assert (list? server-information))
  (assert (equal? 4 (length server-information)))
  (assert (every string? server-information)))

; TODO would probably need to introspect to determine correct parameters type signature
#;(test pass-arguments
  (define notification
    (dbus-call
      'session "org.freedesktop.Notifications" "/org/freedesktop/Notifications"
      "org.freedesktop.Notifications" "Notify" "test dbus" 0 "" "test" "test notification, please ignore."
      (list "yes" "Yes" "no" "No") '() -1))
  (assert (number? notification)))
  

