(import (battery info) (hdt hdt))

(test all-upower-info
  (define upower (all-upower-info))
  (assert (string-contains upower "DisplayDevice")))
  
