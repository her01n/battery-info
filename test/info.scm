(import (hdt hdt) (battery info))

(test get-info
  (assert (list? (get-battery-info))))


