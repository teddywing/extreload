(ql:quickload "extreload")

(defvar extreload::*config*
    (extreload::make-config :socket-url "ws://127.0.0.1:55755/devtools/browser/4536efdf-6ddf-40b6-9a16-258a1935d866" 
                 :reload-current-tab t
                 :debug-output t
                 :extension-ids '("imcibeelfmccdpnnlemllnepgbfdbkgo"))  
  )


(replace
  "chrome-extension://imcibeelfmccdpnnlemllnepgbfdbkgo/background.bundle.js"
  ""
  :end1 20)
;chrome-extension://imcibeelfmccdpnnlemllnepgbfdbkgo/background.bundle.js

(subseq
  "chrome-extension://imcibeelfmccdpnnlemllnepgbfdbkgo/background.bundle.js"
  19
  (+ 19 32))
