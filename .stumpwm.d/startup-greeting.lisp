(setf hour (run-shell-command "date +%H" t))
(setq hour (parse-integer hour))
(setf message (if (< hour 12)
                "Good morning, Daniel"
                (if (< hour 17)
                  "Good afternoon, Daniel"
                  "Good evening, Daniel")))
(setf *startup-message* message)
