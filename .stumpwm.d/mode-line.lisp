;; Mode line
(defun format-battery-string (ml)
  "Format the battery string (colorized output for low battery)"
  (setf battery_string (string-trim
                         '(#\Newline)
                         (stumpwm:run-shell-command
                           "cat /sys/class/power_supply/BAT0/capacity"
                           t)))
  (setq battery_num (parse-integer battery_string))

  (setf color (if (> battery_num 20) "" (if (> battery_num 10) "^5*" "^1*")))
  (concatenate 'string color battery_string "% "))
(add-screen-mode-line-formatter #\b #'format-battery-string)

(defun alarm-message (ml) 
  "Sets the alarm message to display either as the dlarm `message` or an empty string"
  (setf alarm-time
        (run-shell-command
          "grep -E \"^alarm-time=\" \"/home/dsock/.dlarm.rc\" | head -n 1 | cut -d '=' -f 2"
          t))
  (setq alarm-time (parse-integer alarm-time))

  (setq current-time (parse-integer (run-shell-command "date +%s" t)))

  (if (and
        (< alarm-time current-time)
        (= 0 (mod current-time 2)))
    (string-trim '(#\Newline)
                 (run-shell-command "grep -E \"^alarm-message=\" \"/home/dsock/.dlarm.rc\" | head -n 1 | cut -d '=' -f 2" t))
    ""))
(add-screen-mode-line-formatter #\a #'alarm-message)


(stumpwm:add-hook *key-press-hook* 'stumpwm::which-key-mode-key-press-hook)

(setf *time-modeline-string* "%a, %b %e %l:%M EST")
(setf stumpwm:*screen-mode-line-format* (list "          ^1%a" "^>" "%d" " | " "%b"))

(setf *mode-line-timeout* 1)
(stumpwm:enable-mode-line (stumpwm:current-screen) (stumpwm:current-head) T)
