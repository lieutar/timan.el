;;; timan.el --- 

;; Copyright (C) 2010  

;; Author:  <lieutar@TREEFROG>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

;;(add-to-list 'load-path default-directory)

;;;
;;; MODEL
;;; 

(defvar timan-file-type :mp3
  "Player type.")

(defconst timan-sound-alist
  (let ((base (file-name-directory (locate-library "timan"))))
    (apply
     'append
     (mapcar
      (lambda (info)
        (let ((cat (car info)))
          (mapcar (lambda (n)
                    (list 
                     (intern (format "%s-0%d" cat n))
                     :native
                     (expand-file-name
                      (format "sounds/%s/0%d.wav" cat n) base)
                     :mp3
                     (expand-file-name
                      (format "sounds/%s/0%d.mp3" cat n) base)
                     ))
                  (cdr info))))
      '((chaim  0 1 2 3 4 5 6)
        (animal 0 1 2)
        (notify 0 1 2 3 4)))))
  "Registered sound files.")

(defvar timan-player-alist
  '((:native . play-sound)
    (:mp3    . timan-play-mp3))
  "Player function.")

(defvar timan-mp3-player-program "mpg123"
  "")

(defvar timan-default-chaim-symbol 'chaim-00)



(defun timan-get-file (sym)
  (let ((slot (assq (or sym timan-default-chaim-symbol)
                    timan-sound-alist)))
    (and slot
         (plist-get (cdr slot) timan-file-type ))))

(eval-after-load "yatest"
  '(yatest::define-test timan timan-get-file
     (let ((timan-sound-alist '((x :native "x.wav" :mp3 "x.mp3")
                                (a :native "a.wav" :mp3 "a.mp3")))
           (timan-file-type :native))
       (yatest wav (equal "a.wav" (yatest::p "wav" (timan-get-file 'a))))
       (let ((timan-file-type :mp3))
         (yatest mp3 (equal "a.mp3" (yatest::p "mp3" (timan-get-file 'a))))))))
;;(yatest::run 'timan 'timan-get-file)



;;;
;;; Players
;;;

(defun timan-play-mp3 (&rest spec)
  (let* ((file     (plist-get spec :file))
         (callback (or (plist-get spec :callback) (lambda ())))
         (buf      (generate-new-buffer " *timan mp3*"))
         (sentinel `(lambda (&rest args)
                      (let ((sig (and (cdr args)(cadr args))))
                        (when (equal sig "finished\n")
                          (kill-buffer ,buf)
                          (funcall ,callback)
                          ))))
         (proc (start-process "timan-mp3" 
                              buf
                              timan-mp3-player-program
                              file)))
    (set-process-sentinel proc sentinel)))

;;(timan-play-mp3 :file "sounds/chaim/00.mp3")
(eval-after-load "yatest"
  '(yatest::define-test timan timan-play-mp3
     (let ((len  (length (buffer-list)))
           (hoge nil))
       (timan-play-mp3 :file "sounds/notify/00.mp3"
                       :callback (lambda () (setq hoge "hoge")))
       (yatest "buffer was increased"
               (< len (length (buffer-list))))
       ;; TODO yatest に非同期テストを実装しないとテストしきれない
       ;;        (sleep-for 4)
       ;;        (yatest "buffer was decreased"
       ;;                (= len (length (buffer-list))))
       ;;        (yatest "callback was invoked"
       ;;                (equal hoge "hoge"))
       )))
;;(yatest::run 'timan 'timan-play-mp3)


(defun timan-play-sound (&optional sym &rest opt)
  (let ((file (timan-get-file sym))
        (player (assq timan-file-type timan-player-alist)))
    (apply (cdr player) :file file opt)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; timan play
;;;

(defun timan-play-all-sounds::iter (nth len)
  (let ((sym (car (nth nth timan-sound-alist))))
    (message "%s (%d/%d)" sym (1+ nth) len)
    (timan-play-sound
     sym
     :callback
     `(lambda ()
        (if (< ,(1+ nth) ,len)
            (funcall (timan-play-all-sounds::iter
                      ,(1+ nth)
                      ,len))
          (message "Done..."))))))

;;; NOTE: The test of following function is to be manually.

(defun timan-play-all-sounds ()
  "Plays all sounds that were registered into the `timan-sounds-alist'."
  (interactive)
  (funcall (timan-play-all-sounds::iter
            0
            (length timan-sound-alist))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; timan insert
;;;

(defun timan-insert-current-time (&optional header)
  (interactive)
  (insert (or header "AT") ": " (current-time-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; kitchen timer.
;;;


(defvar timan-kitchen-timer-unit (* 60 15))
;;(setq timan-kitchen-timer-unit 1)
;;(setq timan-kitchen-timer-unit 900)

(defvar timan-kitchen-timer-tick-sound    'notify-02)
(defvar timan-kitchen-timer-tick-interval (* 60 5))

(defconst timan-kitchen-timer--all-timers (make-hash-table :test 'eq))
(defconst timan-kitchen-timer--count 0)



(defun timan-kitchen-timer-parse-timestring (timestring)
  (let ((R
         (cond
          ((string-match
            "\\([0-9][0-9]?\\)[^0-9]+\\([0-9][0-9]?\\)[^0-9]+\\([0-9][0-9]?\\)"
            timestring)
           (format "%s sec %s min %s hour"
                   (match-string 3 timestring)
                   (match-string 2 timestring)
                   (match-string 1 timestring)))
          
          ((string-match
            "\\`\\([0-9][0-9]?\\)[^0-9]+\\([0-9][0-9]?\\)[^0-9]\\'"
            timestring)
           (format "%s min %s hour"
                   (match-string 2 timestring)
                   (match-string 1 timestring)))
        
          ((string-match "\\`\\([0-9]+\\)\\'"
                         timestring)
           (format "%s min" (match-string 1 timestring)))
          ((string-match "\\`\\([0-9]\\)[uU]\\'" timestring)
           (let ((units (string-to-number (match-string 1 timestring))))
             (format "%d sec" (* timan-kitchen-timer-unit units))))
          (t (format "%d sec" timan-kitchen-timer-unit)))))
    (message R)
    R))

(defun timan-kitchen-timer-cancel-all-timers ()
  (interactive)
  (let ((hash timan-kitchen-timer--all-timers)
        (count 0))
    (maphash
     (lambda (key val)
       (setq count (1+ count))
       (timan-kitchen-timer-cancel-timer :timer-id key))
     hash)
    (message "canceled %d timers." count)))

(defun timan-kitchen-timer-cancel-timer (&rest spec)
  (cond ((plist-get spec :timer-id)
         (let* ((timer-id (plist-get spec :timer-id))
                (no-hook  (plist-get spec :no-hook))
                (slot     (gethash timer-id
                                   timan-kitchen-timer--all-timers)))
           (when slot
             (let ((timer  (car    slot))
                   (ticker (cadr   slot))
                   (action (caddr  slot))
                   (label  (cadddr slot)))
               (when timer  (cancel-timer timer))
               (when ticker (cancel-timer ticker))
               (remhash timer-id
                        timan-kitchen-timer--all-timers)
               (unless no-hook
                 (funcall (or action (lambda ())))
                 (message "canceled: %s" label)))))
         (timan-menu-update))))





(defun timan-kitchen-timer--read-intervalstr (intervalstr)
  (+ (*    1 (if (string-match "\\([0-9]+\\)\\s *sec" intervalstr)
                 (string-to-number (match-string 1 intervalstr))
               0))
     (*   60 (if (string-match "\\([0-9]+\\)\\s *min" intervalstr)
                 (string-to-number (match-string 1 intervalstr))
               0))
     (* 3600 (if (string-match "\\([0-9]+\\)\\s *hour" intervalstr)
                 (string-to-number (match-string 1 intervalstr))
               0))))

(eval-after-load "yatest"
  '(yatest::define-test timan timan-kitchen-timer--read-intervalstr
     (dolist (tcase '(("5 sec" 5)
                      ("5 sec 2 min" 125)
                      ("5 sec 2 min 2 hour" 7325)
                     ))
       (let ((str (car tcase))
             (num (cadr tcase)))
         (eval
          `(yatest ,str (eq ,num
                            (yatest::p ,str
                                       (timan-kitchen-timer--read-intervalstr
                                        ,str)))))))))
;;(yatest::run 'timan 'timan-kitchen-timer--read-intervalstr)





(defun timan-kitchen-timer--run-at-time (timestring callback &optional cancel)
  (let*
      ((timer-id
        (intern (format "timer-%d" (setq timan-kitchen-timer--count
                                         (1+ timan-kitchen-timer--count)))))
       (intervalstr
        (timan-kitchen-timer-parse-timestring timestring))

       (interval
        (timan-kitchen-timer--read-intervalstr intervalstr))

       (endtime (+ (cadr (current-time)) interval))

       (timer
        (run-at-time
         intervalstr
         nil
         `(lambda ()
            (timan-kitchen-timer-cancel-timer :timer-id ',timer-id
                                              :no-hook t)
            (funcall ,callback))))

       (ticker
        (run-at-time
         (format "%d sec" timan-kitchen-timer-tick-interval)
         timan-kitchen-timer-tick-interval
         `(lambda ()
            (when (< (cadr (current-time)) ,endtime)
              (timan-play-sound timan-kitchen-timer-tick-sound))
            ))))

    (puthash timer-id (list timer
                            ticker
                            cancel
                            (format "%s -> %s"
                                    (current-time-string)
                                    intervalstr))
             timan-kitchen-timer--all-timers)

    (timan-menu-update)
    t))

;;
;;(run-at-time "0 sec" 1000 'identity)
;;(let ((timan-kitchen-timer-tick-interval 2)) (timan-kitchen-timer "0 0 6"))
;;(setq aaa (run-at-time "0 sec" nil 'identity))
;;(cancel-timer aaa)
;;(progn (message "\n\n\n\n")(sleep-for 2))
;;

(defun timan-kitchen-timer--read-timestring ()
  (list (read-string "chaim at time:")
        (cond ((consp current-prefix-arg)
               (let ((sym (intern (completing-read
                                   "sym: "
                                   timan-sound-alist))))
                 (if (assoc sym timan-sound-alist)
                     sym
                   (error "unknown symbol: %s" sym))))
              ((numberp current-prefix-arg)
               (let* ((len (length timan-sound-alist))
                      (nth (mod current-prefix-arg len)))
                 (car (nth nth timan-sound-alist))))
              (t nil))))

(defun timan-kitchen-timer (timestring &optional timan-sym)
  (interactive (timan-kitchen-timer--read-timestring))
  (timan-kitchen-timer--run-at-time
   timestring
   `(lambda () (timan-play-sound ,timan-sym))))

(defun timan-kitchen-timer-with-insert (timestring &optional timan-sym)
  (interactive (timan-kitchen-timer--read-timestring))
  (timan-insert-current-time "START")
  (insert " -> ")
  (let* ((buf   (current-buffer))
         (point (point))
         (done  `(lambda ()
                   (save-excursion
                     (save-window-excursion
                       (set-buffer ,buf)
                       (goto-char  ,point)
                       (timan-insert-current-time "CLOSED")
                       ))
                   (timan-play-sound ,timan-sym)))
         (cancel `(lambda ()
                      (save-excursion
                        (save-window-excursion
                          (set-buffer ,buf)
                          (goto-char  ,point)
                          (timan-insert-current-time "CANCELED")
                          )))))

    (timan-kitchen-timer--run-at-time timestring done  cancel)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; timan-menu
;;;

(defconst timan-menu-place [menu-bar tools timan])

(defun timan-menu-update ()
  (define-key (current-global-map)
    timan-menu-place

    `("Timan" keymap

      (timan-insert-current-time
       menu-item
       "Insert current time"
       timan-insert-current-time)

      (timan-kitchen-timer
       menu-item
       "Kitchin Timer"
       (keymap
        (timan-kitchen-timer
         menu-item
         "Run"
         timan-kitchen-timer)
        
        (timan-kitchen-timer-with-insert
         menu-item
         "Run with insert"
         timan-kitchen-timer-with-insert)


        (timan-kitchen-timer-cancel
         menu-item
         "Cancel"
         (keymap

          (timan-kitchen-timer-cancel-all-timers
           menu-item
           "All"
           timan-kitchen-timer-cancel-all-timers)

          ,@(let ((R))
              (maphash
               (lambda (key val)
                 (let ((label (cadddr val)))
                   (add-to-list
                    'R
                    `(,(intern (format "timan-kitchen-timer-cancel-%s" key))
                      menu-item
                      ,label
                      (lambda ()
                        (interactive)
                        (timan-kitchen-timer-cancel-timer
                         :timer-id ',key))))))
               timan-kitchen-timer--all-timers)
              (when R
                   (cons 
                    '(timan-sep
                      menu-item
                      "----")
                    (reverse R)))))
         ) ;; end of cancel
        ) ;; end of kitchin timer

      (timan-play
       menu-item
       "Play"
       (keymap
        (timan-play-all-sounds
         menu-item
         "All sounds"
         timan-play-all-sounds)
        (timan-sep
         menu-item
         "----")
        ,@(mapcar
           (lambda (slot)
             (let* ((sym (car slot)))
               `(,sym
                 menu-item
                 ,(symbol-name sym)
                 (lambda ()
                   (interactive)
                   (timan-play-sound
                    ',sym
                    :callback (lambda () (message "Done...")))
                   (message "%s" ',sym)))))

           timan-sound-alist)))))))

(defun timan-menu-enable ()
  (interactive)
  (timan-menu-update))

(defun timan-menu-desable ()
  (interactive)
  (define-key (current-global-map)
    timan-menu-place
    nil))

(defun timan-menu-refresh (&optional new-place)
  (interactive)
  (timan-menu-desable)
  (when new-place (setq timan-menu-place new-place))
  (timan-menu-enable))

(timan-menu-enable)

(provide 'timan)
;;; timan.el ends here

;;(lookup-key (current-global-map) [menu-bar tools])
