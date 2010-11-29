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

(defvar timan-file-type :mp3)

(defvar timan-sound-alist
  (let ((base (file-name-directory (locate-library "timan"))))
    (mapcar (lambda (n)
              (list 
               (intern (format "timan-0%d" n))
               :native
               (expand-file-name (format "sounds/0%d.wav" n) base)
               :mp3
               (expand-file-name (format "sounds/0%d.mp3" n) base)
               ))
            '(0 1 2 3 4 5))))

(defvar timan-player-alist
  '((:native . play-sound)
    (:mp3    . timan-play-mp3)))

(defvar timan-mp3-player-program "mpg123")

(defvar timan-default-chaim-symbol 'timan-00)

(defun timan-get-file (sym)
  (let ((slot (assq (or sym
                        timan-default-chaim-symbol) timan-sound-alist)))
    (and slot
         (plist-get (cdr slot) timan-file-type ))))
;;(timan-get-file 'timan-00)

(defun timan-play-mp3 (&rest spec)
  (let* ((file     (plist-get spec :file))
         (buf      (generate-new-buffer " *timan mp3*"))
         (sentinel `(lambda (proc signal)
                      (when (equal signal "finished\n")
                        (kill-buffer ,buf))))
         (proc (start-process "timan-mp3" 
                              buf
                              timan-mp3-player-program
                              file)))
    (set-process-sentinel proc sentinel)))
;;(timan-play-mp3 :file "sounds/00.mp3")

(defun timan-play-chaim (&optional sym)
  (let ((file (timan-get-file sym))
        (player (assq timan-file-type timan-player-alist)))
    (funcall (cdr player) :file file)))



(defvar timan-kitchen-timer-unit (* 60 15))

(defun timan-kitchen-timer-parse-timestring (timestring)
  (cond ((string-match
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
        (t (format "%d sec" timan-kitchen-timer-unit))))


(defun timan-kitchen-timer--read-timestring ()
  (list (read-string "chaim at time:")
        (cond ((consp current-prefix-arg)
               (let ((sym (intern (completing-read "sym: " timan-sound-alist))))
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
  (run-at-time
   (timan-kitchen-timer-parse-timestring timestring)
   nil
   `(lambda (timan-play-chaim ,timan-sym))))

(defun timan-kitchen-timer-with-insert (timestring &optional timan-sym)
  (interactive (timan-kitchen-timer--read-timestring))
  (let* ((buf   (current-buffer))
         (point (point))
         (done  `(lambda ()
                   (save-excursion
                     (save-window-excursion
                       (set-buffer ,buf)
                       (goto-char  ,point)
                       (let ((col  (- (point)
                                      (save-excursion
                                        (beginning-of-line)(point)))))
                         (end-of-line)
                         (insert "\n"
                                 (make-string col 32) (current-time-string)
                                 "\n")
                         )))
                   (timan-play-chaim ,timan-sym))))
    (insert (current-time-string))
    (run-at-time
     (timan-kitchen-timer-parse-timestring timestring)
     nil
     done)))





(provide 'timan)
;;; timan.el ends here
