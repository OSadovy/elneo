;;; bar-places-test.el --- room bar places across a full re-render -*- lexical-binding: t; -*-

;; A full re-render of the room bar puts every place kept in it back
;; on the room row it was on: the buffer's point, the point of each
;; window showing the bar, and the place a window keeps for the bar
;; while it shows something else, which `switch-to-prev-buffer'
;; returns to.  The registry is stubbed; the re-render is the real one.
;;
;; Run: emacs -Q --batch -L lisp -l test/bar-places-test.el

(require 'cl-lib)
(require 'elneo-room-bar)

(let ((pass 0) (fail 0)
      (rooms '((:profile "a" :room-id "!quiet" :display-name "Quiet room"
                         :unread 0 :highlights 0 :last-activity 100 :last-message "")
               (:profile "a" :room-id "!loud" :display-name "Loud room"
                         :unread 5 :highlights 0 :last-activity 300 :last-message "")
               (:profile "a" :room-id "!middle" :display-name "Middle room"
                         :unread 2 :highlights 0 :last-activity 200 :last-message ""))))
  (defun mx-check (label got expected)
    (if (equal got expected)
        (progn (cl-incf pass) (message "ok: %s" label))
      (cl-incf fail)
      (message "FAIL: %s — got %S, expected %S" label got expected)))

  (defmacro mx-with-rooms (&rest body)
    `(cl-letf (((symbol-function 'elneo-rooms-all) (lambda () rooms)))
       ,@body))

  (defun mx-room-at (pos)
    (with-current-buffer elneo-room-bar--buffer
      (cdr (get-text-property pos 'elneo-room-key))))

  (defun mx-row (room-id)
    (with-current-buffer elneo-room-bar--buffer
      (marker-position (gethash (cons "a" room-id) elneo-room-bar--markers))))

  (unwind-protect
      (mx-with-rooms
       (elneo-room-bar--ensure)
       (switch-to-buffer elneo-room-bar--buffer)

       ;; --- two windows showing the bar, each on its own row ---
       (let* ((first (selected-window))
              (second (split-window)))
         (set-window-buffer second elneo-room-bar--buffer)
         (set-window-point first (mx-row "!middle"))
         (set-window-point second (mx-row "!quiet"))
         (elneo-room-bar-refresh-full)
         (mx-check "the selected window stays on its room"
                   (mx-room-at (window-point first)) "!middle")
         (mx-check "a second window stays on its room"
                   (mx-room-at (window-point second)) "!quiet")
         (delete-window second))

       ;; --- the bar hidden: its point and the window's kept place ---
       (goto-char (mx-row "!middle"))
       (switch-to-buffer "*scratch*")
       (with-current-buffer elneo-room-bar--buffer
         (goto-char (mx-row "!quiet")))
       (elneo-room-bar-refresh-full)
       (mx-check "the hidden bar's own point stays on its room"
                 (with-current-buffer elneo-room-bar--buffer
                   (mx-room-at (point)))
                 "!quiet")
       (switch-to-prev-buffer)
       (mx-check "going back to the bar lands on the room left"
                 (mx-room-at (window-point)) "!middle"))
    (when (buffer-live-p elneo-room-bar--buffer)
      (kill-buffer elneo-room-bar--buffer)))

  (message "results: %d passed, %d failed" pass fail)
  (kill-emacs (if (zerop fail) 0 1)))
