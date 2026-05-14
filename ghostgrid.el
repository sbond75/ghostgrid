;;; ghostgrid.el --- Ghost a base text grid into an overlay buffer -*- lexical-binding: t; -*-

;; This file is intentionally standalone.  Drop it on your load-path and:
;;   (require 'ghostgrid)

(require 'cl-lib)
(require 'subr-x)

(defgroup ghostgrid nil
  "Show a base text-grid as ghost characters in an overlay buffer."
  :group 'tools)

(defcustom ghostgrid-associations-file
  (expand-file-name "ghostgrid-assoc.el" user-emacs-directory)
  "File where ghostgrid persists overlay-file -> base-file associations."
  :type 'file)

(defcustom ghostgrid-refresh-delay 0.04
  "Idle delay, in seconds, before refreshing ghost overlays after edits."
  :type 'number)

(defcustom ghostgrid-overlay-priority 10000
  "Overlay priority used for ghost characters.
Higher values make the ghost display win over font-lock/tree-sitter faces."
  :type 'integer)

(defcustom ghostgrid-materialize-padding t
  "If non-nil, insert real spaces in short overlay lines before drawing ghosts.

Emacs cannot put point inside pure `after-string' display text.  When this
option is enabled, ghostgrid pads overlay-region lines out to the base line
length with ordinary spaces.  That makes every ghost column an editable buffer
position, so you can click, move, and type before a ghost character.  The spaces
are real buffer text; ghostgrid preserves the buffer's modified flag when it
adds padding during refresh, but a later save can include those spaces."
  :type 'boolean)

(defface ghostgrid-ghost-face
  '((t (:inherit shadow :slant italic)))
  "Face used for ghost characters copied from the base buffer."
  :group 'ghostgrid)

(defvar ghostgrid--table (make-hash-table :test 'equal)
  "Persistent table keyed by absolute overlay file path.
Value is a plist:
  (:base-file ABS
   :base-spec SPEC
   :overlay-spec SPEC)

SPEC is either:
  (:type lua-long-string :name NAME)
or:
  (:type linecol :start-line LINE :start-col COL :end-line LINE :end-col COL)")

(defvar-local ghostgrid--overlays nil
  "Buffer-local list of live ghost overlays.")

(defvar-local ghostgrid--entry nil
  "Buffer-local active association plist for this overlay buffer.")

(defvar-local ghostgrid--base-buffer nil
  "Buffer-local base buffer watched by this overlay buffer.")

(defvar-local ghostgrid--refresh-timer nil
  "Buffer-local idle timer used to debounce refreshes.")

(defvar-local ghostgrid--inhibit-refresh nil
  "Non-nil while ghostgrid is changing padding internally.")

(defvar-local ghostgrid--base-watchers nil
  "In a base buffer, list of overlay buffers that should refresh after base edits.")

(defun ghostgrid--abs (file)
  "Return FILE as an absolute path."
  (expand-file-name file))

(defun ghostgrid-save-associations ()
  "Persist `ghostgrid--table' to `ghostgrid-associations-file'."
  (interactive)
  (make-directory (file-name-directory ghostgrid-associations-file) t)
  (with-temp-file ghostgrid-associations-file
    (let ((print-length nil)
          (print-level nil))
      (prin1
       (let (alist)
         (maphash (lambda (k v) (push (cons k v) alist)) ghostgrid--table)
         alist)
       (current-buffer)))))

(defun ghostgrid-load-associations ()
  "Load `ghostgrid--table' from `ghostgrid-associations-file', if present."
  (interactive)
  (setq ghostgrid--table (make-hash-table :test 'equal))
  (when (file-readable-p ghostgrid-associations-file)
    (with-temp-buffer
      (insert-file-contents ghostgrid-associations-file)
      (goto-char (point-min))
      (let ((alist (read (current-buffer))))
        (dolist (cell alist)
          (puthash (car cell) (cdr cell) ghostgrid--table))))))

(ghostgrid-load-associations)

(defun ghostgrid--linecol-at (pos)
  "Return a plist for POS with 1-based line and 0-based column."
  (save-excursion
    (goto-char pos)
    (list :line (line-number-at-pos)
          :col (current-column))))

(defun ghostgrid--region-to-linecol-spec (beg end)
  "Return a persistent line/column region spec for BEG..END."
  (let* ((start (ghostgrid--linecol-at beg))
         (finish (ghostgrid--linecol-at end)))
    (list :type 'linecol
          :start-line (plist-get start :line)
          :start-col (plist-get start :col)
          :end-line (plist-get finish :line)
          :end-col (plist-get finish :col))))

(defun ghostgrid--point-at-linecol (line col)
  "Return buffer position at 1-based LINE and 0-based COL.
This does not insert padding; if COL is past EOL, return EOL."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column col)
    (point)))

(defun ghostgrid--lua-long-string-region (name)
  "Return (BEG . END) for the Lua long string assigned to NAME.
For example NAME can be `shape.contents' or `contentsDecoration'.
The returned region is the inside of the [[...]] string, excluding delimiters."
  (save-excursion
    (goto-char (point-min))
    (let ((re (format "\\_<%s\\_>\\s-*=[ \t\n\r]*\\[\\["
                      (regexp-quote name))))
      (unless (re-search-forward re nil t)
        (user-error "ghostgrid: could not find Lua long string assignment for %S" name))
      (let ((beg (point)))
        (unless (search-forward "]]" nil t)
          (user-error "ghostgrid: could not find closing ]] for %S" name))
        (cons beg (match-beginning 0))))))

(defun ghostgrid--resolve-region (buffer spec)
  "Resolve SPEC in BUFFER and return (BEG . END)."
  (with-current-buffer buffer
    (pcase (plist-get spec :type)
      ('lua-long-string
       (ghostgrid--lua-long-string-region (plist-get spec :name)))
      ('linecol
       (cons (ghostgrid--point-at-linecol
              (plist-get spec :start-line)
              (plist-get spec :start-col))
             (ghostgrid--point-at-linecol
              (plist-get spec :end-line)
              (plist-get spec :end-col))))
      (_
       (user-error "ghostgrid: unknown region spec %S" spec)))))

(defun ghostgrid--region-lines (buffer region)
  "Return lines in BUFFER inside REGION as strings without text properties."
  (with-current-buffer buffer
    (let ((text (buffer-substring-no-properties (car region) (cdr region))))
      ;; Keep empty rows.  This matters because the grid is line-based.
      (split-string text "\n" nil))))

(defun ghostgrid--whitespace-char-p (ch)
  "Return non-nil if CH is nil or whitespace."
  (or (null ch)
      (memq ch '(?\s ?\t ?\n ?\r))))

(defun ghostgrid--nonwhitespace-char-p (ch)
  "Return non-nil if CH exists and is not whitespace."
  (and ch (not (ghostgrid--whitespace-char-p ch))))

(defun ghostgrid--propertize-ghost (s)
  "Return S propertized as ghost display text."
  (propertize s
              'face 'ghostgrid-ghost-face
              'font-lock-face 'ghostgrid-ghost-face))

(defun ghostgrid--delete-overlays ()
  "Delete every live ghost overlay in the current buffer.

The list in `ghostgrid--overlays' is the normal fast path, but commands such as
`revert-buffer' can leave stale zero-width display overlays around if a timer
fires at an awkward time.  The property sweep is the belt-and-suspenders cleanup
that prevents duplicate ghost text from stacking and drifting sideways."
  (mapc (lambda (ov)
          (when (overlayp ov)
            (delete-overlay ov)))
        ghostgrid--overlays)
  (setq ghostgrid--overlays nil)
  (remove-overlays (point-min) (point-max) 'ghostgrid t)
  ;; `remove-overlays' is range-oriented; explicitly catch zero-width overlays
  ;; pinned to the buffer edges.
  (dolist (pos (list (point-min) (point-max)))
    (dolist (ov (overlays-at pos))
      (when (overlay-get ov 'ghostgrid)
        (delete-overlay ov)))))

(defun ghostgrid--add-display-overlay (beg end text)
  "Display TEXT over BEG..END as a ghost overlay."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'ghostgrid t)
    (overlay-put ov 'priority ghostgrid-overlay-priority)
    (overlay-put ov 'display (ghostgrid--propertize-ghost text))
    (push ov ghostgrid--overlays)
    ov))

(defun ghostgrid--add-after-string-overlay (pos text)
  "Display TEXT after POS as a ghost overlay."
  (let ((ov (make-overlay pos pos nil t nil)))
    (overlay-put ov 'ghostgrid t)
    (overlay-put ov 'priority ghostgrid-overlay-priority)
    (overlay-put ov 'after-string text)
    (push ov ghostgrid--overlays)
    ov))

(defun ghostgrid--line-starts-in-region (region)
  "Return buffer positions for starts of lines inside REGION."
  (let (starts)
    (save-excursion
      (goto-char (car region))
      (while (<= (point) (cdr region))
        (push (point) starts)
        (if (= (forward-line 1) 1)
            (goto-char (1+ (cdr region))))))
    (nreverse starts)))

(defun ghostgrid--render-existing-run (line-start run-start run-end base-line)
  "Render BASE-LINE characters RUN-START..RUN-END over existing overlay whitespace."
  (ghostgrid--add-display-overlay
   (+ line-start run-start)
   (+ line-start run-end)
   (substring base-line run-start run-end)))

(defun ghostgrid--current-line-char-length ()
  "Return the current line length in buffer characters."
  (- (line-end-position) (line-beginning-position)))

(defun ghostgrid--ensure-overlay-padding (overlay-region base-lines)
  "Pad short overlay lines in OVERLAY-REGION to match BASE-LINES.

This fixes the editing problem caused by displaying ghosts past EOL with
`after-string': after-string text is visible, but point cannot move inside it.
By inserting ordinary spaces first, the ghost columns become normal editable
positions."
  (when ghostgrid-materialize-padding
    (let ((old-modified (buffer-modified-p))
          (inhibit-read-only t)
          (ghostgrid--inhibit-refresh t)
          ;; Use a marker, not the raw integer end from OVERLAY-REGION.  Padding
          ;; inserted near the top of the grid moves the closing delimiter to the
          ;; right; a stale integer end makes the loop think it has already
          ;; passed the region after only a few wide rows.
          (overlay-end-marker (copy-marker (cdr overlay-region) t)))
      (unwind-protect
          (save-excursion
            (goto-char (car overlay-region))
            (cl-loop for base-line in base-lines
                     while (<= (point) overlay-end-marker)
                     do (let* ((base-len (length base-line))
                               (overlay-len (ghostgrid--current-line-char-length))
                               (missing (- base-len overlay-len)))
                          (when (> missing 0)
                            (goto-char (line-end-position))
                            (insert (make-string missing ?\s)))
                          (forward-line 1))))
        (set-marker overlay-end-marker nil))
      ;; Padding is infrastructure, not a user edit.  If the buffer was clean
      ;; before the refresh, keep it clean after adding spaces.  If the user
      ;; later edits and saves, those spaces can naturally become part of the
      ;; file, which is useful for fixed-width grid editing.
      (unless old-modified
        (set-buffer-modified-p nil)))))

(defun ghostgrid--render-line (line-start line-end base-line overlay-line)
  "Render ghost chars for BASE-LINE on one overlay line.
LINE-START and LINE-END are positions in the overlay buffer."
  (let* ((base-len (length base-line))
         (overlay-len (length overlay-line))
         (existing-max (min base-len overlay-len))
         (col 0))
    ;; Existing chars: replace whitespace chars with base chars only when the
    ;; base has a non-whitespace char and overlay has no real char there.
    (while (< col existing-max)
      (if (and (ghostgrid--nonwhitespace-char-p (aref base-line col))
               (ghostgrid--whitespace-char-p (aref overlay-line col)))
          (let ((run-start col))
            (while (and (< col existing-max)
                        (ghostgrid--nonwhitespace-char-p (aref base-line col))
                        (ghostgrid--whitespace-char-p (aref overlay-line col)))
              (setq col (1+ col)))
            (ghostgrid--render-existing-run line-start run-start col base-line))
        (setq col (1+ col))))

    ;; Fallback for users who disable `ghostgrid-materialize-padding'.  This is
    ;; visible, but not truly editable past EOL because Emacs does not put point
    ;; inside after-string display text.
    (when (and (not ghostgrid-materialize-padding)
               (< overlay-len base-len))
      (let ((tail (make-string (- base-len overlay-len) ?\s))
            (has-ghost nil))
        (cl-loop for i from overlay-len below base-len
                 for base-ch = (aref base-line i)
                 when (ghostgrid--nonwhitespace-char-p base-ch)
                 do (progn
                      (aset tail (- i overlay-len) base-ch)
                      (setq has-ghost t)))
        (when has-ghost
          ;; Face only non-whitespace ghost chars.  Padding remains plain so it
          ;; does not paint a visible background unless the user customizes it.
          (cl-loop for i from 0 below (length tail)
                   for ch = (aref tail i)
                   when (ghostgrid--nonwhitespace-char-p ch)
                   do (add-text-properties i (1+ i)
                                           '(face ghostgrid-ghost-face
                                             font-lock-face ghostgrid-ghost-face)
                                           tail))
          (ghostgrid--add-after-string-overlay line-end tail))))))

(defun ghostgrid-refresh ()
  "Refresh ghost characters in the current overlay buffer."
  (interactive)
  (unless ghostgrid-mode
    (user-error "ghostgrid-mode is not active"))
  (unless ghostgrid--entry
    (user-error "ghostgrid: no active association for this buffer"))
  (let* ((base-file (plist-get ghostgrid--entry :base-file))
         (base-buffer (or ghostgrid--base-buffer
                          (find-file-noselect base-file)))
         (base-region (ghostgrid--resolve-region
                       base-buffer
                       (plist-get ghostgrid--entry :base-spec)))
         (overlay-region (ghostgrid--resolve-region
                          (current-buffer)
                          (plist-get ghostgrid--entry :overlay-spec)))
         (base-lines (ghostgrid--region-lines base-buffer base-region)))
    (setq ghostgrid--base-buffer base-buffer)
    (ghostgrid--delete-overlays)
    (ghostgrid--ensure-overlay-padding overlay-region base-lines)
    ;; Padding can move the closing delimiter and therefore the resolved region
    ;; end.  Resolve again before reading overlay lines or placing overlays.
    (setq overlay-region (ghostgrid--resolve-region
                          (current-buffer)
                          (plist-get ghostgrid--entry :overlay-spec)))
    (let ((overlay-lines (ghostgrid--region-lines (current-buffer) overlay-region))
          (overlay-starts (ghostgrid--line-starts-in-region overlay-region)))
      (cl-loop for base-line in base-lines
               for overlay-line in overlay-lines
               for line-start in overlay-starts
               do (let ((line-end (save-excursion
                                    (goto-char line-start)
                                    (line-end-position))))
                    (ghostgrid--render-line line-start line-end base-line overlay-line))))))

(defun ghostgrid--schedule-refresh (&rest _ignore)
  "Schedule a debounced refresh for the current overlay buffer."
  (unless ghostgrid--inhibit-refresh
    (when ghostgrid--refresh-timer
      (cancel-timer ghostgrid--refresh-timer))
    (let ((buffer (current-buffer)))
      (setq ghostgrid--refresh-timer
            (run-with-idle-timer
             ghostgrid-refresh-delay nil
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq ghostgrid--refresh-timer nil)
                   (when ghostgrid-mode
                     (ignore-errors (ghostgrid-refresh)))))))))))

(defun ghostgrid--base-after-change (&rest _ignore)
  "Refresh all overlay buffers watching the current base buffer."
  (dolist (overlay-buffer ghostgrid--base-watchers)
    (when (buffer-live-p overlay-buffer)
      (with-current-buffer overlay-buffer
        (when ghostgrid-mode
          (ghostgrid--schedule-refresh))))))

(defun ghostgrid--watch-base-buffer (base-buffer overlay-buffer)
  "Make OVERLAY-BUFFER refresh when BASE-BUFFER changes."
  (with-current-buffer base-buffer
    (cl-pushnew overlay-buffer ghostgrid--base-watchers :test #'eq)
    (add-hook 'after-change-functions #'ghostgrid--base-after-change nil t)))

(defun ghostgrid--unwatch-base-buffer (base-buffer overlay-buffer)
  "Stop making OVERLAY-BUFFER refresh when BASE-BUFFER changes."
  (when (buffer-live-p base-buffer)
    (with-current-buffer base-buffer
      (setq ghostgrid--base-watchers
            (delq overlay-buffer ghostgrid--base-watchers))
      (unless ghostgrid--base-watchers
        (remove-hook 'after-change-functions #'ghostgrid--base-after-change t)))))

(defun ghostgrid--before-revert ()
  "Remove ghost overlays before `revert-buffer' replaces buffer text."
  (when ghostgrid-mode
    (when ghostgrid--refresh-timer
      (cancel-timer ghostgrid--refresh-timer)
      (setq ghostgrid--refresh-timer nil))
    (ghostgrid--delete-overlays)))

(defun ghostgrid--after-revert ()
  "Refresh ghost overlays after `revert-buffer'."
  (when ghostgrid-mode
    (ghostgrid--schedule-refresh)))

(defun ghostgrid--activate-entry (entry)
  "Activate ghostgrid in current overlay buffer using ENTRY."
  ;; Be idempotent.  This matters when auto-activation and manual activation
  ;; both touch the same buffer, and it also prevents duplicate hooks/watchers.
  (ghostgrid--deactivate-entry)
  (setq ghostgrid--entry entry)
  (let ((base-buffer (find-file-noselect (plist-get entry :base-file))))
    (setq ghostgrid--base-buffer base-buffer)
    (ghostgrid--watch-base-buffer base-buffer (current-buffer)))
  (add-hook 'after-change-functions #'ghostgrid--schedule-refresh nil t)
  (add-hook 'before-revert-hook #'ghostgrid--before-revert nil t)
  (add-hook 'after-revert-hook #'ghostgrid--after-revert nil t)
  (ghostgrid-refresh))

(defun ghostgrid--deactivate-entry ()
  "Deactivate ghostgrid bookkeeping in the current overlay buffer."
  (when ghostgrid--refresh-timer
    (cancel-timer ghostgrid--refresh-timer)
    (setq ghostgrid--refresh-timer nil))
  (remove-hook 'after-change-functions #'ghostgrid--schedule-refresh t)
  (remove-hook 'before-revert-hook #'ghostgrid--before-revert t)
  (remove-hook 'after-revert-hook #'ghostgrid--after-revert t)
  (when ghostgrid--base-buffer
    (ghostgrid--unwatch-base-buffer ghostgrid--base-buffer (current-buffer)))
  (setq ghostgrid--base-buffer nil)
  (setq ghostgrid--entry nil)
  (ghostgrid--delete-overlays))

(defvar ghostgrid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c g r") #'ghostgrid-refresh)
    (define-key map (kbd "C-c g g") #'ghostgrid-register)
    (define-key map (kbd "C-c g l") #'ghostgrid-register-lua-decoration)
    (define-key map (kbd "C-c g s") #'ghostgrid-save-associations)
    (define-key map (kbd "C-c g c") #'ghostgrid-clear-association)
    (define-key map (kbd "C-c g b") #'ghostgrid-jump-to-base)
    map)
  "Keymap for `ghostgrid-mode'.")

;;;###autoload
(define-minor-mode ghostgrid-mode
  "Minor mode that ghosts a base text-grid into an overlay buffer.

The buffer that owns the original grid is called the base.  The buffer
where this mode is enabled is called the overlay.  For each aligned
line/column position, the overlay's real non-whitespace characters win.
If the overlay position is whitespace or missing and the base position is
non-whitespace, ghostgrid displays the base character using
`ghostgrid-ghost-face'."
  :lighter " GhostGrid"
  :keymap ghostgrid-mode-map
  (if ghostgrid-mode
      (let* ((overlay-file (and buffer-file-name (ghostgrid--abs buffer-file-name)))
             (entry (and overlay-file (gethash overlay-file ghostgrid--table))))
        (unless overlay-file
          (setq ghostgrid-mode nil)
          (user-error "ghostgrid-mode needs a file-visiting overlay buffer"))
        (unless entry
          (setq ghostgrid-mode nil)
          (user-error "ghostgrid: no association for %s; use ghostgrid-register first" overlay-file))
        (ghostgrid--activate-entry entry))
    (ghostgrid--deactivate-entry)))

(defun ghostgrid--set-association (overlay-file entry)
  "Persist ENTRY for OVERLAY-FILE."
  (puthash (ghostgrid--abs overlay-file) entry ghostgrid--table)
  (ghostgrid-save-associations))

;;;###autoload
(defun ghostgrid-register (base-buffer)
  "Associate current overlay region with BASE-BUFFER's active region.

Usage:
1. In the base buffer, select the grid region.
2. In the overlay buffer, select the structurally matching region.
3. Run this command from the overlay buffer and choose the base buffer.

The association is saved, so opening this overlay file later enables
`ghostgrid-mode' automatically."
  (interactive
   (list (read-buffer "Base buffer with active region: "
                      (buffer-name (other-buffer (current-buffer) t))
                      t)))
  (unless buffer-file-name
    (user-error "ghostgrid-register must run in a file-visiting overlay buffer"))
  (unless (use-region-p)
    (user-error "Select the overlay region first"))
  (let* ((overlay-buffer (current-buffer))
         (overlay-file (ghostgrid--abs buffer-file-name))
         (overlay-spec (ghostgrid--region-to-linecol-spec
                        (region-beginning)
                        (region-end)))
         (base-buf (get-buffer base-buffer)))
    (unless base-buf
      (user-error "No such base buffer: %s" base-buffer))
    (with-current-buffer base-buf
      (unless buffer-file-name
        (user-error "Base buffer must visit a file"))
      (unless (use-region-p)
        (user-error "Select the base region first"))
      (let ((entry (list :base-file (ghostgrid--abs buffer-file-name)
                         :base-spec (ghostgrid--region-to-linecol-spec
                                     (region-beginning)
                                     (region-end))
                         :overlay-spec overlay-spec)))
        (with-current-buffer overlay-buffer
          (ghostgrid--set-association overlay-file entry)
          (when ghostgrid-mode
            (ghostgrid-mode -1))
          (ghostgrid-mode 1)
          (message "ghostgrid: associated overlay %s with base %s"
                   overlay-file (plist-get entry :base-file)))))))

;;;###autoload
(defun ghostgrid-register-lua-decoration (base-file)
  "Associate this Lua decoration file with BASE-FILE.

This convenience command auto-finds:
  base:    shape.contents = [[...]]
  overlay: contentsDecoration = [[...]]

Run it from the _decorations Lua buffer.  The association is saved, and
future visits to this overlay file auto-enable `ghostgrid-mode'."
  (interactive "fBase level Lua file: ")
  (unless buffer-file-name
    (user-error "ghostgrid-register-lua-decoration must run in a file-visiting overlay buffer"))
  (let* ((overlay-file (ghostgrid--abs buffer-file-name))
         (base-file-abs (ghostgrid--abs base-file))
         (entry (list :base-file base-file-abs
                      :base-spec (list :type 'lua-long-string
                                       :name "shape.contents")
                      :overlay-spec (list :type 'lua-long-string
                                          :name "contentsDecoration"))))
    ;; Validate eagerly so a typo fails now instead of on next open.
    (ghostgrid--resolve-region (find-file-noselect base-file-abs)
                               (plist-get entry :base-spec))
    (ghostgrid--resolve-region (current-buffer)
                               (plist-get entry :overlay-spec))
    (ghostgrid--set-association overlay-file entry)
    (when ghostgrid-mode
      (ghostgrid-mode -1))
    (ghostgrid-mode 1)
    (message "ghostgrid: associated Lua overlay %s with base %s"
             overlay-file base-file-abs)))

;;;###autoload
(defun ghostgrid-clear-association ()
  "Remove the persisted association for the current overlay file."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let ((overlay-file (ghostgrid--abs buffer-file-name)))
    (when ghostgrid-mode
      (ghostgrid-mode -1))
    (remhash overlay-file ghostgrid--table)
    (ghostgrid-save-associations)
    (message "ghostgrid: cleared association for %s" overlay-file)))

(defun ghostgrid-jump-to-base ()
  "Open the base file for the current overlay buffer."
  (interactive)
  (unless ghostgrid--entry
    (user-error "ghostgrid: no active association"))
  (find-file-other-window (plist-get ghostgrid--entry :base-file)))

;;;###autoload
(defun ghostgrid-maybe-activate ()
  "Enable `ghostgrid-mode' when visiting a persisted overlay file.
Base files do not auto-activate."
  (when buffer-file-name
    (let* ((overlay-file (ghostgrid--abs buffer-file-name))
           (entry (gethash overlay-file ghostgrid--table)))
      (when entry
        (ghostgrid-mode 1)))))

;;;###autoload
(define-globalized-minor-mode global-ghostgrid-mode
  ghostgrid-mode
  ghostgrid-maybe-activate
  :group 'ghostgrid)

(add-hook 'find-file-hook #'ghostgrid-maybe-activate)

(provide 'ghostgrid)

;;; ghostgrid.el ends here
