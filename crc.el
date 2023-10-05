;;
;; Simple crc implementation for elisp
;;
;; crc-table is inspired from https://www.w3.org/TR/PNG/#D-CRCAppendix
;;

(defun crc-make-table ()
  (let ((table (make-vector 256 0)))
    (cl-loop for n from 0 to 255
             do (let ((c n))
                  (cl-loop for k from 0 to 7
                           do (if (not (= (logand c #x1) 0))
                                  (setq c (logxor #xedb88320 (ash c -1)))
                                (setq c (ash c -1))))
                  (aset table n c)))
    table))

;; (setq test (seq-reverse (seq-reduce (lambda (acc i) (cons (format "0x%X\n" i) acc)) (crc-make-table) '())))

(defun crc-current-string (&rest delimiter-list-arg)
  (let* ((delimiter-list (if (not delimiter-list-arg) (list ?\") delimiter-list-arg))
         (current-point (point)))
    (save-excursion
      (goto-char (point-min))
      (cl-loop with str-start = nil
               with str-char = nil
               with str-end = nil
               with prev-escape? = nil
               for i from (point-min) to (point-max)
               do (let ((c (char-after i)))
                    (let ((is-str-char? (and (not prev-escape?)
                                             (memq c delimiter-list))))
                      (if is-str-char?
                          (if str-start
                              (if (eq c str-char)
                                  (if (and (>= current-point str-start)
                                           (<= current-point i))
                                      (setq str-end i)
                                    (progn (setq str-start nil)
                                           (setq str-char nil))))
                            (progn (setq str-start i)
                                   (setq str-char c)
                                   (message (concat "str starting at " (number-to-string i) " with char " (string c)))))))
                    (setq prev-escape? (and (eq c ?\\)
                                            (not prev-escape?))))
               if str-end return (buffer-substring-no-properties (+ str-start 1) str-end)))))

;; " test  est \"Hello\" asdasdfdasfasdf?fsdf"

(defun crc (str &optional case-sensitive?)
  "Calculate crc of provided string"
  (interactive (list (let* ((default-str (if (region-active-p)
                                             (buffer-substring-no-properties (region-beginning) (region-end))
                                             (crc-current-string)))
                            (input-str (read-string (concat "crc string(default: \"" default-str "\"):"))))
                       (if (string= input-str "") default-str input-str))
                     (y-or-n-p "case sensitive?")))
  (let* ((fixed-string (if case-sensitive? str (downcase str)))
         (crc-value (logxor (seq-reduce (lambda (crc32 i) (let ((lookup (logand (logxor crc32 i) #xFF)))
                                                            (logxor (ash crc32 -8) (elt crc-table lookup))))
                                        (string-to-list fixed-string)
                                        #xFFFFFFFF)
                            #xFFFFFFFF))
         (crc-str (format "0x%X" crc-value)))
    (kill-new crc-str)
    (message (concat crc-str " is the crc of \""str "\" [copied to killring]"))))

(setq crc-table (crc-make-table))
(provide 'crc)
