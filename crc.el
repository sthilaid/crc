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
                                  (setq c (logxor #xedb88320 (lsh c -1)))
                                (setq c (lsh c -1))))
                  (aset table n c)))
    table))

;; (setq test (seq-reverse (seq-reduce (lambda (acc i) (cons (format "0x%X\n" i) acc)) (crc-make-table) '())))


(defun crc (str &optional case-insensitive?)
  "Calculate crc of provided string"
  (interactive (list (read-string "crc string: ")
                     (y-or-n-p "case insensitive?")))
  (let* ((fixed-string (if case-insensitive? (downcase str) str))
         (crc-value (logxor (seq-reduce (lambda (crc32 i) (let ((lookup (logand (logxor crc32 i) #xFF)))
                                                            (logxor (lsh crc32 -8) (elt crc-table lookup))))
                                        (string-to-list fixed-string)
                                        #xFFFFFFFF)
                            #xFFFFFFFF))
         (crc-str (format "0x%X" crc-value)))
    (kill-new crc-str)
    (message (concat crc-str " [copied to killring]"))))

(setq crc-table (crc-make-table))
(provide 'crc)

