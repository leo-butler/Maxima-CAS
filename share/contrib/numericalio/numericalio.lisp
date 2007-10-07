;;  Copyright 2005 by Robert Dodier

;;  This program is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License, version 2.

;;  This program has NO WARRANTY, not even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(in-package :maxima)

;; Read functions:
;;   M: read_matrix (file_name, sep_ch_flag)$
;;   read_lisp_array (file_name, A, sep_ch_flag)$
;;   read_maxima_array (file_name, A, sep_ch_flag)$
;;   read_hashed_array (file_name, A, sep_ch_flag)$
;;   L: read_nested_list (file_name, sep_ch_flag)$
;;   L: read_list (file_name, sep_ch_flag)$
;;
;; Write function:
;;   write_data (X, file_name, sep_ch_flag)$

;; See numericalio.texi for a lengthier description.

(defun $assign_io_endianness (x)
  (cond
    ((eq x '$little_endian)
     (define-io-endianness :little-endian))
    ((eq x '$big_endian)
     (define-io-endianness :big-endian))
    (t
      (merror "assign_io_endianness: unrecognized endianness"))))

;; -------------------- read functions --------------------

(defun $read_matrix (file-name &optional sep-ch-flag)
  `(($matrix) ,@(cdr ($read_nested_list file-name sep-ch-flag))))


(defun $read_lisp_array (file-name A &optional sep-ch-flag)
  ($fillarray A ($read_list file-name sep-ch-flag))
  '$done)


(defun $read_maxima_array (file-name A &optional sep-ch-flag)
  ($fillarray A ($read_list file-name sep-ch-flag))
  '$done)


(defun $read_hashed_array (stream-or-filename A &optional sep-ch-flag)
  (if (streamp stream-or-filename)
    (read-hashed-array-from-stream stream-or-filename A sep-ch-flag)
    (let ((file-name (require-string stream-or-filename)))
      (with-open-file (in file-name :if-does-not-exist nil)
        (if (not (null in))
          (read-hashed-array-from-stream in A sep-ch-flag)
          (merror "read_hashed_array no such file `~a'" file-name))))))

(defun read-hashed-array-from-stream (in A sep-ch-flag)
  (let (key L (sep-ch (get-input-sep-ch sep-ch-flag (truename in))))
    (loop
      (setq L (read-line in nil 'eof))
      (if (eq L 'eof) (return))
      (setq L (make-mlist-from-string L sep-ch))
      (cond
        ((> ($length L) 0)
         (setq key ($first L))
         (if (= ($length L) 1)
           (arrstore (list (list A 'array) key) nil)
           (arrstore (list (list A 'array) key) ($rest L)))))))
  A)

(defun $read_nested_list (stream-or-filename &optional sep-ch-flag)
  (if (streamp stream-or-filename)
    (read-nested-list-from-stream stream-or-filename sep-ch-flag)
    (let ((file-name (require-string stream-or-filename)))
      (with-open-file (in file-name :if-does-not-exist nil)
        (if (not (null in))
          (read-nested-list-from-stream in sep-ch-flag)
          (merror "read_nested_list: no such file `~a'" file-name))))))

(defun read-nested-list-from-stream (in sep-ch-flag)
  (let (A L (sep-ch (get-input-sep-ch sep-ch-flag (truename in))))
    (loop
      (setq L (read-line in nil 'eof))
      (if (eq L 'eof)
        (return (cons '(mlist simp) (nreverse A))))
      (setq A (cons (make-mlist-from-string L sep-ch) A)))))


(defun $read_list (stream-or-filename &optional sep-ch-flag)
  (if (streamp stream-or-filename)
    (read-list-from-stream stream-or-filename sep-ch-flag)
    (let ((file-name (require-string stream-or-filename)))
      (with-open-file (in file-name :if-does-not-exist nil)
        (if (not (null in))
          (read-list-from-stream in sep-ch-flag)
          (merror "read_list: no such file `~a'" file-name))))))

(defun read-list-from-stream (in sep-ch-flag)
  (let (A L (sep-ch (get-input-sep-ch sep-ch-flag (truename in))))
    (loop
      (setq L (read-line in nil 'eof))
      (if (eq L 'eof)
        (return (cons '(mlist simp) (nreverse A))))
      ;; use nreconc accumulation to avoid n^2 cons's
      (setq A (nreconc (cdr (make-mlist-from-string L sep-ch)) A)))))

;; Usage: (make-mlist-from-string "1 2 3 foo bar baz")

(defun make-mlist-from-string (s sep-ch)
  ; scan-one-token-g isn't happy with symbol at end of string.
  (setq s (concatenate 'string s " "))

  (with-input-from-string (*parse-stream* s)
    (let ((token) (L) (LL) (sign))
      (loop
        (setq token (scan-one-token-g t 'eof))
        (cond
          ((eq token 'eof)
           (cond
             ((not (null sign))
              (format t "numericalio: trailing sign (~S) at end of line; strange, but just eat it.~%" sign)))
           (cond
             ((eq sep-ch #\space)
              (return (cons '(mlist) LL)))
             (t
               (return (cons '(mlist) (appropriate-append L LL)))))))
        (cond
          ((or (eq token '$-) (eq token '$+))
           (setq sign (cond ((eq token '$-) -1) (t 1))))
          (t
            (cond
              ((not (null sign))
               (setq token (m* sign token))
               (setq sign nil)))
            (cond
              ((eq sep-ch #\space)
               (setq LL (append LL (list token))))
              (t
                (cond
                  ((eq token sep-ch)
                   (setq L (appropriate-append L LL))
                   (setq LL nil))
                  (t
                    (setq LL (append LL (list token)))))))))))))


(defun appropriate-append (L LL)
  (cond
    ((null LL) (append L '(nil)))
    ((= (length LL) 1) (append L LL))
    (t (append L (list (append '((mlist)) LL))))))


;; -------------------- write functions -------------------

(defun open-file-appropriately (file-name mode)
  (open file-name
        :direction :output
        :element-type (if (eq mode 'text) 'character '(unsigned-byte 8))
        :if-exists (if (or (eq $file_output_append '$true) (eq $file_output_append t)) :append :supersede)
        :if-does-not-exist :create))

(defun $write_data (X stream-or-filename &optional sep-ch-flag)
  (write-data X stream-or-filename sep-ch-flag 'text))

(defun $write_binary_data (X stream-or-filename)
  (write-data X stream-or-filename nil 'binary))

(defun write-data (X stream-or-filename sep-ch-flag mode)
  (let
    ((out
       (if (streamp stream-or-filename)
         stream-or-filename
         (open-file-appropriately (require-string stream-or-filename) mode))))
    (cond
      (($matrixp X)
        (write-matrix X out sep-ch-flag mode))
      ((arrayp X)
        (write-lisp-array X out sep-ch-flag mode))
      ((mget X 'array)
        (write-maxima-array X out sep-ch-flag mode))
      ((mget X 'hashar)
        (write-hashed-array X out sep-ch-flag mode))
      (($listp X)
        (write-list X out sep-ch-flag mode))
      (t (merror "write_data: don't know what to do with a ~M" (type-of X))))
    (if (not (streamp stream-or-filename))
      (close out))
    '$done))

(defun write-matrix (M out sep-ch-flag mode)
  (let ((sep-ch (get-output-sep-ch sep-ch-flag (truename out))))
    (mapcar #'(lambda (x) (write-list-lowlevel (cdr x) out sep-ch mode)) (cdr M))))

(defun write-lisp-array (A out sep-ch-flag mode)
  (let ((sep-ch (get-output-sep-ch sep-ch-flag (truename out))) (d (array-dimensions A)))
    (write-lisp-array-helper A d '() out sep-ch mode)))

(defun write-lisp-array-helper (A d indices out sep-ch mode)
  (cond ((equalp (length d) 1)
      (let ((L '()))
        (sloop for i from 0 to (- (car d) 1) do
          (let ((x (apply 'aref (append (list A) (reverse (cons i indices))))))
            (setq L (cons x L))))
        (write-list-lowlevel (reverse L) out sep-ch mode)))
    (t
      (sloop for i from 0 to (- (car d) 1) do
        (write-lisp-array-helper A (cdr d) (cons i indices) out sep-ch mode)
        (if (and (eq mode 'text) (> (length d) 2))
          (terpri out))))))

(defun write-maxima-array (A out sep-ch-flag mode)
  (write-lisp-array (symbol-array (mget A 'array)) out sep-ch-flag mode))

(defun write-hashed-array (A out sep-ch-flag mode)
  (let
    ((keys (cdddr (meval (list '($arrayinfo) A))))
     (sep-ch (get-output-sep-ch sep-ch-flag (truename out)))
     L)
    (loop
      (if (not keys) (return))
      (setq L ($arrayapply A (car keys)))
      (cond ((listp L) (pop L))
            (t (setq L (list L))))
      (write-list-lowlevel (append (cdr (pop keys)) L) out sep-ch mode))))

(defun write-list (L out sep-ch-flag mode)
  (let ((sep-ch (get-output-sep-ch sep-ch-flag (truename out))))
    (write-list-lowlevel (cdr L) out sep-ch mode)))

(defun write-list-lowlevel (L out sep-ch mode)
  (setq sep-ch (cond ((symbolp sep-ch) (cadr (exploden sep-ch))) (t sep-ch)))
  (cond ((not (null L))
      (loop 
        (if (not L) (return))
        (let ((e (pop L)))
          (cond (($listp e)
              (write-list-lowlevel (cdr e) out sep-ch mode))
            (t
              (cond
                ((eq mode 'text)
                 (mgrind e out)
                 (cond
                   ((null L) (terpri out))
                   (t (write-char sep-ch out))))
                ((eq mode 'binary)
                 (if ($numberp e)
                   (write-float-64 ($float e) out)
                   (merror "write_data: encountered non-numeric data in binary output")))
                (t
                  (merror "write_data: unrecognized mode")))))))))
  (finish-output out))

(defun get-input-sep-ch (sep-ch-flag file-name)
  (cond
    ((eq sep-ch-flag '$tab)
     (format t "numericalio: separator flag ``tab'' not recognized for input; assume ``space'' instead.~%")
     #\space)
    (t (get-output-sep-ch sep-ch-flag file-name))))

(defun get-output-sep-ch (sep-ch-flag file-name)
  (cond
    ((eq sep-ch-flag '$space) #\space)
    ((eq sep-ch-flag '$tab) #\tab)
    ((or (eq sep-ch-flag '$comma) (eq sep-ch-flag '$csv)) '$\,) ; '$csv is backwards compatibility ... sigh
    ((eq sep-ch-flag '$pipe) '$\|)
    ((eq sep-ch-flag '$semicolon) '$\;)

    ((null sep-ch-flag)
      (cond ((equal (pathname-type file-name) "csv") '$\,)
        (t #\space)))
    (t
      (format t "numericalio: separator flag ~S not recognized; assume ``space''.~%" (stripdollar sep-ch-flag))
      #\space)))

(defun require-string (s)
  (cond
    ((stringp s)
     s)
    ((mstringp s)
     (print-invert-case (stripdollar s)))
    (t
      (merror "numericalio: expected a string, instead found a ~:M" (type-of s)))))
