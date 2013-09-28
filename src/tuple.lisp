;;; -*- Mode: Lisp -*-

(in-package :foundationdb)

(defclass tuple ()
  ((items :initarg :items)
   (bytes :initform nil)
   (decoding-bytes :initform nil)
   (decoding-index)))

(defun make-tuple (&rest items)
  (make-instance 'tuple :items (make-array (length items) 
                                           :initial-contents items
                                           :fill-pointer t :adjustable t)))

(defmethod print-object ((obj tuple) stream)
  (print-unreadable-object (obj stream :type t)
    (finish-decoding obj)
    (with-slots (items) obj
      (write items :stream stream))))
      
(defun tuple-push (tuple item)
  (finish-decoding tuple)
  (with-slots (items bytes) tuple
    (vector-push-extend item items)
    (setf bytes nil)))

(defun tuple-elt (tuple index)
  (finish-decoding tuple index)
  (with-slots (items) tuple
    (aref items index)))

(defun tuple-subtuple (tuple start &optional end)
  (finish-decoding tuple end)
  (with-slots (items) tuple
    (when (null end)
      (setq end (length items)))
    (let ((nitems (make-array (- end start) :fill-pointer t :adjustable t)))
      (loop for i from start below end 
            for j from 0
            do (setf (aref nitems j) (aref items i)))
      (make-instance 'tuple :items nitems))))

(defun tuple-bytes (tuple)
  (with-slots (bytes items) tuple
    (when (null bytes)
      (finish-decoding tuple)           ; For when started with subsequence bytes.
      (setf bytes (encode items)))
    bytes))

(defun tuple-length (tuple)
  (finish-decoding tuple)
  (with-slots (items) tuple
    (length items)))

(defun tuple-equalp (tuple1 tuple2)
  (eq (compare-bytes tuple1 tuple2) :equal))

(defun tuple-not-equalp (tuple1 tuple2)
  (not (eq (compare-bytes tuple1 tuple2) :equal)))

(defun tuple-greaterp (tuple1 tuple2)
  (eq (compare-bytes tuple1 tuple2) :greater))

(defun tuple-not-greaterp (tuple1 tuple2)
  (not (eq (compare-bytes tuple1 tuple2) :greater)))

(defun tuple-lessp (tuple1 tuple2)
  (eq (compare-bytes tuple1 tuple2) :less))

(defun tuple-not-lessp (tuple1 tuple2)
  (not (eq (compare-bytes tuple1 tuple2) :less)))

(defun tuple-encode (&rest items)
  (cond ((null items) nil)
        ((and (null (rest items))
              (typep (first items) 'tuple))
         (tuple-bytes (first items)))
        (t
         (encode items))))

(defun tuple-encode-item (item &key bytes (index 0) prefix)
  (encode item bytes index prefix))

(defun tuple-encoded-length (tuple)
  (length (tuple-bytes tuple)))         ; TODO: Could optimize when bytes not yet done.

(defun tuple-decode (bytes &key (start 0) end)
  (when (null end)
    (setq end (length bytes)))
  (let ((tuple (make-instance 'tuple)))
    (with-slots ((tbytes bytes) decoding-bytes decoding-index items) tuple
      ;; Need zero-based vector that we can modify for nul unescaping.
      (if (and (= start 0) (= end (length bytes)) (null (find #xFF bytes)))
          (setf tbytes bytes
                decoding-bytes bytes)
          (setf decoding-bytes (subseq bytes start end)))
      (setf decoding-index 0)
      (setf items (make-array (decoded-length decoding-bytes) 
                              :fill-pointer 0 :adjustable t)))
    tuple))

;;; Decode more of decoding-bytes, at least up to given items index.
(defun finish-decoding (tuple &optional limit)
  (with-slots (decoding-bytes decoding-index items) tuple
    (loop while (and (not (null decoding-bytes))
                     (or (null limit)
                         (<= (fill-pointer items) limit)))
      do (multiple-value-bind (item index)
             (decode decoding-bytes decoding-index)
           (vector-push-extend item items)
           (if (>= index (length decoding-bytes))
               (setf decoding-bytes nil
                     decoding-index nil)
               (setf decoding-index index))))))
      
(defun compare-bytes (tuple1 tuple2)
  (let* ((bytes1 (tuple-bytes tuple1))
         (length1 (length bytes1))
         (bytes2 (tuple-bytes tuple2))
         (length2 (length bytes2))
         (index 0))
    (loop
      (cond ((= index length1)
             (return (if (= index length2) :equal :less)))
            ((= index length2)
             (return :greater))
            (t
             (let ((byte1 (aref bytes1 index))
                   (byte2 (aref bytes2 index)))
               (cond ((= byte1 byte2))
                     ((< byte1 byte2) (return :less))
                     (t (return :greater))))))
        (incf index))))
    
;;; Simple encoding scheme:
;;; 1 - byte array, nul-terminated, nuls escaped by following FF
;;; 2 - UTF-8 stream, similarly
;;; (20 + n) - non-negative integer end-endian as n bytes
;;; (20 - n) - negative integer, similarly

;;; Encoding routines accept tuples and sequences and flatten for convenience.

(defun encoded-length (value)
  (etypecase value
    (string
     (+ (babel:string-size-in-octets value) (count #\nul value) 2))
    ((array (unsigned-byte 8) (*))
     (+ (length value) (count 0 value) 2))
    (integer
     (1+ (ceiling (integer-length (abs value)) 8)))
    (tuple
     (tuple-encoded-length value))
    (list
     (let ((total 0))
       (dolist (item value)
         (incf total (encoded-length item)))
       total))
    (vector
     (let ((total 0))
       (dotimes (i (length value))
         (incf total (encoded-length (aref value i))))
       total))))
          
(defun encode (value &optional bytes (index 0) prefix)
  (when (null bytes)
    (setq bytes (make-array (+ (encoded-length value) (length prefix))
                            :element-type '(unsigned-byte 8))))
  (macrolet ((add-byte (b)
               `(progn
                 (setf (aref bytes index) ,b)
                 (incf index))))
    (when prefix
      (dotimes (i (length prefix))
        (add-byte (elt prefix i))))
    (flet ((append-and-escape-nuls (src)
             (dotimes (i (length src))
               (let ((b (aref src i)))
                 (add-byte b)
                 (when (zerop b)
                   (add-byte #xFF))))
             (add-byte #x00)))
      (etypecase value
        (string
         (add-byte #x02)
         ;; No exported way to encode to array.
         (append-and-escape-nuls (babel:string-to-octets value)))
        ((array (unsigned-byte 8) (*))
         (add-byte #x01)
         (append-and-escape-nuls value))
        (integer
         (if (minusp value)
             (let* ((n (ceiling (integer-length (- value)) 8))
                    (v (+ value (1- (ash 1 (* n 8))))))
               (add-byte (- #x14 n))
               (dotimes (i n)
                 (add-byte (ldb (byte 8 (* 8 (- n i 1))) v))))
             (let ((n (ceiling (integer-length value) 8)))
               (add-byte (+ #x14 n))
               (dotimes (i n)
                 (add-byte (ldb (byte 8 (* 8 (- n i 1))) value))))))
        (tuple
         (let ((tbytes (tuple-bytes value)))
           (dotimes (i (length tbytes))
             (add-byte (aref tbytes i)))))
        (list
         (dolist (item value)
           (multiple-value-setq (bytes index) (encode item bytes index))))
        (vector
         (dotimes (i (length value))
           (multiple-value-setq (bytes index) (encode (aref value i) bytes index)))))))
  (values bytes index))

(defun decoded-length (bytes &optional (start 0) (end (length bytes)))
  (let ((count 0)
        (index start))
    (loop while (< index end) 
      do (let ((code (aref bytes index)))
           (incf index)
           (cond ((or (= code #x01) (= code #x02))
                  (loop
                    (let ((b (aref bytes index)))
                      (incf index)
                      (when (zerop b)
                        (if (and (< index end) (= (aref bytes index) #xFF))
                            (incf index)
                            (return))))))
                 ((<= #x0C code #x13)
                  (incf index (- #x14 code)))
                 ((<= #x14 code #x1C)
                  (incf index (- code #x14)))
                 (t
                  (error "Unrecognized code ~X" code)))
           (incf count)))
    count))

(defun unescape-nuls (from index end)
  (let ((rp index)
        (wp index))
    (loop
       (let ((b (aref from rp)))
         (when (zerop b)
           (incf rp)
           (unless (and (< rp end) (= (aref from rp) #xFF))
             (return (values wp rp))))
         (unless (= rp wp)
           (setf (aref from wp) b))
         (incf rp)
         (incf wp)))))

(defun decode (bytes &optional (start 0) (end (length bytes)))
  (let ((code (aref bytes start))
        (index (1+ start))
        value)
    (cond ((= code #x01)
           (multiple-value-bind (string-end nindex)
               (unescape-nuls bytes index end)
             (setq value (subseq bytes index string-end)
                   index nindex)))
          ((= code #x02)
           (multiple-value-bind (string-end nindex)
               (unescape-nuls bytes index end)
             (setq value (babel:octets-to-string bytes :start index :end string-end)
                   index nindex)))
          ((<= #x14 code #x1C)
           (let ((n (- code #x14)))
             (setq value 0)
             (dotimes (i n)
               (setf (ldb (byte 8 (* 8 (- n i 1))) value) (aref bytes index))
               (incf index))))
          ((<= #x0C code #x13)
           (let ((n (- #x14 code)))
             (setq value 0)
             (dotimes (i n)
               (setf (ldb (byte 8 (* 8 (- n i 1))) value) (aref bytes index))
               (incf index))
             (decf value (1- (ash 1 (* n 8))))))
          (t
           (error "Unrecognized code ~X" code)))
    (values value index)))
