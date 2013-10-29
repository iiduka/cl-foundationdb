;;; -*- Mode: Lisp -*-

(in-package :foundationdb)

(defclass tuple ()
  ((items :initarg :items)
   (octets :initform nil)
   (decoding-octets :initform nil)
   (decoding-index)))

(defun make-tuple (&rest items)
  (make-instance 'tuple :items (make-array (length items) 
                                           :initial-contents items
                                           :fill-pointer t :adjustable t)))

;;; Use when want fully decoded, not as accessor.
(defun tuple-items (tuple)
  (finish-decoding tuple)
  (slot-value tuple 'items))
  
(defmethod print-object ((obj tuple) stream)
  (print-unreadable-object (obj stream :type t)
    (write (tuple-items obj) :stream stream)))

(defun copy-tuple (tuple)
  (finish-decoding tuple)
  (with-slots (items) tuple
    (make-instance 'tuple :items (make-array (length items) 
                                             :initial-contents items
                                             :fill-pointer t :adjustable t))))

(defun tuple-push (tuple item)
  (finish-decoding tuple)
  (with-slots (items octets) tuple
    (vector-push-extend item items)
    (setf octets nil)))

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

(defun tuple-octets (tuple)
  (with-slots (octets items) tuple
    (when (null octets)
      (finish-decoding tuple)           ; For when started with subsequence octets.
      (setf octets (encode items)))
    octets))

(defun tuple-length (tuple)
  (finish-decoding tuple)
  (with-slots (items) tuple
    (length items)))

(defun tuple-equalp (tuple1 tuple2)
  (eq (compare-octets tuple1 tuple2) :equal))

(defun tuple-not-equalp (tuple1 tuple2)
  (not (eq (compare-octets tuple1 tuple2) :equal)))

(defun tuple-greaterp (tuple1 tuple2)
  (eq (compare-octets tuple1 tuple2) :greater))

(defun tuple-not-greaterp (tuple1 tuple2)
  (not (eq (compare-octets tuple1 tuple2) :greater)))

(defun tuple-lessp (tuple1 tuple2)
  (eq (compare-octets tuple1 tuple2) :less))

(defun tuple-not-lessp (tuple1 tuple2)
  (not (eq (compare-octets tuple1 tuple2) :less)))

(defun tuple-encode (&rest items)
  (cond ((null items) nil)
        ((and (null (rest items))
              (typep (first items) 'tuple))
         (tuple-octets (first items)))
        (t
         (encode items))))

(defun tuple-encode-item (item &key octets (index 0) prefix)
  (encode item octets index prefix))

(defun tuple-encoded-length (tuple)
  (length (tuple-octets tuple)))         ; TODO: Could optimize when octets not yet done.

(defun tuple-decode (octets &key (start 0) end)
  (when (null end)
    (setq end (length octets)))
  (let ((tuple (make-instance 'tuple)))
    (with-slots ((toctets octets) decoding-octets decoding-index items) tuple
      ;; Need zero-based vector that we can modify for nul unescaping.
      (if (and (= start 0) (= end (length octets)) (null (find #xFF octets)))
          (setf toctets octets
                decoding-octets octets)
          (setf decoding-octets (subseq octets start end)))
      (setf decoding-index 0)
      (setf items (make-array (decoded-length decoding-octets) 
                              :fill-pointer 0 :adjustable t)))
    tuple))

;;; Decode more of decoding-octets, at least up to given items index.
(defun finish-decoding (tuple &optional limit)
  (with-slots (decoding-octets decoding-index items) tuple
    (loop while (and (not (null decoding-octets))
                     (or (null limit)
                         (<= (fill-pointer items) limit)))
      do (multiple-value-bind (item index)
             (decode decoding-octets decoding-index)
           (vector-push-extend item items)
           (if (>= index (length decoding-octets))
               (setf decoding-octets nil
                     decoding-index nil)
               (setf decoding-index index))))))
      
(defun compare-octets (tuple1 tuple2)
  (let* ((octets1 (tuple-octets tuple1))
         (length1 (length octets1))
         (octets2 (tuple-octets tuple2))
         (length2 (length octets2))
         (index 0))
    (loop
      (cond ((= index length1)
             (return (if (= index length2) :equal :less)))
            ((= index length2)
             (return :greater))
            (t
             (let ((octet1 (aref octets1 index))
                   (octet2 (aref octets2 index)))
               (cond ((= octet1 octet2))
                     ((< octet1 octet2) (return :less))
                     (t (return :greater))))))
        (incf index))))
    
;;; Simple encoding scheme:
;;; 1 - octet array, nul-terminated, nuls escaped by following FF
;;; 2 - UTF-8 stream, similarly
;;; (20 + n) - non-negative integer end-endian as n octets
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
          
(defun encode (value &optional octets (index 0) prefix)
  (when (null octets)
    (setq octets (make-array (+ (encoded-length value) (length prefix))
                             :element-type '(unsigned-byte 8))))
  (macrolet ((add-octet (b)
               `(progn
                 (setf (aref octets index) ,b)
                 (incf index))))
    (when prefix
      (dotimes (i (length prefix))
        (add-octet (elt prefix i))))
    (flet ((append-and-escape-nuls (src)
             (dotimes (i (length src))
               (let ((b (aref src i)))
                 (add-octet b)
                 (when (zerop b)
                   (add-octet #xFF))))
             (add-octet #x00)))
      (etypecase value
        (string
         (add-octet #x02)
         ;; No exported way to encode to array.
         (append-and-escape-nuls (babel:string-to-octets value)))
        ((array (unsigned-byte 8) (*))
         (add-octet #x01)
         (append-and-escape-nuls value))
        (null
         (add-octet #x00))
        (integer
         (if (minusp value)
             (let* ((n (ceiling (integer-length (- value)) 8))
                    (v (+ value (1- (ash 1 (* n 8))))))
               (add-octet (- #x14 n))
               (dotimes (i n)
                 (add-octet (ldb (byte 8 (* 8 (- n i 1))) v))))
             (let ((n (ceiling (integer-length value) 8)))
               (add-octet (+ #x14 n))
               (dotimes (i n)
                 (add-octet (ldb (byte 8 (* 8 (- n i 1))) value))))))
        (tuple
         (let ((toctets (tuple-octets value)))
           (dotimes (i (length toctets))
             (add-octet (aref toctets i)))))
        (list
         (dolist (item value)
           (multiple-value-setq (octets index) (encode item octets index))))
        (vector
         (dotimes (i (length value))
           (multiple-value-setq (octets index) (encode (aref value i) octets index)))))))
  (values octets index))

(defun decoded-length (octets &optional (start 0) (end (length octets)))
  (let ((count 0)
        (index start))
    (loop while (< index end) 
      do (let ((code (aref octets index)))
           (incf index)
           (cond ((or (= code #x01) (= code #x02))
                  (loop
                    (let ((b (aref octets index)))
                      (incf index)
                      (when (zerop b)
                        (if (and (< index end) (= (aref octets index) #xFF))
                            (incf index)
                            (return))))))
                 ((= code #x00))
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

(defun decode (octets &optional (start 0) (end (length octets)))
  (let ((code (aref octets start))
        (index (1+ start))
        value)
    (cond ((= code #x01)
           (multiple-value-bind (string-end nindex)
               (unescape-nuls octets index end)
             (setq value (subseq octets index string-end)
                   index nindex)))
          ((= code #x02)
           (multiple-value-bind (string-end nindex)
               (unescape-nuls octets index end)
             (setq value (babel:octets-to-string octets :start index :end string-end)
                   index nindex)))
          ((= code #x00)
           (setq value nil))
          ((<= #x14 code #x1C)
           (let ((n (- code #x14)))
             (setq value 0)
             (dotimes (i n)
               (setf (ldb (byte 8 (* 8 (- n i 1))) value) (aref octets index))
               (incf index))))
          ((<= #x0C code #x13)
           (let ((n (- #x14 code)))
             (setq value 0)
             (dotimes (i n)
               (setf (ldb (byte 8 (* 8 (- n i 1))) value) (aref octets index))
               (incf index))
             (decf value (1- (ash 1 (* n 8))))))
          (t
           (error "Unrecognized code ~X" code)))
    (values value index)))
