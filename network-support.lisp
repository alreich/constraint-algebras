;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;                         NETWORK SUPPORT
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------

;(in-package "CONSTRAINT-MAINTENANCE")

;;;----------------------------------------------------------------------

(defstruct (NETWORK
	     (:conc-name get-network-)
	     (:print-function print-network)
	     )
  ALGEBRA				; Constraint algebra of elements of net.
  ARR)					; An array of encoded algebra element sets.

;;; NOTE: The automatically defined function, COPY-NETWORK, is insufficient
;;;       for purposes of this module.  Consequently, it is redefined below.

;;;---------------------------------------------------------------------------

;;; AN "AREF"-LIKE FUNCTION FOR ACCESSING (& DECODING) NETWORK ELEMENTS:

(defun NREF (net row col)
  (funcall (get-algebra-decoder (get-network-algebra net))
	   (aref (get-network-arr net) (1- row) (1- col))))

;;;---------------------------------------------------------------------------

;;; UTILITIES FOR READING NETWORKS:

(defun ELEMENT-OF (element algebra)
  (case (get-algebra-type algebra)
    ((:relation-system :constraint-algebra)
     (member element (get-algebra-rels algebra)))
    (otherwise (typep element (get-algebra-element-type algebra)))))

(defun DETERMINE-NETWORK-TYPE (net-array)
  (let* ((sample-element (caar net-array))
	 (sample-element (if (listp sample-element)
			     (car sample-element)
			     sample-element)))
    (dolist (alg *algebras*)
      (if (element-of sample-element alg)
	  (return alg)))))

(defun DETERMINE-NETWORK-SIZE (net-array)
  (let ((n (length (first net-array))))
    (list n n)))

;;;---------------------------------------------------------------------------

;;; NETWORK READ AND PRINT FUNCTIONS FOR READ MACRO:

(defun PRINT-NETWORK (net stream depth)
  (declare (ignore depth))
  (format stream "#N~S" (decode-array-list
			  (list-array (get-network-arr net))
			  (get-algebra-decoder (get-network-algebra net)))))

(defun CREATE-NETWORK (algebra dimensions encoded-contents)
  (make-network
   :algebra algebra
   :arr (make-array
         dimensions
         :adjustable t
         :element-type 'integer
         :initial-contents encoded-contents)))

(defun READ-NETWORK (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((contents   (read stream))
	 (algebra    (determine-network-type contents))
	 (dimensions (determine-network-size contents)))
    (create-network algebra dimensions (encode-array-list
                                        contents
                                        (get-algebra-encoder algebra)))))

;;; The following function, COPY-NETWORK, redefines the function that
;;; was automatically defined when the network structure was defined.
;;; The reason for this redefinition is that the latter copy function
;;; doesn't go the distance and copy the internal contents of a network;
;;; The function below does.

(defun COPY-NETWORK (network)
  "Return a copy of the input network.  Network copy will have same
   algebra but a copy of the array."
  (let ((net-array (get-network-arr network)))
    (create-network
     (get-network-algebra network)
     (array-dimensions net-array)
     (list-array net-array))))

(SET-DISPATCH-MACRO-CHARACTER #\# #\N #'READ-NETWORK)

;;;---------------------------------------------------------------------------

(defun EQUAL-NETS-P (net1 net2)
  (let* ((arr1     (get-network-arr net1  ))
	 (row-dim1 (array-dimension arr1 0))
	 (col-dim1 (array-dimension arr1 1))
	 (arr2     (get-network-arr net2  ))
	 (row-dim2 (array-dimension arr2 0))
	 (col-dim2 (array-dimension arr2 1))
	 (ret-val  t)
	 (alg1     (get-network-algebra   net1))
	 (alg-nam1 (get-algebra-name      alg1))
	 (alg-nam2 (get-algebra-name
		     (get-network-algebra net2)))
	 (eq-fnc   (get-algebra-equal-p   alg1)))
    (if (or (/= row-dim1 row-dim2)
	    (/= col-dim1 col-dim2)
	    (not (equal alg-nam1 alg-nam2)))
	nil
	(dotimes (i row-dim1 ret-val)
	  (dotimes (j col-dim1)
	    (setq ret-val
		  (and ret-val
		       (funcall eq-fnc
				(aref arr1 i j)
				(aref arr2 i j)))))))))

(defun SKEW-OF-P (net1 net2)			; <--- DOESN'T WORK FOR DURATION NETS YET.
  (let* ((arr1     (get-network-arr net1  ))
	 (row-dim1 (array-dimension arr1 0))
	 (col-dim1 (array-dimension arr1 1))
	 (arr2     (get-network-arr net2  ))
	 (row-dim2 (array-dimension arr2 0))
	 (col-dim2 (array-dimension arr2 1))
	 (ret-val  t)
	 (alg1     (get-network-algebra   net1))
	 (alg-nam1 (get-algebra-name      alg1))
	 (alg-nam2 (get-algebra-name
		     (get-network-algebra net2)))
	 (eq-fnc   (get-algebra-equal-p   alg1))
	 (inv      (get-algebra-inverter  alg1)))
    (if (or (/= row-dim1 col-dim2)
	    (/= row-dim2 col-dim1)
	    (not (equal alg-nam1 alg-nam2)))
	nil
	(dotimes (i row-dim1 ret-val)
	  (dotimes (j col-dim1)
	    (setq ret-val
		  (and ret-val
		       (funcall eq-fnc
				(aref arr1 i j)
				(funcall inv (aref arr2 j i))))))))))

(defun INVERSE-NET-OF-P (net1 net2)		; <--- DOESN'T WORK FOR DURATION NETS YET.
  (let* ((arr1     (get-network-arr net1  ))
	 (row-dim1 (array-dimension arr1 0))
	 (col-dim1 (array-dimension arr1 1))
	 (arr2     (get-network-arr net2  ))
	 (row-dim2 (array-dimension arr2 0))
	 (col-dim2 (array-dimension arr2 1))
	 (ret-val  t)
	 (alg1     (get-network-algebra   net1))
	 (alg-nam1 (get-algebra-name      alg1))
	 (alg-nam2 (get-algebra-name
		     (get-network-algebra net2)))
	 (eq-fnc   (get-algebra-equal-p   alg1))
	 (inv      (get-algebra-inverter  alg1))
	 (psize    (/ row-dim1 2)))		;Partition size (= 2 for period nets)
    (if (or (/= row-dim1 col-dim1 row-dim2 col-dim2)
	    (not (equal alg-nam1 alg-nam2)))
	nil
	(doloop i psize (1- row-dim1)
	  (doloop j 0 (1- psize)
	    (setq ret-val
		  (and ret-val
		       (funcall eq-fnc
				(aref arr1 i j)
				(funcall inv (aref arr2 j i))))))))))

(defun CONSISTENTP (net)
  (let* ((arr (get-network-arr net))
	 (ret-val t))
    (dotimes (i (array-dimension arr 0) ret-val)
      (dotimes (j (array-dimension arr 1))
	(if (zerop (aref arr i j))
	    (setq ret-val nil))))))

;;;----------------------------------------------------------------------

(defun EXPAND-NETWORK (net equality-type &optional (incr 1))
  (let* ((net-arr    (get-network-arr      net))
         (algebra    (get-network-algebra  net))
	 (equal-elem (get-equality-element algebra equality-type))
         (dimensions (array-dimensions     net-arr))
         (zero-elem  (get-algebra-zero     algebra))
	 (dimension  (first dimensions)))
    (adjust-array net-arr
                  (mapcar #'(lambda (dim)
                              (+ dim incr))
                          dimensions)
                  :element-type (array-element-type net-arr)
                  :initial-element zero-elem)
    (doloop index dimension (+ dimension incr -1)
	    (setf (aref net-arr index index) equal-elem))
    net))

;;;----------------------------------------------------------------------

(defun CREATE-CONSTRAINT-MATRIX (algebra-name object-type)
  (let* ((algebra (find-algebra algebra-name))
	 (eq-elem (if object-type
                    (get-equality-element algebra object-type)
                    (cdr (first (get-algebra-equalities algebra))))))
    (create-network algebra (list 1 1) (list (list eq-elem)))))

;;;----------------------------------------------------------------------
;;;                           END OF FILE
;;;----------------------------------------------------------------------
