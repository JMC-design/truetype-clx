(defpackage :truetype-clx
  (:use :cl :zpb-ttf)
  (:export *DPI-X*
	   *DPI-Y*
	   BASELINE-TO-BASELINE

	   TEXT-BOUNDING-BOX
           TEXT-LINE-BOUNDING-BOX

	   TEXT-WIDTH
	   TEXT-HEIGHT

	   TEXT-LINE-HEIGHT
	   TEXT-LINE-WIDTH
	   
	   TEXT-PIXARRAY
	   TEXT-LINE-PIXARRAY
	   
           FONT-ASCENT
	   FONT-DESCENT
	   FONT-LINE-GAP
	   
	   UNITS->PIXELS-X
	   UNITS->PIXELS-Y))
(in-package :truetype-clx)

;;;;Defaults to be set by user or system
(defparameter *dpi-x* 144)
(defparameter *dpi-y* 144)
(defparameter *inch/pts* 72 "How many pts per inch. CSS/android is 96")

;;; Font metrics

(defun pt->pixels (pt-size dpi units/em)
  "px = funits*coeff. Function returns coeff."
  (* (* pt-size (/ dpi *inch/pts*))(/ units/em)))

(defun font-ascent (font pt dpi)
  "Returns ascent of @var{font}."
  (with-font-loader (loader font)
    (ceiling (* (pt->pixels pt dpi (zpb-ttf:units/em loader))(zpb-ttf:ascender loader)))))

(defun font-descent (font pt dpi)
  "Returns descent of @var{font}."
  (with-font-loader (loader font)
    (floor (* (pt->pixels pt dpi (zpb-ttf:units/em loader))(zpb-ttf:descender loader)))))

(defun font-line-gap (font pt dpi)
  "Returns line gap of @var{font}."
  (with-font-loader (loader font)
    (ceiling (* (pt->pixels pt dpi (zpb-ttf:units/em loader)) (zpb-ttf:line-gap loader)))))

;;; baseline-to-baseline = ascent - descent + line gap
(defun baseline-to-baseline (font pt dpi)
  "Returns distance between baselines of @var{font}. ascent - descent + line gap"
  (+ (font-ascent font pt dpi) (- (font-descent font pt dpi))
     (font-line-gap font pt dpi)))

(defun text-bounding-box (font pt-size string dpi-x dpi-y &key (underline nil) (overline nil))
  "Returns text bounding box. Text bounding box is only for contours. Bounding box for space (#x20) is zero."
  (zpb-ttf:with-font-loader (font-loader font)
    (let* ((units/em (zpb-ttf:units/em font-loader))
	   (bbox  (zpb-ttf:string-bounding-box string font-loader))
	   (units->pixels-x (pt->pixels pt-size dpi-x units/em))
	   (units->pixels-y (pt->pixels pt-size dpi-y units/em))
	   (xmin (zpb-ttf:xmin bbox))
	   (ymin (zpb-ttf:ymin bbox))
	   (xmax (zpb-ttf:xmax bbox))
	   (ymax (zpb-ttf:ymax bbox)))
      (when underline
	(setf ymin (min ymin (- (zpb-ttf:underline-position font-loader)
				(zpb-ttf:underline-thickness font-loader)))))
      (when overline
	(setf ymax (max ymax (+ (zpb-ttf:ascender font-loader)
				(zpb-ttf:underline-position font-loader)
				(+ (zpb-ttf:underline-thickness font-loader))))))
      (vector (floor (* xmin
			units->pixels-x))
	      (floor (* ymin
			units->pixels-y))
	      (ceiling (* xmax
			  units->pixels-x))
	      (ceiling (* ymax
			  units->pixels-y))))))

(defun text-width (font pt string dpi-x dpi-y)
  "Returns width of text bounding box."
  (let ((bbox (text-bounding-box font pt string dpi-x dpi-y)))
    (- (xmax bbox) (xmin bbox))))

(defun text-height (font pt string dpi-x dpi-y)
  "Returns height of text bounding box."
  (let ((bbox (text-bounding-box font pt string dpi-x dpi-y)))
    (- (ymax bbox) (ymin bbox))))

(defun text-line-bounding-box (font pt-size string dpi-x dpi-y &key (underline nil) (overline nil))
  "Returns text line bounding box. Text line bounding box is bigger than text bounding box. It's height is ascent + descent, width is sum of advance widths minus sum of kernings."
  (with-font-loader (loader font)
    (let* ((units/em (zpb-ttf:units/em loader))
	   (units->pixels-x (pt->pixels pt-size dpi-x units/em))
	   (xmin (zpb-ttf:left-side-bearing (zpb-ttf:find-glyph (elt string 0) loader))
		 )
	   (ymin (font-descent font pt-size dpi-y))
	   (ymax (font-ascent font pt-size dpi-y))
	   (string-length (length string))
	   (xmax (if (> string-length 0)
		     (zpb-ttf:advance-width (zpb-ttf:find-glyph (elt string 0) loader))
		     0)))
      (if (zpb-ttf:fixed-pitch-p loader)
	  (setf xmax (* xmax string-length))
	  (do ((i 1 (1+ i)))
	      ((>= i string-length))
	    (incf xmax
		  (+ (zpb-ttf:advance-width (zpb-ttf:find-glyph (elt string i) loader))
		     (zpb-ttf:kerning-offset (elt string (1- i)) (elt string i) loader)))))
      (vector (floor (* xmin units->pixels-x))
	      ymin
	      (ceiling (* xmax
			  units->pixels-x))
	      ymax))))

(defun text-line-width (font pt string dpi-x dpi-y)
  "Returns width of text line bounding box. It is sum of advance widths minus sum of kernings."
  (let ((bbox (text-line-bounding-box font pt string dpi-x dpi-y)))
    (- (xmax bbox) (xmin bbox))))
(defun text-line-height (font pt string dpi-x dpi-y)
  "Returns height of text line bounding box."
  (let ((bbox (text-line-bounding-box font pt string dpi-x dpi-y)))
    (- (ymax bbox) (ymin bbox))))

(defun xmin (bounding-box)
  "Returns left side x of @var{bounding-box}"
  (typecase bounding-box
    (vector (elt bounding-box 0))))

(defun ymin (bounding-box)
  "Returns bottom side y of @var{bounding-box}"
  (typecase bounding-box
    (vector (elt bounding-box 1))))

(defun xmax (bounding-box)
  "Returns right side x of @var{bounding-box}"
  (typecase bounding-box
    (vector (elt bounding-box 2))))

(defun ymax (bounding-box)
  "Returns top side y of @var{bounding-box}"
  (typecase bounding-box
    (vector (elt bounding-box 3))))

;;; Font rendering 
(defun clamp (value min max)
  "Clamps the value 'value' into the range [min,max]."
  (max min (min max value)))

(defun clamp-alpha (alpha)
  (min 255 (abs alpha)))

(defun make-state (&optional (antialias t))
  "Wrapper around antialising and not antialiasing renderers."
  (if antialias
      (aa:make-state)
      (aa-bin:make-state)))

;;; apparently ripped from aa:update-state, such duplications should be taken care of.
(defun aa-bin/update-state (state paths)
  "Update state for not antialiasing renderer."
  (if (listp paths)
      (dolist (path paths)
        (aa-bin/update-state state path))
      (let ((iterator (paths:path-iterator-segmented paths)))
        (multiple-value-bind (i1 k1 e1) (paths:path-iterator-next iterator)
          (declare (ignore i1))
          (when (and k1 (not e1))
            ;; at least 2 knots
            (let ((first-knot k1))
              (loop
                 (multiple-value-bind (i2 k2 e2) (paths:path-iterator-next iterator)
                   (declare (ignore i2))
                   (aa-bin:line-f state
                           (paths:point-x k1) (paths:point-y k1)
                           (paths:point-x k2) (paths:point-y k2))
                   (setf k1 k2)
                   (when e2
                     (return))))
              (aa-bin:line-f state
                      (paths:point-x k1) (paths:point-y k1)
                      (paths:point-x first-knot) (paths:point-y first-knot)))))))
  state)

(defun update-state (state paths &optional (antialias t))
  "Wrapper around antialising and not antialiasing renderers."
  (if antialias
      (vectors:update-state state paths)
      (aa-bin/update-state state paths)))

(defun cells-sweep (state function &optional function-span (antialias t))
  "Wrapper around antialising and not antialiasig renderers."
  (if antialias
      (aa:cells-sweep state function function-span)
      (aa-bin:cells-sweep state function function-span)))

(defun text-pixarray (font string pt dpi-x dpi-y &key (antialias t) (underline nil) (overline nil) (strikethrough nil))
  "Render a text string of FONT, returning a 2D (unsigned-byte 8) array 
suitable as an alpha mask, and dimensions. This function returns five
values: alpha mask byte array, x-origin, y-origin (subtracted from
position before rendering), horizontal and vertical advances.
Only works for single characters."
  (apply 
   'values 
   (zpb-ttf:with-font-loader (font-loader font)
     (let* ((bbox (text-bounding-box font pt string dpi-x dpi-y))
	    (units/em (zpb-ttf:units/em font-loader))
	    (min-x (xmin bbox))
	    (min-y (ymin bbox))
	    (max-x (xmax bbox))
	    (max-y (ymax bbox))
	    (width  (- max-x min-x))
	    (height (- max-y min-y)))
       (if (or (= 0 width) (= 0 height))
	   (list nil 0 0 0 0)
	   (let* ((units->pixels-x (pt->pixels pt dpi-x units/em))
		  (units->pixels-y (pt->pixels pt dpi-y units/em))
		  (array (make-array (list height width)
				     :initial-element 0
				     :element-type '(unsigned-byte 8)))
		  (state (make-state antialias))
		  (paths (paths-ttf:paths-from-string font-loader string
						      :offset (paths:make-point (- min-x)
										max-y)
						      :scale-x units->pixels-x
						      :scale-y (- units->pixels-y))))
	     
	     (when underline
	       (let* ((thickness (* units->pixels-y (zpb-ttf:underline-thickness font-loader)))
		      (underline-offset (* units->pixels-y (zpb-ttf:underline-position font-loader)))
		      (underline-path (paths:make-rectangle-path 0 (+ max-y (- underline-offset))
								 max-x (+ max-y (- underline-offset) thickness))))
		 (push underline-path paths)))
	     (when strikethrough
	       (let* ((thickness (* units->pixels-y (zpb-ttf:underline-thickness font-loader)))
		      (underline-offset (* 2 units->pixels-y (zpb-ttf:underline-position font-loader)))
		      (line-path (paths:make-rectangle-path 0 (+ max-y underline-offset) max-x (+ max-y underline-offset thickness))))
		 (push line-path paths)))
	     (when overline
	       (let* ((thickness (* units->pixels-y (zpb-ttf:underline-thickness font-loader)))
		      (underline-offset (* units->pixels-y (zpb-ttf:underline-position font-loader)))
		      (ascend (* units->pixels-y (zpb-ttf:ascender font-loader)))
		      (overline-path (paths:make-rectangle-path 0 (- max-y ascend underline-offset)
								max-x
								(- max-y ascend underline-offset thickness))))
		 (push overline-path paths)))
	     (update-state state paths antialias)
	     (cells-sweep state
			  (lambda (x y alpha)
			    (when (and (<= 0 x (1- width))
				       (<= 0 y (1- height)))
			      (setf alpha (min 255 (abs alpha))
				    (aref array y x) (clamp
						      (floor (+ (* (- 256 alpha) (aref array y x))
								(* alpha 255))
							     256)
						      0 255)))) nil antialias)
	     (list array 
		   min-x
		   max-y
		   width
		   height)))))))


(defun text-line-pixarray (font string pt dpi-x dpi-y &key (antialias t) (underline nil) (overline nil) (strikethrough nil))
  "Render a text line of 'face', returning a 2D (unsigned-byte 8) array
suitable as an alpha mask, and dimensions. This function returns five
values: alpha mask byte array, x-origin, y-origin (subtracted from
position before rendering), horizontal and vertical advances."
  (apply 
   'values
   (with-font-loader (font-loader font)
     (let* ((bbox (text-line-bounding-box font pt string dpi-x dpi-y))
	    (units/em (zpb-ttf:units/em font-loader))
	    (min-x (xmin bbox))
	    (min-y (ymin bbox))
	    (max-x (xmax bbox))
	    (max-y (ymax bbox))
	    (width  (- max-x min-x))
	    (height (- max-y min-y)))
       (if (or (= 0 width) (= 0 height))
	   (list nil 0 0 0 0)
	   (let* ((units->pixels-x (pt->pixels pt dpi-x units/em))
		  (units->pixels-y (pt->pixels pt dpi-y units/em))
		  (array (make-array (list height width)
				     :initial-element 0
				     :element-type '(unsigned-byte 8)))
		  (state (make-state antialias))
		  (paths (paths-ttf:paths-from-string font-loader string
						      :offset (paths:make-point (- min-x)
										max-y)
						      :scale-x units->pixels-x
						      :scale-y (- units->pixels-y))))
	     (when underline
	       (let* ((thickness (* units->pixels-y (zpb-ttf:underline-thickness font-loader)))
		      (underline-offset (* units->pixels-y (zpb-ttf:underline-position font-loader)))
		      (underline-path (paths:make-rectangle-path 0 (+ max-y (- underline-offset))
								 max-x (+ max-y (- underline-offset) thickness))))
		 (push underline-path paths)))
	     (when strikethrough
	       (let* ((thickness (* units->pixels-y (zpb-ttf:underline-thickness font-loader)))
		      (underline-offset (* 2 units->pixels-y (zpb-ttf:underline-position font-loader)))
		      (line-path (paths:make-rectangle-path 0 (+ max-y underline-offset) max-x (+ max-y underline-offset thickness))))
		 (push line-path paths)))
	     (when overline
	       (let* ((thickness (* units->pixels-y (zpb-ttf:underline-thickness font-loader)))
		      (underline-offset (* units->pixels-y (zpb-ttf:underline-position font-loader)))
		      (ascend (* units->pixels-y (zpb-ttf:ascender font-loader)))
		      (overline-path (paths:make-rectangle-path 0 (- max-y ascend underline-offset)
								max-x
								(- max-y ascend underline-offset thickness))))
		 (push overline-path paths)))
	     (update-state state paths antialias)
	     (cells-sweep state
			  (lambda (x y alpha)
			    (when (and (<= 0 x (1- width))
				       (<= 0 y (1- height)))
			      (setf alpha (min 255 (abs alpha))
				    (aref array y x) (clamp
						      (floor (+ (* (- 256 alpha) (aref array y x))
								(* alpha 255))
							     256)
						      0 255)))) nil antialias)
	     (list array 
		   min-x
		   max-y
		   width
		   height)))))))
