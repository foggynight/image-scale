;;;; image-scale.scm - Scale an image using nearest neighbour interpolation.
;;
;; Procedures in this program use the following terms when referring to pixel
;; colour values:
;; - pixel: Triple of numbers representing the separate RGB values of a pixel.
;; - RGB: Single integer representing the combined RGB values of a pixel.

(import (chicken bitwise)
        (chicken format)
        (chicken process-context)
        (srfi 4)
        (srfi 141)
        stb-image
        stb-image-write)

;; Convert a pixel into an RGB value.
(: pixel->rgb (fixnum fixnum fixnum --> fixnum))
(define (pixel->rgb r g b)
  (+ (arithmetic-shift (+ (arithmetic-shift r 8) g) 8) b))

;; Convert an RGB value into a pixel.
(: rgb->pixel (fixnum --> fixnum fixnum fixnum))
(define (rgb->pixel rgb)
  (values (bitwise-and (arithmetic-shift rgb -16) #xFF)
          (bitwise-and (arithmetic-shift rgb -8) #xFF)
          (bitwise-and rgb #xFF)))

;; Get a newly allocated vector of RGB values containing the RGB values of SRC
;; scaled from size W1xH1 to W2xH2 using nearest neighbour interpolation.
;;
;; This code is based off code listed at:
;; <https://tech-algorithm.com/articles/nearest-neighbor-image-scaling>
;; as of 2022-01-24.
(: scale-rgb ((vector-of fixnum) fixnum fixnum fixnum fixnum
              --> (vector-of fixnum)))
(define (scale-rgb src w1 h1 w2 h2)
  (define dest (make-vector (* w2 h2) 0))
  (define x_ratio (+ (floor/ (arithmetic-shift w1 16) w2) 1))
  (define y_ratio (+ (floor/ (arithmetic-shift h1 16) h2) 1))
  (do ((i 0 (+ i 1)))
      ((>= i h2))
    (do ((j 0 (+ j 1)))
        ((>= j w2))
      (let ((x2 (arithmetic-shift (* j x_ratio) -16))
            (y2 (arithmetic-shift (* i y_ratio) -16)))
        (vector-set! dest (+ (* i w2) j)
                     (vector-ref src (+ (* y2 w1) x2))))))
  dest)

(: main (string string fixnum fixnum -> void))
(define (main in-path out-path out-width out-height)
  (define-values (pixels width height channels)
    (with-input-from-file in-path read-image))
  (set! pixels
    (let* ((rgb-lst (let loop ((lst (u8vector->list pixels)))
                      (if (null? lst)
                          '()
                          (cons (pixel->rgb (car lst) (cadr lst) (caddr lst))
                                (loop (cdddr lst))))))
           (rgb-vec (list->vector rgb-lst))
           (scaled-rgb-vec (scale-rgb rgb-vec width height
                                      out-width out-height))
           (scaled-rgb-lst (vector->list scaled-rgb-vec))
           (scaled-pix-lst (let loop ((lst scaled-rgb-lst))
                             (if (null? lst)
                                 '()
                                 (let-values (((r g b) (rgb->pixel (car lst))))
                                   (append (list r g b) (loop (cdr lst))))))))
      (list->u8vector scaled-pix-lst)))
  (with-output-to-file out-path
    (lambda () (write-png pixels out-width out-height 3))))

(let ((args (command-line-arguments))
      (err (lambda (msg) (printf "image-scale: ~A~%" msg) (exit 1))))
  (cond ((not (= (length args) 4)) (err "invalid number of arguments"))
        ((or (not (string->number (caddr args)))
             (not (string->number (cadddr args))))
         (err "invalid scale argument"))
        (else (main (car args)
                    (cadr args)
                    (string->number (caddr args))
                    (string->number (cadddr args))))))
