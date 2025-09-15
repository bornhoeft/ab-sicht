;;;---------------------------------------------------------
;;; AB93 - Plain (Naturell)
;;; Copyright © 2019-20 Achim Bornhoeft
;;;
;;; PWGL Functions and Parameters
;;;
;;;---------------------------------------------------------

;;(defpackage :AB93 (:use :cl))
;; To work with the package AB93 you have to evaluate defpackage first.

;;(in-package :AB93)

;;; instrument ranges as functions

(defun bass-flute-range (&key (percent nil))
  (let* ((lo 48)
         (hi 84)
         (r (abs (- lo hi)))
         (perc (loop for i from lo to hi by (/ r 100)
                collect (* 1.0 i))))
    (if percent perc (list lo hi))))

;; (bass-flute-range) => 48 84)
;; (bass-flute-range :percent t)

(defun flute-range (&key (percent nil))
  (let* ((lo 60)
         (hi 98)
         (r (abs (- lo hi)))
         (perc (loop for i from lo to hi by (/ r 100)
                collect (* 1.0 i))))
    (if percent perc (list lo hi))))

(defun oboe-range (&key (percent nil))
  (let* ((lo 58)
         (hi 89)
         (r (abs (- lo hi)))
         (perc (loop for i from lo to hi by (/ r 100)
                collect (* 1.0 i))))
    (if percent perc (list lo hi))))

(defun alto-saxophone-range (&key (percent nil))
  (let* ((lo 49)
         (hi 80)
         (r (abs (- lo hi)))
         (perc (loop for i from lo to hi by (/ r 100)
                collect (* 1.0 i))))
    (if percent perc (list lo hi))))

(defun baritone-saxophone-range (&key (percent nil))
  (let* ((lo 37)
         (hi 68)
         (r (abs (- lo hi)))
         (perc (loop for i from lo to hi by (/ r 100)
                collect (* 1.0 i))))
    (if percent perc (list lo hi))))

(defun bass-clarinet-range (&key (percent nil))
  (let* ((lo 36)
         (hi 77)
         (r (abs (- lo hi)))
         (perc (loop for i from lo to hi by (/ r 100)
                collect (* 1.0 i))))
    (if percent perc (list lo hi))))

(defun violin-range (&key (percent nil))
  (let* ((lo 55)
         (hi 105)
         (r (abs (- lo hi)))
         (perc (loop for i from lo to hi by (/ r 100)
                collect (* 1.0 i))))
    (if percent perc (list lo hi))))

(defun viola-range (&key (percent nil))
  (let* ((lo 48)
         (hi 98)
         (r (abs (- lo hi)))
         (perc (loop for i from lo to hi by (/ r 100)
                collect (* 1.0 i))))
    (if percent perc (list lo hi))))

(defun violoncello-range (&key (percent nil))
  (let* ((lo 36)
         (hi 86)
         (r (abs (- lo hi)))
         (perc (loop for i from lo to hi by (/ r 100)
                collect (* 1.0 i))))
    (if percent perc (list lo hi))))



(defun brownian (n weight-list start)
  (let ((wlis (loop for j from 0 below (- (length weight-list) 1) by 2
                append 
                (make-list (nth (+ 1 j) weight-list) 
                           :initial-element (nth j weight-list)) 
                into reslis
                finally (return reslis))))
    (loop repeat n
      with s = start
      collect s into reslis
      do (setf s (+ s (nth (random (- (length wlis) 1)) wlis)))
      finally (return reslis))))

;; (brownian 40 '(1 30 -1 30 2 20 -2 20) 10)

(defun brownian-borders (n weight-list start low high)
  (let ((wlis (loop for j from 0 below (- (length weight-list) 1) by 2
                append 
                (make-list (nth (+ 1 j) weight-list) 
                           :initial-element (nth j weight-list)) 
                into reslis
                finally (return reslis))))
    (loop repeat n
      with s = start
      do (setf s (if (< s low) 
                     (+ low (abs (- s low)))
                     (if (> s high) 
                         (- high (abs (- s high)))
                         s)))
      collect s into reslis
      do (setf s (+ s (nth (random (length wlis)) wlis)))
      finally (return reslis))))

;; (brownian-borders 50 '(1 40 -1 60 2 20) 10 5 15)

(defun sum-of-2 (n weight-list sum start)
  (let ((wlis 
         (loop for j from 0 below (- (length weight-list) 1) by 2
           append 
           (make-list (nth (+ 1 j) weight-list) 
                      :initial-element (nth j weight-list)) 
           into reslis
           finally (return reslis))))
    (loop
      with s = start
      with l = 0
      append (remove 0 (list s (- sum s))) into reslis
      do (setf s (nth (random (- (length wlis) 1)) wlis))
      do (setf l (length reslis))
      until (>= l n)
      finally (return (if (> l n) (butlast reslis) reslis)))))

;; (sum-of-2 10 '(7 30 3 20 4 20 3.5 30) 7 3)
;; (sum-of-2 11 '(7 30 3 20 4 20 3.5 30) 7 3)

(defun insert-dur (dur cut insdur)
  (let* ((ddur (* 1/3 dur))
         (idur (if (< insdur ddur) insdur ddur))
         ; max insertion duration is 1/3 of dur
         (ndur (- dur idur))
         ; duration - insertion duration (rest)
         (firsdur (* cut ndur))
         ; cut = percent (0-1) of the rest duration
         (secdur (- ndur firsdur))
         ; duration - first duration - insertion duration
         )
    (list firsdur idur secdur)))


;; (insert-dur 3 0.4 0.25)
;; (loop repeat 10 collect (insert-dur 3 (/ (+ 2 (random 7)) 10.0) 0.25))

;;; combination tones
;;; https://en.wikipedia.org/wiki/Combination_tone

(defun comb-tones (freq1 freq2 low-order high-order &key type chords ratios)
  "Combination tones between freq1 and freq2 (Vers 2)."
  (labels ((comb-tones (fq1 fq2 low-odr high-odr)
             "combination tones of freq1 with all ratios low to high order."
             (let* ((start (- low-odr 1))
                    (end (- high-odr 1))
                    (sum-tones (loop for k from start to end
                                 append
                                 (loop for i from k downto 1
                                   for j from 1 to k
                                   collect (+ (* i fq1) 
                                              (* j fq2)))))
                    (difference-tones (loop for k from start to end
                                        append
                                        (loop for i from k downto 1
                                          for j from 1 to k
                                          collect (abs (- (* i fq1) 
                                                          (* j fq2)))))))
               (list sum-tones difference-tones)))
           (transpose-matrix (list-of-lists)
             "Transpose a matrix represented as a list of lists."
             (apply #'mapcar #'list list-of-lists))
           (flatter (obj)
             "Flatten nested lists."
             (do* ((result (list obj))
                   (node result))
                  ((null node) (delete nil result))
               (cond ((consp (car node))
                      (when (cdar node) (push (cdar node) (cdr node)))
                      (setf (car node) (caar node)))
                     (t (setf node (cdr node)))))))
    (let* ((fq2 (if (listp freq2) freq2 (list freq2)))
           (coto (loop for i in fq2
                     collect
                     (comb-tones freq1 (if ratios (* freq1 i) i) 
                   low-order high-order)))
           (combtone-chords (loop for i in coto collect (flatter i)))
           (combtone-voices (transpose-matrix combtone-chords))
           (coto-trans (transpose-matrix coto))
           (sumtone-chords (first coto-trans))
           (sumtone-voices (transpose-matrix sumtone-chords))
           (difftone-chords (second coto-trans))
           (difftone-voices (transpose-matrix difftone-chords))
           (freqs (loop for i in fq2
                          collect freq1 into res1
                          collect (if ratios (* freq1 i) i) into res2
                          finally (return (list res1 res2))))
           (freq-trans (transpose-matrix freqs))
           (comball-chords (loop for i in combtone-chords
                           for j in freq-trans
                           collect (append j i)))
           (comball-voices (append freqs combtone-voices)))
      (case type
        (combtones (if chords combtone-chords combtone-voices))
        (sumtones (if chords sumtone-chords sumtone-voices))
        (difftones (if chords difftone-chords difftone-voices))
        (otherwise (if chords comball-chords comball-voices))))))

;; combination-tones - 1 voice per order plus interval (in frequencies)
;; (comb-tones 400 '(500 600) 2 2)
;; ((400 400) (500 600) (900 1000) (100 200))

;; combination-tones - 1 voice per order plus interval (in ratios)
;; (comb-tones 400 '(5/4 6/4 9/4) 2 2 :ratios t)
;;((400 400 400) (500 600 900) (900 1000 1300) (100 200 500))

;; combination-tones - 1 chord per interval (included) with all orders 
;; (comb-tones 400 '(500 600) 2 3 :chords t)
;; ((400 500 900 1300 1400 100 300 600) (400 600 1000 1400 1600 200 200 800))

;; combination-tones - 1 voice per order
;; (comb-tones 400 '(500 600) 2 3 :type 'combtones)
;; ((900 1000) (1300 1400) (1400 1600) (100 200) (300 200) (600 800))

;; combination-tones - 1 chord per interval with all orders
;; (comb-tones 400 '(500 600) 2 3 :type 'combtones :chords t)
;; ((900 1300 1400 100 300 600) (1000 1400 1600 200 200 800))

;; sum-tones - 1 voice per order
;; (comb-tones 400 '(500 600) 2 3 :type 'sumtones)
;; ((900 1000) (1300 1400) (1400 1600))

;; sum-tones - 1 chord per interval with all orders
;; (comb-tones 400 '(500 600) 2 3 :type 'sumtones :chords t)
;; ((900 1300 1400) (1000 1400 1600))

;; difference-tones - 1 voice per order
;; (comb-tones 400 '(500 600) 2 3 :type 'difftones)
;; ((100 200) (300 200) (600 800))

;; difference-tones - 1 chord per interval with all orders
;; (comb-tones 400 '(500 600) 2 3 :type 'difftones :chords t)
;; ((100 300 600) (200 200 800))

(defun refl (n lo hi)
  "Reflect number at lo hi borders."
  (if (< n lo) (+ lo (abs (- n lo)))
      (if (> n hi) (- hi (abs (- n hi)))
          n)))

;; (refl -3 1 6) 
;; (refl -2 -1 5)
;; (refl -8 -6 -1)
;; (refl 17 5 15)

;;; NOT WORKING, INFINITE LOOP !!!
#|
(defun in-range (n hi lo)
   (loop for s = n
     do (when (< s lo) (setf s (+ lo (abs (- s lo)))))
     do (when (> s hi) (setf s (- hi (abs (- s hi)))))
     do (when (and (>= s lo) (<= s hi)) (setf s n))
     do (print s)
     until (and (>= s lo) (<= s hi))
     finally (return s)))

;; (in-range 3 1 6)
|#

(defun random-sum1 (lst-sum low high)
  "Collects random numbers between low and high until lst-sum. If the result is > lst sum
  the last number is truncated to fit lst-sum."
  (loop
    with s
    collect (+ low (random (- high low))) into reslis
    do (setf s (reduce #'+ reslis))
    until (>= s lst-sum)
    finally (return
             (if (> s lst-sum)
               (append (butlast reslis) 
                       (list (- lst-sum (reduce #'+ (butlast reslis)))))
                      reslis))))

;; (random-sum1 20 2 5)

(defun random-sum2 (lst-sum lis)
  "Try to build a list containing the numbers in lis with the sum of lst-sum."
  (loop
    with sum
    for x = (loop
               with s
               collect (nth (random (length lis)) lis) into reslis
               do (setf s (reduce #'+ reslis))
               until (>= s lst-sum)
               finally (return reslis))
    do (setf sum (reduce #'+ x))
    do (print x)
    until (= sum lst-sum)
    finally (return x)))

;; (random-sum2 20 '(7 3 8))

(defun raster (lis quant-lis)
  (loop for i in lis collect
    (loop for j in quant-lis
      collect (abs (- i j)) into reslis
      finally (return (nth (position (apply #'min reslis) reslis) quant-lis)))))

#|
(raster '(1/12 5/27 31/108 7/18 53/108 16/27 25/36 43/54 97/108 1)
        '(1/12 1/10 1/9 1/8 1/7 1/6 1/5 1/4 1/3 1/2 1 2 3 4))
 => (1/12 1/5 1/4 1/3 1/2 1/2 1/2 1 1 1)
|#  

(defun spec-intrvl (fund start-harm harms harm-intrvl &optional (numb-harm? t))
(if numb-harm?
(loop repeat harms
  with x = start-harm
  for i from 0
  for j = (nth (mod i (length harm-intrvl)) harm-intrvl)
  collect (* fund x)
  do (setf x (+ x j)))
(loop with x = start-harm
  for i from 0
  for j = (nth (mod i (length harm-intrvl)) harm-intrvl)
  collect (* fund x) into reslis
  do (setf x (+ x j))
  until (>= x (+ harms 1))
  finally (return reslis))))

;; (spec-intrvl 10 3 10 '(1 1 9)) => (30 40 50 140 150 160 250 260 270 360) = 10 harmonics
;; (spec-intrvl 10 3 14 '(1 1 9) nil) => (30 40 50 140) = harmonics 3 4 5 and 14
;; (spec-intrvl 10 1 10 '(1) nil) => (10 20 30 40 50 60 70 80 90 100) = upto the 10. harmonic
