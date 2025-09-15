;;; ---------------------------------------------------------
;;; Sicht
;;; Copyright © 2015-2021 Achim Bornhoeft
;;; ---------------------------------------------------------

;;; Lisp Functions

(defun reciproc (ratios)
  "Return the recirpcal of ratios.

  Examples:
  (reciproc 5/4) => (4/5)
  (reciproc '(3/4 5/7 6/3 8/9)) => (4/3 7/5 1/2 9/8)"
  
  (let ((r (if (numberp ratios) (list ratios) ratios)))
    (loop for i in r collect (/ 1 i))))

(reciproc '(3/4 5/7 6/3 8/9))

(defun sum-tones (fq1 fq2 lo hi)
  "Sumtones of freq 1 and freq 2 with all ratios low to high order.
  
  Examples:
  (sum-tones 400 500 2 3) => (900 1300 1400)"

  (let* ((start (- lo 1))
         (end (- hi 1)))
    (loop for k from start to end
      append
      (loop for i from k downto 1
        for j from 1 to k
        collect (+ (* i fq1) 
                   (* j fq2))))))

(defun diff-tones (fq1 fq2 lo hi)
  "difference tones of freq 1 and freq 2 with all ratios low to high order.

  Examples:
  (diff-tones 400 500 2 3) => (100 300 600)"
  (let* ((start (- lo 1))
         (end (- hi 1)))
    (loop for k from start to end
      append
      (loop for i from k downto 1
        for j from 1 to k
        collect (abs (- (* i fq1) 
                        (* j fq2)))))))

#|
(defun diff-fix-tones (diff fq1 lo hi)
  "Calculating all f2 when diff-tone and f1 known.
  
  Example:
  (diff-fix-tones  200 500 1 3)  
  => (200 500 300.0 150.0 800.0 100.0 400.0 1300.0)"

  #| Test:
  1 * 500 - 1 * 300 = 200
  1 * 500 - 2 * 150 = 200 
  2 * 500 - 1 * 800 = 200
  1 * 500 - 3 * 100 = 200
  2 * 500 - 2 * 400 = 200  
  3 * 500 - 1 * 1300 = 200
  |# 

  (append (list diff fq1)
          (loop for k from lo to hi
            append
            (loop for i from k downto 1
              for j from 1 to k
              collect (* 1.0 (/ (- (* fq1 j)
                                   diff) i))))))
|#

(defun comb-fix-tones (diff fq1 lo hi)
  "Calculating all f2 when sum-tone and f1 known.
  
  Example:
  (comb-fix-tones  200 500 1 3)  
  => (200 500 300.0 150.0 800.0 100.0 400.0 1300.0)
  
  Test:
  1 * 500 - 1 * 300 = 200
  1 * 500 - 2 * 150 = 200 
  2 * 500 - 1 * 800 = 200
  1 * 500 - 3 * 100 = 200
  2 * 500 - 2 * 400 = 200  
  3 * 500 - 1 * 1300 = 200
  
  Example:
  (comb-fix-tones 800 200 1 3)  
  => (800 200 600.0 300.0 400.0 200.0 200.0 200.0)
  
  Test:
  1 * 200 + 1 * 600 = 800
  1 * 200 + 2 * 300 = 800
  2 * 200 + 1 * 400 = 800
  1 * 200 + 3 * 200 = 800
  2 * 200 + 2 * 200 = 800  
  3 * 200 + 1 * 200 = 800
  " 
  
  (append (list diff fq1)
          (loop for k from lo to hi
            append
            (loop for i from k downto 1
              for j from 1 to k
              collect (* 1.0 (/ (abs (- (* fq1 j)
                                        diff)) i))))))

#!
(defun diff-fix-tones (diff fq1 lo hi)
  "Calculating all f2 when diff-tone and f1 known."
  (append (list diff fq1)
          (loop for k from lo to hi
            append
            (loop for i from k downto 1
              for j from 1 to k
              collect (* 1.0 (/ (- (* fq1 j) diff) i))))))

;; (diff-fix-tones  800 200 1 3)  => (200 800 600.0 300.0 1400.0 200.0 700.0 2200.0)
!#


(defun comb-tones (freq1 freq2 low-order high-order 
                         &key type chords ratios)
  "Combination tones between freq1 and freq2 (Vers 2).
  
  Info: https://en.wikipedia.org/wiki/Combination_tone"
  (labels ((comb-tones (fq1 fq2 low-odr high-odr)
             "combination tones of freq1 with all ratios 
  low to high order."
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

;; combination-tones - 1 voice per order plus interval frequencies
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

(defun calc-ratio-devs (num den mult)
  "Calculation of ratios with higher multipliers and +/-1 deviations.
  
  Args:
  num = numerator
  den = denominator
  mult = multiplier (from 2 to mult)

  Example:
  (calc-ratio-devs 9 7 4) 
  => (9/7 6/5 18/13 27/22 27/20 36/29 4/3)"
  
  (cons (/ num den) ; original ratio
        (loop for i from 2 to mult 
          for im = (* i num)
          for dm =  (* i den)
          append
          (list
           (/ im (+ dm 1))
           (/ im (- dm 1))))))

(defun spec-intrvl (fund start-harm harms harm-intrvl 
                         &optional (numb-harm? t))
  "xxx
  
  Args:
  fund = fundamental, 
  start-harm = start harmonic, 
  harms = number of harmonics or if (numb-harm? nil) nth harmonic,
  harm-intrvl = list of harmonic intervals, 
  numb-harm? = number of harmonics or nth harmonic (default t).
  
  Examples:
  (spec-intrvl 10 3 10 '(1 1 9)) 
  => (30 40 50 140 150 160 250 260 270 360) = 10 harmonics
  (spec-intrvl 10 3 10 '(1 1 5) nil) 
  => (30 40 50 100) = upto the 10. harmonic
  (spec-intrvl 10 1 10 1 nil) 
  => (10 20 30 40 50 60 70 80 90 100) = upto the 10. harmonic"
  
  (let ((harm-intrv (if (numberp harm-intrvl) 
                      (list harm-intrvl) harm-intrvl)))
    (if numb-harm?
      (loop repeat harms
        with x = start-harm
        for i from 0
        for j = (nth (mod i (length harm-intrv)) harm-intrv)
        collect (* fund x)
        do (setf x (+ x j)))
      (loop with x = start-harm
        for i from 0
        for j = (nth (mod i (length harm-intrv)) harm-intrv)
        collect (* fund x) into reslis
        do (setf x (+ x j))
        until (>= x (+ harms 1))
        finally (return reslis)))))

(defun brownian-borders (n weight-list start low high &key seed)
  "
  Example:
  (brownian-borders 50 '(1 40 -1 40 2 20 -2 20) 10 5 15 :seed 123)"
  
  (let ((wlis (loop for j from 0 below (- (length weight-list) 1) by 2
                append 
                (make-list (nth (+ 1 j) weight-list) 
                           :initial-element (nth j weight-list)) 
                into reslis
                finally (return reslis))))
    (loop repeat n
      initially (rnd-seed seed)
      with s = start
      do (setf s (if (< s low) 
                   (+ low (abs (- s low)))
                   (if (> s high) 
                     (- high (abs (- s high)))
                     s)))
      collect s into reslis
      do (setf s 
               (+ s (nth (random* (length wlis) :seed (seed)) wlis)))
      finally (init-seed nil) 
      (return reslis))))

(defun random-sum2 (lst-sum lis &key seed)
  "Try to build a list containing the numbers in lis 
  with the sum of lst-sum.

  Examples:
  (random-sum2 20 '(7 3 8))
  (random-sum2 31 '(2 4 6))"

  (loop with sum
    initially (rnd-seed seed)
    for x = (loop
              with s
              collect (nth (random* (length lis) :seed (seed)) lis) into reslis
              do (setf s (reduce #'+ reslis))
              until (>= s lst-sum)
              finally (return reslis)) 
    do (setf sum (reduce #'+ x))
    do (print x)
    until (= sum lst-sum)
    finally (return x)))

(defun nls (start end)
  "Creates a list of numbers between start and end.

  Example:
  (nls 3 7) => (3 4 5 6 7)"

  (loop for i from start to end
    collect i))


(defun rhythm-shift (dur rhy low high quant &key dist quarter seed)
  "rhy (or list of rhythms) is shifted randomly between quantized low and high in dur.

  Args:
  dur = duration of segment, 
  rhy = rhythmic value(s) shifted in dur,
  low = lowest shift value, 
  high = highest shift value, 
  quant = number or list of possible quantisations for shift
  dist = distibution of rests and notes, 
  quarter = if t durations are divided by 4
  seed = random seed value."
  (labels ((quantize (num steps)
             "Quantize a number to a (list of) step value(s)."
             (let* ((stp (if (numberp steps) (list steps) steps))
                    (round-lis (loop for i in stp 
                                 collect (* (round (/ num i)) i)))
                    (diff-lis (loop for i in round-lis 
                                collect (abs (- i num))))
                    (smallest (apply #'min diff-lis)))
               (nth (position smallest diff-lis) round-lis))))    
    (let* ((r (if (numberp rhy) (list rhy)
                (if (listp rhy) rhy
                  (error "Not of type number or list.")))) 
           ;check type of rhy
           (rhy-dur (reduce #'+ (mapcar #'abs r))) ;abs dur of rhy
           (shift-range (- dur rhy-dur)) ;max shift duration
           (lo (max 0 low)) ;avoid negative shifts
           (hi (min high shift-range)) ;avoid shifts that exceeds dur
           (ran (if (and (zerop low) (zerop high)) 0 
                  ;check if no shift at all
                  (+ lo (random* (* 1.0 (- hi lo)) :seed seed)))) 
           ; random shift between lo and hi
           (shift-dur (* -1 (quantize ran quant))) ; quantise shift     
           (rest-dur (- (* -1 shift-range) shift-dur)) 
           ; calculate the rest; dur - shift-dur rhy
           (quart (if quarter 4 1)) ; in quarter, divide by 4
           (1p (abs shift-dur)) ; positive shift-dur
           (1m shift-dur) ; negative shift-dur
           (3p (abs rest-dur)) ; positive rest-dur
           (3m rest-dur) ; negative rest-dur
           (rm (loop for i in r collect (* i -1)))
           (results 
            (case dist
              (nnn (cons 1p (append r (list 3p))));note-note(s)-note
              (rnn (cons 1m (append r (list 3p)))) ;rest-note(s)-note
              (nrn (cons 1p (append rm (list 3p)))) ;note-rest(s)-note
              (nnr (cons 1p (append r (list 3m)))) ;note-note(s)-rest
              (rrn (cons 1m (append rm (list 3p)))) ;rest-rest(s)-note
              (rnr (cons 1m (append r (list 3m)))) ;rest-note(s)-rest
              (nrr (cons 1p (append rm (list 3m)))) ;note-rest(s)-rest
              (otherwise (cons 1m (append r (list 3m)))))) ; see no. 5
           (quart-results (mapcar (lambda (x) (/ x quart)) results))
           ; calulate durations absolute or in quarter
           (ratio-results (mapcar #'rationalize quart-results))
           ; convert to ratios
           (result (loop for i in ratio-results
                     when (not (zerop i))
                     collect i))) ; delete unwanted zeros
      (when (> high shift-range)
        (format T "With rhythm = ~a shift range is set to max ~a.~%"     
                rhy shift-range))
      (when (< low 0)
        (format T "Negative shift value. Rhythm will start at least from 0.~%")) 
      result)))

;; (rhythm-shift 5 3 0.5 1.5 1/4) => (-5/4 3 -3/4)
;; (rhythm-shift 5 3 0.5 2 1/4 :quarter t) => (-3/16 3/4 -5/16)
;; (rhythm-shift 5 3 0.5 2 '(1/3 1/5) :quarter t) => (-9/20 3/4 -1/20)
;; (rhythm-shift 5 '(1 1) 0.5 2 1/3 :quarter t) => (-1/4 1/4 1/4 -1/2)
;; (rhythm-shift 5 3 0.5 1.5 1/4 :seed 2 :quarter t) => (-5/16 3/4 -3/16)
;; (rhythm-shift 5 3 -1 1.5 1/4 :quarter t) 
;; => Message: Negative shift value. Rhythm will start at least from 0.
;; (rhythm-shift 5 3 0 2.5 '(1/2 1/3 1/4 1/5) :quarter t) 
;; => Message: With rhythm = 3 shift range is set to max 2.
;; (rhythm-shift 5 3 0 0 1/4 :seed 123 :quarter t)
;; (rhythm-shift 5 2.5 0.5 2 1/4 :quarter t :dist 'nnn) => (1/2 5/8 1/8) ; note-note(s)-note
;; (rhythm-shift 5 2.5 0.5 2 1/4 :quarter t :dist 'rnn) => (-5/16 5/8 5/16) ; rest-note(s)-note
;; (rhythm-shift 5 2.5 0.5 2 1/4 :quarter t :dist 'nrn) => (3/16 -5/8 7/16) ; note-rest(s)-note
;; (rhythm-shift 5 2.5 0.5 2 1/4 :quarter t :dist 'nnr) => (7/16 5/8 -3/16) ; note-note(s)-rest
;; (rhythm-shift 5 2.5 0.5 2 1/4 :quarter t :dist 'rrn) => (-1/4 -5/8 3/8) ; rest-rest(s)-note
;; (rhythm-shift 5 2.5 0.5 2 1/4 :quarter t :dist 'rnr) => (-5/16 5/8 -5/16) ; rest-note(s)-rest
;; (rhythm-shift 5 2.5 0.5 2 1/4 :quarter t :dist 'nrr) => (5/16 -5/8 -5/16) ; note-rest(s)-rest

(defun fade-io (durlis voices up-curve down-curve)
  "Calculate durations for a continuous fade in and out with indepentent curves for any amount of voices."
  (let* ((fdura (first durlis))
         (mdura (second durlis))
         (ldura (third durlis))
         (up-interp (reverse (gen-transition 0 fdura voices up-curve))) 
         ; first rest duration
         (down-interp (gen-transition ldura 0 voices down-curve)) 
         ; second rest duration
         (up-down (matrix-transpose (list up-interp down-interp)))
         ; combine rest durations
         )  
    (loop for i in up-down 
      for fi = (first i)
      for si = (second i)
      for hsi = (/ mdura 2)
      collect
      (quantize
       (list 
        (* -1.0 fi) ; first rest
        (+ hsi (- fdura fi)) ; duration until beginning mdura
        (- (- (find-sum (list mdura ldura)) si) hsi) ; remaining duration
        (* -1.0 si)) ; second rest
       '(1 2 3 4 5 6 7 8) :type :ratio))))

;; (fade-io '(3 1 2) 8 1 1)

(defun list-replace-at (lst start-id elm)
  (replace lst 
           (if (numberp elm) (list elm) elm) 
           :start1 start-id))

;; (list-replace-at '(1 2 3 4 5) 2 9) => (1 2 9 4 5)
;; (list-replace-at '(1 2 3 4 5) 2 '(9 10)) => (1 2 9 10 5)

(defun list-insert-at (lst index new-value)
  "https://stackoverflow.com/questions/4387570/in-common-lisp-how-can-i-insert-an-element-into-a-list-in-place/4388263"
  (let ((retval nil))
    (loop for i from 0 to (- (length lst) 1) do
      (when (= i index)
        (push new-value retval))
      (push (nth i lst) retval))
    (when (>= index (length lst))
      (push new-value retval))
    (nreverse retval)))

;; (list-insert-at '(1 2 3 4 5) 5 'a)
;; (list-insert-at '(1 2 3 4 5) 0 'a)
;; (list-insert-at '(1 2 3 4 5) 0 '(a b))

(defun ninsert (val target n)
  (cond ((<= n 0) (cons val target))
        (t (push val (cdr (nthcdr (1- n) target)))
           target)))

;; (ninsert '(a b) '(1 2 3 4 5) 3)

#|
12/16 = 3/4
11/16 und 13/16

15/20 = 3/4
14/20 und 16/20

18/24 = 3/4
17/24 und 19/14

(defun dev-durations (num denom quant dev &key flat)
  (let ((quant-lst (if (numberp quant) (list quant) quant))
        (dev-lst (if (numberp dev) (list dev) dev)))
    (if flat
      (loop for q in quant-lst appending
        (loop for i in dev-lst
          collect (/  (+  (* num q) i)
                      (* denom q))))
      (loop for q in quant-lst collect
        (loop for i in dev-lst
          collect (/  (+  (* num q) i)
                      (* denom q)))))))

(defun dev-durations (ratio quant dev)
  (let ((quant-lst (if (numberp quant) (list quant) quant))
        (dev-lst (if (numberp dev) (list dev) dev)))
      (loop for q in quant-lst appending
        (loop for i in dev-lst
          for rq = (* ratio q)
          collect (/ (+ rq i)
                      rq)))))
|#

(defun dev-durations (ratio quant dev &key (flat t))
  "Calculate a list of quantified deviations from a ratio.

  Args:
  ratio: ratio = measure
  quant: (list of) quantized deviations (divisor of 1)
  dev: deviation from quant, e.g. -1 = -1/16
  flat: default t = results in a flat list

  Examples:
  (dev-durations 3/4 16 '(-1 1))
  (dev-durations 3/4 '(12 16 20) '(-1 1))
  (dev-durations 4/4 '(12 16 20) '(-1 1) :flat nil) "

  (let ((quant-lst (if (numberp quant) (list quant) quant)) 
        ; force to list if quantisations, divisions of 1 = 4/4
        (dev-lst (if (numberp dev) (list dev) dev)))
    ;deviation list measured by quantisation e.g.  +1 => + 1/16
    (loop for q in quant-lst
      for result = (cons ratio ; add original ratio
                         (loop for i in dev-lst
                           for rq = (* ratio q)
                           collect (* (/ (+ rq i) rq) ratio)))
      if flat appending result ; result as flat list
      else collect result))) ; result with nested lists

(defun dev-durations1 (ratio quant dev)

  "Calculate a list of quantified deviations from a ratio.
  
  Args:
  ratio: ratio = measure
  quant: (list of) quantized deviations (divisor of 1)
  dev: deviation from quant, e.g. -1 = -1/16
  flat: default t = results in a flat list
  
  Examples:
  (dev-durations1 3/4 16 '(2 1))
  (dev-durations1 3/4 '(12 16 20) '(2 1)) "
  
  (let ((quant-lst (if (numberp quant) (list quant) quant))
        ; force to list if quantisations, divisions of 1 = 4/4
        (dev-lst (if (numberp dev) (list dev) dev)))
    ;deviation list measured by quantisation e.g.  +1 => + 1/16
    (loop for q in quant-lst appending
      (loop for i in dev-lst
        for rq = (* ratio q)
        collect 
        (cons ratio 
              (list (* (/ (+ rq i) rq) ratio)
                    (* (/ (- rq i) rq) ratio)))))))

(defun merge-articulations 
       (arts &key (empty-articulations '(- default)))
  "Merges list of OMN articulations to a combined attribute.

  Args:
  arts: a list of OMN articulations
  empty-attributes: articulations to ignore in a combination. 

  Examples:
  (merge-articulations '(ten ponte ubow))
  => ten+ponte+ubow
  (merge-articulations '(- stacc))
  => stacc

  2017 (c) Thorsten Anders
  https://opusmodus.com/forums/topic/773-merging-and-disassembling-articulation-symbols/"
  #| ; can be uncommented when articulationp works
  (assert (every #'articulationp arts)
          (arts)
          "All values to merge must be OMN attributes: ~A.~%" arts)
  |#

  (intern 
   (reduce #'(lambda (a1 a2) (format nil "~A+~A" a1 a2))
           (mappend #'(lambda (art) 
                        (unless (some #'(lambda (a) (eq art a))
                                      empty-articulations)
                          (list (symbol-name art))))
                    arts))))

(defun disassemble-articulations (art)
  "Splits a combined OMN articulations into a list of its 
  individual attributes.

  Example:
  (disassemble-articulations 'leg+ponte)
  => (leg ponte)

  2017 (c) Thorsten Anders
  https://opusmodus.com/forums/topic/773-merging-and-disassembling-articulation-symbols/"
  #|
  (assert (articulationp art)
          (art)
          "Value not OMN articulations: ~A.~%" art)
  |#

  ;; split-string seems to be part of OMN
  (mapcar #'intern (split-string (symbol-name art) :separator "+")))

(defun circle-repeat (pattern n)
  "Circle through elements in pattern (a list) until n elements 
  are collected.
  NOTE: only supports flat list so far.

  2017 (c) Thorsten Anders
  https://opusmodus.com/forums/topic/773-merging-and-disassembling-articulation-symbols/"  

  (let ((l (length pattern)))
    (loop 
      for i from 0 to (- n 1)
      collect (nth (mod i l) 
                   pattern))))

; (circle-repeat '(bb4 g4 f4) 20)

(defun zip-articulations (&rest arts)
  "Expects lists of articulations and combines them to a single 
  list of merged articulations. Shorter lists are circled to the 
  length of the longest.
  
  Example:
  (zip-articulations '(default leg leg default) '(breathy))
  
  2017 (c) Thorsten Anders
  https://opusmodus.com/forums/topic/773-merging-and-disassembling-articulation-symbols/
  
  ;;; TODO: support nested lists -- somehow store nesting structure, then process flattened list and apply nesting back"
  
  (let ((max-length (apply #'max (mapcar #'length arts))))
    (mapcar #'merge-articulations             
            (matrix-transpose (mapcar #'(lambda (xs) (circle-repeat xs max-length)) arts)))))

(defun zip-articulations (&rest arts)
  "Expects lists of articulations and combines them to a single 
  list of merged articulations. Shorter lists are circled to the 
  length of the longest.
  
  Example:
  (zip-articulations '(default leg leg default) '(breathy))
  
  2017 (c) Thorsten Anders
  https://opusmodus.com/forums/topic/773-merging-and-disassembling-articulation-symbols/
  
  ;;; TODO: support nested lists -- somehow store nesting structure, then process flattened list and apply nesting back"
  
  (let ((max-length (apply #'max (mapcar #'length arts))))
    (mapcar #'merge-articulations             
            (matrix-transpose 
             (mapcar #'(lambda (xs) (circle-repeat xs max-length)) 
                     arts)))))

(defun align-lists (lsts)
  "Align lists to the longest by looping.
  
  Example:
  (align-lists '((1 2 3 4 5) (6 7) (8 9 10)))
  => ((1 2 3 4 5) (6 7 6 7 6) (8 9 10 8 9)) "
  
  (let ((max-length (apply #'max (mapcar #'length lsts))))
    (loop for j in lsts
      collect (loop for i from 0 to (- max-length 1)
        collect (nth (mod i (length j))
                     j)))))

(defun ll (lsts)
  "Lengths of lists"
  (loop for i in lsts
  collect (length i)))

(defun succ-shuffle (lst prob &key seed)
  "Shuffle successive values in a list according to the probability prob (0-100).
  
  Example:
  (succ-shuffle '(1 2 3 4 5 6 7 8 9 0) 50 :seed 123)
  => (1 2 4 3 6 5 7 8 9 0)
  (succ-shuffle '(1 2 3 4 5 6 7 8 9) 50 :seed 17)"
  (let (state)
    (setf state *init-seed*)
    (setf seed (rnd-seed seed))
    (let ((result
  (loop with l = lst
    initially (init-seed 25)
    while l
    for p = (< (random* 100 :seed (seed)) prob)
    append (loop repeat 2 
              collect (pop l) into reslis
              finally (return (if p (reverse reslis) reslis))))))
    (init-state state)
      (remove nil result))))

(defun probability (perc)
  "Probability between 0 and 100 %."
  (< (random 100) perc))

(defun vdoc (var)
  "Documentations string from variable var."
(documentation var 'variable)) 

(defun matra (llst &key (type 'shortest))
  "Matrix-transpose operation.
  
  Types: 
  'shortest (default): depending on the shortest list (omitting values from longer lists)
  'longest: depending on the longest list (adding NILs to shorter lists)
  'trunc: depending on the longest list (removing NILs from shorter lists)

  Examples:
  (matra '((1 2 3 4) (5 6 7 8 9) (10 11 12 13 14 15)))
  => ((1 5 10) (2 6 11) (3 7 12) (4 8 13))
  (matra '((1 2 3 4) (5 6 7 8 9) (10 11 12 13 14 15)) :type 'longest)
  => ((1 5 10) (2 6 11) (3 7 12) (4 8 13) (nil 9 14) (nil nil 15))
  (matra '((1 2 3 4) (5 6 7 8 9) (10 11 12 13 14 15)) :type 'trunc)
  => ((1 5 10) (2 6 11) (3 7 12) (4 8 13) (9 14) (15))"
  
  (let ((minlen (apply #'min 
                (loop for i in llst 
                  collect (length i))))
        (maxlen (apply #'max 
                (loop for i in llst 
                  collect (length i)))))
    (case type
      (shortest  
       (loop for i from 0 to (- minlen 1) 
         collect
         (loop for j in llst collect (nth i j))))
      (longest
       (loop for i from 0 to (- maxlen 1) 
         collect
         (loop for j in llst collect (nth i j))))
      (trunc
       (loop for i from 0 to (- maxlen 1) 
         collect
         (remove nil (loop for j in llst collect (nth i j))))))))


(defun closest-to (val lst)
  "Number in a list closest to val.
  Example:
  ((closest-to 3 '(4 2 5 7 5 8 2))"
 (loop for i in lst
               collect (abs (- i val)) into reslis
  finally (return (nth 
                   (position (apply #'min reslis) reslis) 
                   lst))))

(defun quartertone-closest1 (pitches)
  "Convert pitches with 3 quartertone alteration 
  to 1 quartertone alteration.
  
  Example: (quartertone-closest 
  '(c4 cs4+ as4+ d4+ d4 b4- a4+ gb4- ab4- e4))
  => (c4 d4- b4- d4+ d4 b4- a4+ f4+ g4+ e4)"
  
  (loop for i in pitches
    if (< (length (string i)) 4)
    collect i
    else
    if (equal (string 'b4-) (subseq (string i) 1 4))
    collect 
    (read-from-string 
     (concatenate 'string 
                  (string 
                   (first 
                    (pitch-transpose -0.5 (list i)))) "+"))
    else
    if (equal (string 's4+) (subseq (string i) 1 4))
    collect 
    (read-from-string 
     (concatenate 'string 
                  (string 
                   (first 
                    (pitch-transpose 0.5 (list i)))) "-"))))

(defun quartertone-closest2 (pitches)
  "Convert pitches with 3 quartertone alteration 
  to 1 quartertone alteration.

  Example: (quartertone-closest 
  '(c4 cs4+ as4+ d4+ d4 b4- a4+ gb4- ab4- e4))
  => (c4 d4- b4- d4+ d4 b4- a4+ f4+ g4+ e4)"

  (loop for i in pitches
    if (< (length (string i)) 4)
    collect i
    else
    if (and
        (equal (string 'b) (subseq (string i) 1 2))
        (equal (string '-) (subseq (string i) 3 4)))
    collect (read-from-string 
             (concatenate 'string 
                          (string 
                           (first 
                            (pitch-transpose -0.5 (list i)))) "+"))
    else
    if (and
        (equal (string 's) (subseq (string i) 1 2))
        (equal (string '+) (subseq (string i) 3 4)))
    collect (read-from-string 
             (concatenate 'string 
                          (string 
                           (first 
                            (pitch-transpose 0.5 (list i)))) "-"))))

;; implementation in OM
(defun quartertone-closest (sequence)
  "Convert pitches with 3 quartertone alteration 
  to 1 quartertone alteration.
  
  Example:
  (quartertone-closest 
  '(c4 cs4+ as4+d4+ d4 b4- a4+ gb4- ab4- e4))
  => (c4 d4- b4-d4+ d4 b4- a4+ f4+ g4+ e4)
  
  (quartertone-closest 
  '((q c4 cs4+ e as4+d4+ d4) (s b4- a4+ gb4- ab4- q e4)))
  => ((q c4 d4- e b4-d4+ d4) (s b4- a4+ f4+ g4+ q e4))"

  (do-verbose ("quartertone-closest")
    (labels ((quartertone-closest-l (sequence)
               (flatten
                (loop for p in sequence
                  collect 
                  (chordize 
                   (loop for i in (melodize p)
                     if (< (length (string i)) 4)
                     collect i
                     else
                     if (and
                         (equal (string 'b) (subseq (string i) 1 2))
                         (equal (string '-) (subseq (string i) 3 4)))
                     collect (read-from-string 
                              (concatenate 'string 
                                           (string 
                                            (first 
                                             (pitch-transpose -0.5 (list i)))) "+"))
                     else
                     if (and
                         (equal (string 's) (subseq (string i) 1 2))
                         (equal (string '+) (subseq (string i) 3 4)))
                     collect (read-from-string 
                              (concatenate 'string 
                                           (string 
                                            (first 
                                             (pitch-transpose 0.5 (list i)))) "-")))))))
             
             (quartertone-closest* (sequence)
               (if (listsp sequence) (loop for i in sequence 
                                       collect (quartertone-closest-l i))
                 (quartertone-closest-l sequence))))
      
      (if (omn-formp sequence)
        (disassembling-omn ((sequence plist) sequence :pitch)
          (quartertone-closest* sequence))
        (quartertone-closest* sequence)))))

#|
(defun envelope-exp (env &optional (power 1.0) (xgrid 100))
";; from CLM package"
  (let* ((min (min-envelope env))
	 (largest-diff (- (max-envelope env) min))
	 (x-min (car env))
	 (x-max #-cltl2 (nth (- (length env) 2) env) #+cltl2 (car (last env 2))))
    (loop for x from x-min to x-max by (/ (- x-max x-min) xgrid)
      for y = (envelope-interp x env)
      collect x
      collect (if (zerop largest-diff)
                y
		(+ min
		   (* largest-diff
		      (expt (/ (- y min) largest-diff) power)))))))
|#

(defun fl (lst)
  "Length of a flattened list."
  (length (flatten lst)))

(defun tempo-proportion (prop)
  "Tempo proportion as ratios from 60 MM."
  (/ 60.0 prop)) 
;; (tempo-proportion 8/9) => MM 67.5
;; (tempo-proportion 3/4) => 80.0
;; (tempo-proportion 4/3) => 45.0
;; (tempo-proportion 4/5) => 75.0
;; (tempo-proportion 5/4) => 48.0
;; (tempo-proportion 5/6) => 72.0
;; (tempo-proportion 6/5) => 50.0

(defun list-tempo-proportion (id &optional rev)
  "List spectral proportions and tempi related to MM = 60 upto the given ID.
  Example:
  (list-tempo-prop 4) => ((1/2 120.0) (2/3 90.0) (3/4 80.0) (4/5 75.0))"
  (loop for i from 1 to id
    for j from 2
    for prop = (if rev (/ j i) (/ i j))
    collect (list prop (/ 60.0 prop))))



(defun dur-tempo-sec (dur tempo)
  (let ((mm (if (listp tempo) tempo (list tempo))))
  "Calculate duration in seconds from given duration and tempo.
  Example:
  (dur-tempo-sec 3/4 75) => 2.4
  (dur-tempo-sec 4/4 '(45 60 80)) => (5.3333335 4.0 3.0)"
    (loop for i in mm
      collect
  (* (* dur 4.0) (/ 60 i)))))

(defun dur-sec-tempo (dur sec)
  "Calculate tempo from given duration and resulting seconds.
  Example:
  (dur-sec-tempo 3/4 3) => 60
  (dur-sec-tempo 5/4 4) => 75
  (dur-sec-tempo 5/4 '(3 4 5))  => (100 75 60)
  (dur-sec-tempo '(3/4 4/4 5/4) 3)  => ((60) (80) (100))
  (dur-sec-tempo '(3/4 4/4 5/4) '(3 4 5))
  =>  ((60 45 36) (80 60 48) (100 75 60))"
  (let ((durl (if (listp dur) dur (list dur)))
        (secl (if (listp sec) sec (list sec))))
    (loop for d in durl 
      collect
      (loop for s in secl 
        collect
        (/ 60 (/ s (* d 4))))
      into reslis
      finally (return  (if (= (length reslis) 1)
                         (if (= (length (first reslis)) 1)
                           (first (first reslis))
                           (first reslis))
                         reslis)))))


(defun sec-to-minsec (seconds)
  (let* ((minutes (floor (/ seconds 60)))
       (secs (- seconds (* minutes 60))))                
  (format t "Duration: ~F min ~F sec " minutes secs)))
