(in-package :cl-libworld)

;; 'git clone https://github.com/masatoi/clgplot' in your local-projects directory

(defparameter in-wav (make-world-wav-from-file "/home/wiz/Sound/dragon/mono-22k/001.wav"))
(defparameter in-wav (make-world-wav-from-file "/home/wiz/cl-libworld/lib/World/test/vaiueo2d.wav"))

(defparameter params (analysis in-wav :f0-floor 71d0))

(array-dimensions (world-params-aperiodicity params))
(clgp:plot-list
 (loop for j from 0 to (1- (array-dimension (world-params-aperiodicity params) 1)) collect
   (aref (world-params-spectrogram params) 800 j)))

(clgp:plot-list (loop for elem across (world-params-f0 params) collect elem)
                :title "refined-f0" :x-label "frames" :y-label "F0")

(clgp:splot-matrix (world-params-spectrogram params)
                   :x-label "frames" :y-label "frequency windows")
(clgp:splot-matrix (world-params-aperiodicity params)
                   :x-label "frames" :y-label "frequency windows")

(array-dimensions (world-params-spectrogram params))
(clgp:plot-lists
 (list
  (loop for i from 0 to (1- (array-dimension (world-params-spectrogram params) 0)) collect
    (aref (world-params-spectrogram params) i 13))
  (loop for i from 0 to (1- (array-dimension (world-params-spectrogram params) 0)) collect
    (aref (world-params-spectrogram params) i 59))))

(clgp:splot-matrix (world-params-spectrogram params)
                   :x-label "frames" :y-label "frequency windows" )

(defparameter out-wav (synthesis params))

(clgp:plot-lists
 (list (loop for elem across (world-wav-data in-wav) collect elem)
       (loop for elem across (world-wav-data out-wav) collect elem))
 :title-list '("in-wav" "out-wav") :x-label "time" :y-label "gain"
 :output "/home/wiz/tmp/world-in-out.png")

(output-world-wav-to-file out-wav "/home/wiz/tmp/highspectorogram0-1.wav")


;; スペクトグラムの高周波数帯の値を0にしてしまうと音声はどうなる？ => synthesisで失敗する
;; 0.00001d0などの非常に低い値にする => synthesisには成功
;; どこからを高周波とするか。100にすると音がこもりすぎ。200でも若干こもってる
(loop for i from 0 to (1- (array-dimension (world-params-spectrogram params) 0)) do
  (loop for j from 200 to (1- (array-dimension (world-params-spectrogram params) 1)) do
    (setf (aref (world-params-spectrogram params) i j) 0.00001d0)))


; "/home/wiz/src/segmentation-kit/wav/vaiueo2d.lab"
; "/home/wiz/cljtalk/dataset/001.lab"
(let* ((lab-list (cljtalk::read-lab-file "/home/wiz/src/segmentation-kit/wav/vaiueo2d.lab"))
       (n-frame (length (world-params-f0 params)))
       (frame-size (/ (caddr (wiz:last1 lab-list)) (1- n-frame)))
       (lab-x-list (alexandria:flatten
                    (mapcar (lambda (x)
                              (list (cadr x) (cadr x) (caddr x) (caddr x)))
                            lab-list)))
       (x-list (loop for x from 0.0 by frame-size
                     for i from 0 to (1- n-frame) collect
                     x)))
  (clgp:plot-lists
   (list (loop for elem across (world-params-f0 params) collect elem)
         (alexandria:flatten (loop repeat (length lab-list) collect '(0 160 160 0))))
   :x-lists (list x-list lab-x-list)
   :x-label "time[sec]"
   :title-list '("f0" "phoneme")
   :output "/home/wiz/tmp/vaiueo2d-f0-alignment.png")
  )

(let* ((lab-list (cljtalk::read-lab-file "/home/wiz/src/segmentation-kit/wav/vaiueo2d.lab"))
       (n-frame (length (world-params-f0 params)))
       (frame-size (/ (caddr (wiz:last1 lab-list)) (1- n-frame)))
       (lab-x-list (alexandria:flatten
                    (mapcar (lambda (x)
                              (list (cadr x) (cadr x) (caddr x) (caddr x)))
                            lab-list)))
       (x-list (loop for x from 0.0 by frame-size
                     for i from 0 to (1- n-frame) collect
                     x)))
  (clgp:plot-list
   (alexandria:flatten (loop repeat (length lab-list) collect '(0 1 1 0)))
   :x-list lab-x-list
   :output "/home/wiz/tmp/vaiueo2d-alignment.png"
  ))


;;;;;;;;;;;;;;;;;;;;;; メルフィルタバンクをかける

(defparameter in-wav (make-world-wav-from-file "/home/wiz/cl-libworld/lib/World/test/vaiueo2d.wav"))
(defparameter in-wav (make-world-wav-from-file "/home/wiz/Sound/dragon/mono-22k/001.wav"))

(defparameter params (analysis in-wav :f0-floor 71d0))
(array-dimensions (world-params-spectrogram params))

(defvar mel-filter-bank)
(defvar mel-filter-bank-icenters)

(multiple-value-bind (fb icenters)
    (make-mel-filter-bank 22050 (* (array-dimension (world-params-spectrogram params) 1) 2) 60)
  (setf mel-filter-bank fb
        mel-filter-bank-icenters icenters))

(array-dimensions mel-filter-bank)

(defparameter mel-filter-result
  (wiz:mapmat (lambda (x) (log x 10))
              (wiz:m*
               (world-params-spectrogram params)
               (wiz:m-t mel-filter-bank))))

(clgp:splot-matrix mel-filter-result)

(clgp:splot-matrix (wiz:mapmat (lambda (x) (expt 10 x)) mel-filter-result))

(ql:quickload :wiz-utl)
(ql:quickload :clgplot)

(defun make-spline (x-list y-list)
  (let* ((h (wiz:diff-list x-list))
         (dy (wiz:diff-list y-list))
         (v (wiz:nlet iter ((h h) (dy dy) (product nil))
              (if (null (cdr h))
                (nreverse product)
                (iter (cdr h)
                      (cdr dy)
                      (cons (* 6 (- (/ (cadr dy) (cadr h))
                                    (/ (car dy) (car h))))
                            product)))))
         (mat-size (- (length x-list) 2))
         (vvec (make-array (list mat-size 1) :element-type 'double-float))
         (mat (make-array (list mat-size mat-size) :element-type 'double-float :initial-element 0d0)))
    (loop for j from 0 to (1- mat-size)
          for hj-1 in h
          for hj in (cdr h)
          for vj in v
          do
       (setf (aref vvec j 0) vj) ; init vvec
       (cond ((= j 0)            ; init mat
              (setf (aref mat 0 0) (* 2 (+ hj-1 hj))
                    (aref mat 0 1) hj))
             ((= j (1- mat-size))
              (setf (aref mat (1- mat-size) (- mat-size 2)) hj-1
                    (aref mat (1- mat-size) (1- mat-size)) (* 2 (+ hj-1 hj))))
             (t (setf (aref mat j (1- j)) hj-1
                      (aref mat j j) (* 2 (+ hj-1 hj))
                      (aref mat j (1+ j)) hj))))
    (let* ((mat-1 (wiz:m-1 mat))
           (uvec (wiz:m* mat-1 vvec))
           (u (append (cons 0d0 (loop for i from 0 to (1- mat-size) collect (aref uvec i 0)))
                      '(0d0)))
           (b (mapcar (lambda (uj) (/ uj 2)) u))
           (a (mapcar (lambda (uj+1 bj hj) (/ (- uj+1 (* 2 bj)) (* 6 hj))) (cdr u) b h))
           (c (mapcar (lambda (hj yj+1 aj bj dj)
                        (/ (- yj+1 (* aj hj hj hj) (* bj hj hj) dj) hj))
                      h (cdr y-list) a b y-list)))
      (lambda (x)
        (let* ((pos0 (position-if (lambda (xj+1) (<= x xj+1)) x-list)) ; x-list must be sorted
               (pos (cond ((null pos0) (- (length x-list) 2))
                          ((<= pos0 0) 0)
                          (t (1- pos0))))
               (xj (nth pos x-list))
               (yj (nth pos y-list))
               (aj (nth pos a))
               (bj (nth pos b))
               (cj (nth pos c)))
          (+ (* aj (expt (- x xj) 3))
             (* bj (expt (- x xj) 2))
             (* cj (- x xj))
             yj))))))

(defparameter spline-func
  (make-spline '(0d0 1.5d0 2.2d0 3.7d0 3.9d0 4.5d0 5.1d0 6.6d0 7.7d0) ; x-list
               '(1d0 3d0 2d0 5d0 1d0 4d0 3.1d0 1d0 5d0)))             ; y-list

(let ((x-list (wiz:seq -1d0 8d0 :by 0.1d0)))
  (clgp:plot-lists (list (mapcar spline-func x-list)
                         '(1d0 3d0 2d0 5d0 1d0 4d0 3.1d0 1d0 5d0))
                   :x-lists (list x-list
                                  '(0d0 1.5d0 2.2d0 3.7d0 3.9d0 4.5d0 5.1d0 6.6d0 7.7d0))
                   :style '(lines points)))


;; mel-filter-result と mel-filter-bank-icenters でデータ点を作ってスプライン補間する

(let* ((fft-size 513)
       (frame 20)
       (x-list (mapcar (lambda (x) (* x 1d0))
                       (append (cons 0 mel-filter-bank-icenters) (list fft-size))))
       (y-list (append (cons 0d0
                             (loop for j from 0 to (1- (array-dimension mel-filter-result 1)) collect
                               (expt 10 (aref mel-filter-result frame j))))
                       (list 0d0)))
       (spline-func (make-spline x-list y-list)))

  (let ((x-list (wiz:seq 0d0 513d0 :by 0.5d0)))
    (clgp:plot-lists (list (mapcar spline-func x-list)
                           (loop for j from 0 to (1- fft-size) collect
                             (aref (world-params-spectrogram params) frame j)))
                     :x-lists (list x-list
                                    (loop for j from 0 to (1- fft-size) collect j)))))

(let* ((fft-size 513)
       (frame 20)
       (x-list (mapcar (lambda (x) (* x 1d0)) mel-filter-bank-icenters))
       (y-list (loop for j from 0 to (1- (array-dimension mel-filter-result 1)) collect
                               (expt 10 (aref mel-filter-result frame j))))
       (spline-func (make-spline x-list y-list)))
  (let ((x-list (wiz:seq 0d0 512d0 :by 1d0)))
    (clgp:plot-lists (list (mapcar spline-func x-list)
                           (loop for j from 0 to (1- fft-size) collect
                             (aref (world-params-spectrogram params) frame j)))
                     :x-lists (list x-list
                                    (loop for j from 0 to (1- fft-size) collect j)))))

;; 縦のスケールが違う? とりあえずこのまま戻して音声合成してみる

(defparameter recovered-spectrogram
  (make-array (array-dimensions (world-params-spectrogram params))
              :element-type 'double-float))

(loop for frame from 0 to (1- (array-dimension recovered-spectrogram 0)) do
  (let* ((fft-size (array-dimension recovered-spectrogram 1))
         (x-list (mapcar (lambda (x) (* x 1d0))
                         (append (cons 0 mel-filter-bank-icenters) (list fft-size))))
         (y-list
          (append (cons 0d0
                        (loop for j from 0 to (1- (array-dimension mel-filter-result 1)) collect
                          (expt 10 (aref mel-filter-result frame j))))
                  (list 0d0)))
         (spline-func (make-spline x-list y-list)))
    (loop for i from 0 to (1- fft-size) do
      (setf (aref recovered-spectrogram frame i) (funcall spline-func i)))))

(clgp:splot-matrix recovered-spectrogram)
(clgp:splot-matrix (world-params-spectrogram params))

(setf (world-params-spectrogram params) recovered-spectrogram)

(defparameter recovered-spectrogram-positive
  (make-array (array-dimensions (world-params-spectrogram params))
              :element-type 'double-float))

(loop for i from 0 to (1- (array-dimension (world-params-spectrogram params) 0)) do
  (loop for j from 0 to (1- (array-dimension (world-params-spectrogram params) 1)) do
    (setf (aref recovered-spectrogram-positive i j)
          (if (<= (aref recovered-spectrogram i j) 0)
            0.00000000001d0
            (aref recovered-spectrogram i j)))))

(clgp:splot-matrix recovered-spectrogram-positive)

(progn
  (setf (world-params-spectrogram params)
        (wiz:mapmat (lambda (x) (/ x 6d0)) recovered-spectrogram-positive))
  nil)

(progn
  (setf (world-params-spectrogram params) recovered-spectrogram-positive)
  nil)

(clgp:splot-matrix (world-params-spectrogram params))

(defparameter out-wav (synthesis params))

(output-world-wav-to-file out-wav "/home/wiz/tmp/recovered-dragon-60ch.wav")


;;;; ここまでの一連の流れをまとめる

(defparameter in-wav (make-world-wav-from-file "/home/wiz/cl-libworld/lib/World/test/vaiueo2d.wav"))
(defparameter params (analysis in-wav :f0-floor 71d0))

(defstruct (mel-filter-bank (:constructor %make-mel-filter-bank))
  filter-bank
  index-centers)

(defun hz2mel (f)
  (* 1127.01048d0
     (log (+ (/ f 700.0d0) 1.0d0))))

(defun mel2hz (m)
  (* 700.0d0
     (- (exp (/ m 1127.01048d0)) 1.0d0)))

(defun make-mel-filter-bank (fs fft-size num-channels)
  (let* ((fmax (/ fs 2))        ; Nyquist frequency (Hz)
         (melmax (hz2mel fmax)) ; Nyquist frequency (mel)
         (nmax (/ fft-size 2))
         (df (/ fs fft-size))   ; Frequency per fft-index
         (dmel (/ melmax (1+ num-channels)))
         (melcenters (loop for i from 1 to num-channels collect (* i dmel)))
         (fcenters (mapcar #'mel2hz melcenters))
         (indexcenter (mapcar (lambda (fc) (round (/ fc df))) fcenters))
         (indexstart (cons 0 (subseq indexcenter 0 (1- (length indexcenter)))))
         (indexstop (append (cdr indexcenter) (list nmax)))
         (filter-bank (make-array (list num-channels nmax)
                                  :element-type 'double-float :initial-element 0d0)))
    (loop for c from 0 to (1- num-channels)
          for icenter in indexcenter
          for istart in indexstart
          for istop in indexstop
          do
       (let ((increment (/ 1.0d0 (- icenter istart)))
             (decrement (/ 1.0d0 (- istop icenter))))
         (loop for i from istart to (1- icenter) do
           (setf (aref filter-bank c i) (* (- i istart) increment)))
         (loop for i from icenter to (1- istop) do
           (setf (aref filter-bank c i) (- 1.0d0 (* (- i icenter) decrement))))))
    (%make-mel-filter-bank :filter-bank filter-bank :index-centers indexcenter)))

(defvar mel-filter-bank)
(defvar mel-filter-bank-icenters)

(defun mel-filter-apply (params)
  (multiple-value-bind (fb icenters)
      (make-mel-filter-bank 22050 (* (array-dimension (world-params-spectrogram params) 1) 2) 60)
    (setf mel-filter-bank fb
          mel-filter-bank-icenters icenters))

  
