(in-package :cl-user)

(defpackage cl-libworld
  (:use :cl :cffi)
  (:export :make-world-wav :make-world-params
           :make-world-wav-from-file :output-world-wav-to-file
           :analysis :synthesis)
  (:nicknames :world))

(in-package :cl-libworld)

;;; loading
(defparameter *world-path*
  (merge-pathnames "lib/World/build/"
                   (asdf:system-source-directory :cl-libworld)))

(push *world-path* cffi:*foreign-library-directories*)

(load-foreign-library '(:default "audioio"))
(load-foreign-library '(:default "libworld"))

;;; definition

;;; audioio.so

;; int GetAudioLength(const char *filename);
(defcfun ("GetAudioLength" get-audio-length) :int
  (filename :pointer))

;; void wavread(const char* filename, int *fs, int *nbit, double *x);
(defcfun "wavread" :void
  (filename :pointer)
  (sampling-rate :pointer)
  (bit-rate :pointer)
  (data :pointer))

;; void wavwrite(const double *x, int x_length, int fs, int nbit, const char *filename);
(defcfun "wavwrite" :void
  (data :pointer)
  (data-length :int)
  (sampling-rate :int)
  (bit-rate :int)
  (filename :pointer))

;;; libworld.so

;;; F0 Analysys (Dio, StoneMask)

;; typedef struct {
;;   double f0_floor;
;;   double f0_ceil;
;;   double channels_in_octave;
;;   double frame_period;  // msec
;;   int speed;  // (1, 2, ..., 12)
;;   double allowed_range;  // Threshold used for fixing the F0 contour.
;; } DioOption;

(defcstruct dio-option
  (f0-floor :double)
  (f0-ceil  :double)
  (channels-in-octave :double)
  (frame-period :double)
  (speed    :int) ; (1, 2, ..., 12)
  (allowed-range :double)) ; Threshold used for fixing the F0 contour.

;; void Dio(const double *x, int x_length, int fs, const DioOption *option,
;;   double *time_axis, double *f0);

(defcfun "Dio" :void
  (data :pointer)
  (data-length :int)
  (sampling-rate :int)
  (option :pointer)
  (time-axis :pointer)
  (f0 :pointer))

;; void InitializeDioOption(DioOption *option);

(defcfun ("InitializeDioOption" initialize-dio-option) :void
  (option :pointer))

(defcfun ("GetSamplesForDIO" get-samples-for-dio) :int
  (fs :int)
  (x-length :int)
  (frame-period :double))

;; void StoneMask(const double *x, int x_length, int fs, const double *time_axis,
;;   const double *f0, int f0_length, double *refined_f0);

(defcfun ("StoneMask" stone-mask) :void
  (data :pointer)
  (data-length :int)
  (fs :int)
  (time-axis :pointer)
  (f0 :pointer)
  (f0-length :int)
  (refined-f0 :pointer))

;;; Spectral Envelope Estimation

;; typedef struct {
;;   // This is defined as the struct for future update.
;;   double q1;
;;   double f0_floor;
;; } CheapTrickOption;

(defcstruct cheap-trick-option
  (q1 :double)
  (f0-floor :double))

;; void CheapTrick(const double *x, int x_length, int fs, const double *time_axis,
;;   const double *f0, int f0_length, const CheapTrickOption *option,
;;   double **spectrogram);

(defcfun ("CheapTrick" cheap-trick) :void
  (data :pointer)
  (data-length :int)
  (fs :int)
  (time-axis :pointer)
  (f0 :pointer)
  (f0-length :int)
  (option :pointer)
  (spectrogram :pointer))

;; void InitializeCheapTrickOption(CheapTrickOption *option);

(defcfun ("InitializeCheapTrickOption" initialize-cheap-trick-option) :void
  (option :pointer))

;; int GetFFTSizeForCheapTrick(int fs, const CheapTrickOption *option);

(defcfun ("GetFFTSizeForCheapTrick" get-fft-size-for-cheap-trick) :int
  (fs :int)
  (option :pointer))

;;; Aperiodicity Estimation

;; typedef struct {
;;   double dummy;  // This is the future update.
;; } D4COption;

(defcstruct d4c-option
  (dummy :double))

;; void D4C(const double *x, int x_length, int fs, const double *time_axis,
;;   const double *f0, int f0_length, int fft_size, const D4COption *option,
;;   double **aperiodicity);

(defcfun "D4C" :void
  (data :pointer)
  (data-length :int)
  (fs :int)
  (time-axis :pointer)
  (f0 :pointer)
  (f0-length :int)
  (fft-size :int)
  (option :pointer)
  (aperiodicity :pointer))

;; void InitializeD4COption(D4COption *option);

(defcfun ("InitializeD4COption" initialize-d4c-option) :void
  (option :pointer))

;;; Synthesis

;; void Synthesis(const double *f0, int f0_length, double **const spectrogram,
;;     double **const aperiodicity, int fft_size, double frame_period, int fs,
;;     int y_length, double *y);

(defcfun ("Synthesis" wave-synthesis) :void
  (f0 :pointer)
  (f0-length :int)
  (spectrogram :pointer)
  (aperiodicity :pointer)
  (fft-size :int)
  (frame-period :double)
  (fs :int)
  (out-length :int)
  (out :pointer))

;;; Read/Write WAV files

;; (defparameter *x* (foreign-alloc :double :count 17500))

; "/home/wiz/cl-libworld/lib/World/test/vaiueo2d.wav"
(defun read-wav-file (filename)
  (with-foreign-string (s filename)
    (let* ((len (get-audio-length s))
           (arr (make-array len :element-type 'double-float)))
      (with-foreign-objects ((fs :int) (nbit :int) (x :double len))
        (wavread s fs nbit x)
        
        (format t "Sampling-rate: ~A, Bitrate: ~A~%" (mem-ref fs :int) (mem-ref nbit :int))
        (loop for i from 0 to (1- len) do
          (setf (aref arr i) (mem-aref x :double i)))
        arr
        ))))

;; (defparameter x-arr (read-wav-file "/home/wiz/cl-libworld/lib/World/test/vaiueo2d.wav"))


;;; Examples

;;   // Modification of the option
;;   // When you You must set the same value.
;;   // If a different value is used, you may suffer a fatal error because of a
;;   // illegal memory access.
;;   option.frame_period = world_parameters->frame_period;

;;   // Valuable option.speed represents the ratio for downsampling.
;;   // The signal is downsampled to fs / speed Hz.
;;   // If you want to obtain the accurate result, speed should be set to 1.
;;   option.speed = 1;

;;   // You should not set option.f0_floor to under world_kFloorF0.
;;   // If you want to analyze such low F0 speech, please change world_kFloorF0.
;;   // Processing speed may sacrify, provided that the FFT length changes.
;;   option.f0_floor = 71.0;

;;   // You can give a positive real number as the threshold.
;;   // Most strict value is 0, but almost all results are counted as unvoiced.
;;   // The value from 0.02 to 0.2 would be reasonable.
;;   option.allowed_range = 0.1;

(defun test ()
  (with-foreign-string (s (format nil "~A~A"
                                  (asdf:system-source-directory :cl-libworld)
                                  "lib/World/test/vaiueo2d.wav"))
    (let ((len (get-audio-length s))
          (frame-period 5.0d0))
      (with-foreign-objects ((fs :int) (nbit :int) (x :double len)
                             (dio-opt '(:struct dio-option))
                             (ct-opt '(:struct cheap-trick-option))
                             (d4c-opt '(:struct d4c-option)))
        ;; Read input wav
        (wavread s fs nbit x)
        ;; Initialize option structs
        (initialize-dio-option dio-opt)
        (initialize-cheap-trick-option ct-opt)
        (initialize-d4c-option d4c-opt)
        ;; Set to option structs
        (setf (foreign-slot-value dio-opt '(:struct dio-option) 'frame-period) frame-period
              (foreign-slot-value dio-opt '(:struct dio-option) 'speed) 1
              (foreign-slot-value dio-opt '(:struct dio-option) 'f0-floor) 71.0d0
              (foreign-slot-value dio-opt '(:struct dio-option) 'allowed-range) 0.1d0
              (foreign-slot-value ct-opt '(:struct cheap-trick-option) 'q1) -0.15d0
              (foreign-slot-value ct-opt '(:struct cheap-trick-option) 'f0-floor) 71.0d0)
              
        (let* ((fs (mem-ref fs :int))
               (nbit (mem-ref nbit :int))
               (f0-length (get-samples-for-dio fs len frame-period))
               (fft-size  (get-fft-size-for-cheap-trick fs ct-opt))
               (y-length (floor (+ (/ (* (1- f0-length) frame-period fs) 1000.0) 1))))
          (with-foreign-objects ((f0 :double f0-length)
                                 (time-axis :double f0-length)
                                 (refined-f0 :double f0-length)
                                 (spectrogram :pointer f0-length)
                                 (aperiodicity :pointer f0-length)
                                 (y :double y-length))
            ;; Run DIO and StoneMask (F0 Estimation)
            (time (dio x len fs dio-opt time-axis f0))
            (time (stone-mask x len fs time-axis f0 f0-length refined-f0))

            ;; ;; Plot F0
            ;; (clgp:plot-lists
            ;;  (list
            ;;   (loop for i from 0 to (1- f0-length) collect (mem-aref f0 :double i))
            ;;   (loop for i from 0 to (1- f0-length) collect (mem-aref refined-f0 :double i)))
            ;;  :title-list '("f0" "refined-f0"))

            ;; CheapTrick&D4C

            ;; allocate spectrogram and aperiodicity sub-arrays
            (loop for i from 0 to (1- f0-length) do
              (setf (mem-aref spectrogram :pointer i)
                    (foreign-alloc :double :count (1+ (/ fft-size 2)))
                    (mem-aref aperiodicity :pointer i)
                    (foreign-alloc :double :count (1+ (/ fft-size 2)))))

            (format t "f0-lengh:~A, half-fft-size:~A~%" f0-length (1+ (/ fft-size 2)))
            
            (cheap-trick x len fs time-axis f0 f0-length ct-opt spectrogram)
            (d4c x len fs time-axis f0 f0-length fft-size d4c-opt aperiodicity)

            ;; Plot spectrogram and aperiodicity
            (let* ((dim-list (list f0-length (1+ (/ fft-size 2))))
                   (spectrogram-result  (make-array dim-list))
                   (aperiodicity-result (make-array dim-list)))

              ;; set spectrogram and aperiodicity result to Lisp matrix
              (loop for i from 0 to (1- f0-length) do
                (loop for j from 0 to (/ fft-size 2) do
                  (setf (aref spectrogram-result i j)
                        (mem-aref (mem-aref spectrogram :pointer i) :double j)
                        (aref aperiodicity-result i j)
                        (mem-aref (mem-aref aperiodicity :pointer i) :double j))))
              
              ;; (clgp:splot-matrix spectrogram-result)
              ;; (clgp:splot-matrix aperiodicity-result)
              )
            
            ;; Synthesis
            (format t "x-length: ~A, y-length:~A~%" len y-length)
            (wave-synthesis f0 f0-length spectrogram aperiodicity fft-size frame-period fs y-length y)

            ;; ;; Plot Synthesis result
            ;; (clgp:plot-lists
            ;;  (list
            ;;   (loop for i from 0 to (1- len) collect (mem-aref x :double i))
            ;;   (loop for i from 0 to (1- y-length) collect (mem-aref y :double i))))

            ;; write result
            (with-foreign-string (s "/home/wiz/tmp/wavresult.wav")
              
              (wavwrite y y-length fs nbit s))
            
            ;; free sub-arrays
            (loop for i from 0 to (1- f0-length) do
              (foreign-free (mem-aref spectrogram :pointer i))
              (foreign-free (mem-aref aperiodicity :pointer i)))
            ))))))

;;; Structs

(defstruct world-wav
  fs nbit data)

(defstruct world-params
  fs   ; frame-rate
  nbit ; number of bits for quantization
  (frame-period 5.0d0)
  (speed 1)
  (f0-floor 71.0d0)
  (allowed-range 0.1d0)
  (q1 -0.15d0)
  ;; Analysis results
  f0
  spectrogram
  aperiodicity)

(defun make-world-wav-from-file (file-path)
  (with-foreign-string (s file-path)
    (let ((len (get-audio-length s)))
      (with-foreign-objects ((fs :int) (nbit :int) (x :double len))
        (wavread s fs nbit x)
        (let ((wav (make-world-wav :fs (mem-ref fs :int)
                                   :nbit (mem-ref nbit :int)
                                   :data (make-array len :element-type 'double))))
          (loop for i from 0 to (1- len) do
            (setf (aref (world-wav-data wav) i)
                  (mem-aref x :double i)))
          wav)))))

;; (defparameter wav-obj
;;   (make-world-wav-from-file "/home/wiz/cl-libworld/lib/World/test/vaiueo2d.wav"))

(defun output-world-wav-to-file (wav file-path)
  (let* ((data (world-wav-data wav))
         (len (length data)))
    (with-foreign-object (x :double len)
      (loop for i from 0 to (1- len) do
        (setf (mem-aref x :double i) (aref data i)))
      (with-foreign-string (s file-path)
        (wavwrite x len (world-wav-fs wav) (world-wav-nbit wav) s)))))

;; (output-world-wav-to-file wav-obj "/home/wiz/tmp/vaiueo2d-out.wav")

(defun analysis (wav &key
                       (frame-period 5.0d0) (speed 1) (f0-floor 71.0d0)
                       (allowed-range 0.1d0) (q1 -0.15d0))
  (let* ((params (make-world-params :fs (world-wav-fs wav) :nbit (world-wav-nbit wav)
                                    :frame-period frame-period :speed speed :f0-floor f0-floor
                                    :allowed-range allowed-range :q1 q1))
         (len (length (world-wav-data wav)))
         (frame-period (world-params-frame-period params)))
    (with-foreign-objects ((x :double len)
                           (dio-opt '(:struct dio-option))
                           (ct-opt '(:struct cheap-trick-option))
                           (d4c-opt '(:struct d4c-option)))
      (loop for i from 0 to (1- len) do
        (setf (mem-aref x :double i) (aref (world-wav-data wav) i)))
      
      (initialize-dio-option dio-opt)
      (initialize-cheap-trick-option ct-opt)
      (initialize-d4c-option d4c-opt)
      
      (setf (foreign-slot-value dio-opt '(:struct dio-option) 'frame-period)
            frame-period
            (foreign-slot-value dio-opt '(:struct dio-option) 'speed)
            (world-params-speed params)
            (foreign-slot-value dio-opt '(:struct dio-option) 'f0-floor)
            (world-params-f0-floor params)
            (foreign-slot-value dio-opt '(:struct dio-option) 'allowed-range)
            (world-params-allowed-range params)
            (foreign-slot-value ct-opt '(:struct cheap-trick-option) 'q1)
            (world-params-q1 params)
            (foreign-slot-value ct-opt '(:struct cheap-trick-option) 'f0-floor)
            (world-params-f0-floor params))
      
      (let* ((fs (world-params-fs params))
             (f0-length (get-samples-for-dio fs len frame-period))
             (fft-size  (get-fft-size-for-cheap-trick fs ct-opt)))
        (with-foreign-objects ((f0 :double f0-length)
                               (time-axis :double f0-length)
                               (refined-f0 :double f0-length)
                               (spectrogram :pointer f0-length)
                               (aperiodicity :pointer f0-length))
          ;; Run DIO and StoneMask (F0 Estimation)
          (dio x len fs dio-opt time-axis f0)
          (stone-mask x len fs time-axis f0 f0-length refined-f0)

          ;; set result
          (let ((refined-f0-array (make-array f0-length :element-type 'double-float)))
            (loop for i from 0 to (1- f0-length) do
              (setf (aref refined-f0-array i) (mem-aref refined-f0 :double i) ))
            (setf (world-params-f0 params) refined-f0-array))
          
          ;; CheapTrick&D4C
          ;; allocate spectrogram and aperiodicity sub-arrays
          (loop for i from 0 to (1- f0-length) do
            (setf (mem-aref spectrogram :pointer i)
                  (foreign-alloc :double :count (1+ (/ fft-size 2)))
                  (mem-aref aperiodicity :pointer i)
                  (foreign-alloc :double :count (1+ (/ fft-size 2)))))
          
          (cheap-trick x len fs time-axis f0 f0-length ct-opt spectrogram)
          (d4c x len fs time-axis f0 f0-length fft-size d4c-opt aperiodicity)

          ;; set result
          ;; Plot spectrogram and aperiodicity
          (let* ((dim-list (list f0-length (1+ (/ fft-size 2))))
                 (spectrogram-result  (make-array dim-list :element-type 'double-float))
                 (aperiodicity-result (make-array dim-list :element-type 'double-float)))

            ;; set spectrogram and aperiodicity result to Lisp matrix
            (loop for i from 0 to (1- f0-length) do
              (loop for j from 0 to (/ fft-size 2) do
                (setf (aref spectrogram-result i j)
                      (mem-aref (mem-aref spectrogram :pointer i) :double j)
                      (aref aperiodicity-result i j)
                      (mem-aref (mem-aref aperiodicity :pointer i) :double j))))
            (setf (world-params-spectrogram params) spectrogram-result
                  (world-params-aperiodicity params) aperiodicity-result))

          params)))))

(defun synthesis (params)
  (let* ((f0-length (length (world-params-f0 params)))
         (y-length (floor (+ (/ (* (1- f0-length)
                                   (world-params-frame-period params)
                                   (world-params-fs params))
                                1000.0) 1)))
         (fft-size (* 2 (1- (array-dimension (world-params-spectrogram params) 1))))
         (out-wav (make-world-wav :fs (world-params-fs params)
                                  :nbit (world-params-nbit params)
                                  :data (make-array y-length :element-type 'double-float))))
    (with-foreign-objects ((f0 :double f0-length)
                           (spectrogram :pointer f0-length)
                           (aperiodicity :pointer f0-length)
                           (y :double y-length))

      ;; allocate spectrogram and aperiodicity sub-arrays
      (loop for i from 0 to (1- f0-length) do
        (setf (mem-aref spectrogram :pointer i)
              (foreign-alloc :double :count (1+ (/ fft-size 2)))
              (mem-aref aperiodicity :pointer i)
              (foreign-alloc :double :count (1+ (/ fft-size 2)))))
      
      ;; set to sub-arrays
      (loop for i from 0 to (1- f0-length) do
        (setf (mem-aref f0 :double i) (aref (world-params-f0 params) i))
        (loop for j from 0 to (1- (array-dimension (world-params-spectrogram params) 1)) do
          (setf (mem-aref (mem-aref spectrogram :pointer i) :double j)
                (aref (world-params-spectrogram params) i j)
                (mem-aref (mem-aref aperiodicity :pointer i) :double j)
                (aref (world-params-aperiodicity params) i j))))

      ;; synthesis
      (wave-synthesis f0 f0-length spectrogram aperiodicity
                      fft-size (world-params-frame-period params) (world-params-fs params)
                      y-length y)
      
      ;; set to world-wav struct
      (loop for i from 0 to (1- y-length) do
        (setf (aref (world-wav-data out-wav) i) (mem-aref y :double i)))

      ;; free sub-arrays
      (loop for i from 0 to (1- f0-length) do
        (foreign-free (mem-aref spectrogram :pointer i))
        (foreign-free (mem-aref aperiodicity :pointer i)))
      
      out-wav)))

;; ;; 'git clone https://github.com/masatoi/clgplot' in your local-projects directory
;; (ql:quickload :clgplot)

;; (defparameter in-wav (make-world-wav-from-file "/home/wiz/cl-libworld/lib/World/test/vaiueo2d.wav"))

;; (defparameter params (analysis in-wav))
;; (clgp:plot-list (loop for elem across (world-params-f0 params) collect elem)
;;                 :title "refined-f0" :x-label "frames" :y-label "F0"
;;                 :output "/home/wiz/tmp/world-f0.png")
;; (clgp:splot-matrix (world-params-spectrogram params)
;;                    :x-label "frames" :y-label "frequency windows"
;;                    :output "/home/wiz/tmp/world-spectrogram.png")
;; (clgp:splot-matrix (world-params-aperiodicity params)
;;                    :x-label "frames" :y-label "frequency windows"
;;                    :output "/home/wiz/tmp/world-aperiodicity.png")

;; (defparameter out-wav (synthesis params))

;; (clgp:plot-lists
;;  (list (loop for elem across (world-wav-data in-wav) collect elem)
;;        (loop for elem across (world-wav-data out-wav) collect elem))
;;  :title-list '("in-wav" "out-wav") :x-label "time" :y-label "gain"
;;  :output "/home/wiz/tmp/world-in-out.png")

;; (output-world-wav-to-file out-wav "/home/wiz/tmp/vaiueo2d-out.wav")
