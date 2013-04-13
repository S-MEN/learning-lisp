
;;
;; [min, max] の範囲の乱数を生成する.
;;
(defun random-value (min max)
    (+ (random (+ (- max min) 1)) min))

;;
;; n 個の乱数列を生成する.
;;
(defun random-values (n)
    (defun random-values-tailrec (n acc)
        (if (= n 0)
            acc
            (random-values-tailrec
                (- n 1)
                (cons (random-value 10 99) acc))))
    (random-values-tailrec n '()))

;;
;; xs を pivot 未満のリスト (smallers) と pivot 以上のリスト (greater-or-equals) に分割する.
;; 大小判定は less-p によって行い, 分割した結果は多値 (values smallers greqter-or-equals) として返す.
;;
(defun partition (pivot xs less-p)
    (defun partition-tailrec (pivot xs less-p smallers greater-or-equals)
        (let ((x (car xs)))
            (cond
                ((null x)
                    (values smallers greater-or-equals))
                ((funcall less-p x pivot)
                    (partition-tailrec pivot (cdr xs) less-p (cons x smallers) greater-or-equals))
                (t
                    (partition-tailrec pivot (cdr xs) less-p smallers (cons x greater-or-equals))))))
    (partition-tailrec pivot xs less-p '() '()))

;;
;; xs をクイックソートで並び替える.
;;
(defun quick-sort (xs &optional (less-p #'<))
    (if (null xs)
        '()
        (multiple-value-bind
            (smallers greater-or-equals)
            (partition (car xs) (cdr xs) less-p)
            (append
                (quick-sort smallers)
                (cons (car xs) (quick-sort greater-or-equals))))))

;;
;; main.
;;
(defun main ()
    (setq *random-state* (make-random-state t))
    (let ((xs (random-values 10)))
        (print xs)
        (print (quick-sort xs))))
(main)



