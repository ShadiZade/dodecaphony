#lang racket
(require gregor)
(require gregor/time)
(require gregor/period)

(define (period->integer p f) ; -> integer?
  ; p -> period?
  (period-ref p f))
(define (pad-integer int len)
  (~r int #:min-width len #:pad-string "0"))


(define *day-0* (date 2025 02 09))
(define *ram* 1)
(define *dodec* 12)
(define *trithing* 120)
(define *zhou* 360)
(define *ram-chars* '(甚 闯 岸 豹 斐 顶 卫 田 审 启 剀 矛 匝))
(define *dodec-chars* '(甚 刃 技 凰 兴 琴 从 查 刷 汇 讴 叫 汝 云 罡 功 更 红 绕 幺 套 织 韭 飘 贝 庶 忠 郭 仍 存 幽))
(define *trithing-chars* '(甚 义 而 森))
(define *zhou-chars* '(甚 刊))


(define (rams-since-d0 y m d) ; -> integer?
  ; y; m; d -> integer?
  ; Calculates the number of rams since day 0 of the dodecaphony, 9th Feb 2025
  (let
      ([input-date (date y m d)])
      (if
       (date>? input-date *day-0*)
       (period->integer
        (date-period-between
         *day-0* input-date '(days))
        'days)
       (error (format "~a is not a valid date." input-date)))))

(define (subunits-since-value subunit rams) ; -> integer?
  ; subunit; rams -> integer?
  ; Calculates the number of subunits (dodec, trithing, zhou) from the value 'rams', optimally passed from (rams-since-d0). subunit is expressed by the number of rams in it (i.e. dodec = 12; trithing = 120; zhou = 360)
  (cond ((= subunit *ram*)
         (add1 (modulo (- rams 1) *dodec*)))
        ((= subunit *zhou*)
         (add1 (floor (/ (- rams 1) subunit))))
        (else
      (add1 (floor (/ (modulo (- rams 1) *zhou*) subunit))))))

(define (subunits-since-d0 subunit y m d) ; -> integer?
  ; subunit; y; m; d -> integer?
  ; Passes (rams-since-d0) into (subunits-since-value).
  (subunits-since-value subunit (rams-since-d0 y m d)))
      

(define (georgian->dodec-list y m d) ; -> list?
  ; y; m; d -> integer?
  ; passes a georgian date into (subunits-since-0).
  ; outputs all 4 dodecaphony subunits in a list.
  (map (λ (subunit)
         (subunits-since-d0 subunit y m d))
       (list *zhou* *trithing* *dodec* *ram*)))
       
(define (dodec-list->char-list dodec-list)
  (let
      ([ram      (list-ref dodec-list 3)]
       [dodec    (list-ref dodec-list 2)]
       [trithing (list-ref dodec-list 1)]
       [zhou     (list-ref dodec-list 0)]
       [get-index (λ (index charlist)
                    (if
                     (< index (length charlist))
                     (list-ref charlist index)
                     (list-ref charlist 0)))])

       (map get-index
            (list
             zhou
             trithing
             dodec
             ram)
            (list
             *zhou-chars*
             *trithing-chars*
             *dodec-chars*
             *ram-chars*))
    ))
       
(define (pretty-print-dodec-list dodec-list)
  (let*
      ([ram       (list-ref dodec-list 3)]
       [dodec     (list-ref dodec-list 2)]
       [trithing  (list-ref dodec-list 1)]
       [zhou      (list-ref dodec-list 0)]
       [char-list (dodec-list->char-list dodec-list)]
       [ram-char       (list-ref char-list 3)]
       [dodec-char     (list-ref char-list 2)]
       [trithing-char  (list-ref char-list 1)]
       [zhou-char      (list-ref char-list 0)])
    (printf "∎∎∎∎ ~a.~a.~a.~a ~a~a~a~a"
            (pad-integer zhou 2)
            trithing
            (pad-integer dodec 2)
            (pad-integer ram 2)
            zhou-char
            trithing-char
            dodec-char
            ram-char)))


;; (subunits-since-d0 *ram* 2026 03 01)
;; (rams-since-d0 2026 03 01)
;; (georgian->dodec-list 2026 03 01)
;; (map (λ (day) (georgian->dodec-list 2025 06 day))
;;      (range 1 31))
;; (dodec-list->char-list (georgian->dodec-list 2025 02 23))
;; (pretty-print-dodec-list  (georgian->dodec-list 2025 02 23))
