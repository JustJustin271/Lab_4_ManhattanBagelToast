;; manhattan-distance : posn -> num
;; Takes in a posn and out puts the sum of the x and y posn

(define (manhattan-distance apple)
 (+ (abs(posn-x apple)) (abs(posn-y apple))))

(check-expect (manhattan-distance (make-posn 3 4)) 7)
(check-expect (manhattan-distance (make-posn -3 7)) 10)



;; walk-south : posn -> posn
;; Shifts the y-coordinate down by one unit

(define (walk-south journey)
  (make-posn (posn-x journey) (- (posn-y journey) 1)))

(check-expect (walk-south (make-posn 100 101)) (make-posn 100 100))
(check-expect (walk-south (make-posn 3 -5)) (make-posn 3 -6))





;; A toast is a (make-toast Symbol Number)
;; Type is the kind of the toast that the bread was made of
;; Level is a scale from 0 - 10 of how toasted the bread was

(define-struct toast (type level))

(define breakfast (make-toast 'white 6))
(define midnight-snack (make-toast 'wheat 10))




;; yummy-toast : Toast -> Bool
;; Takes in a type of toast and outputs whether it is tasty or not (bool)

(define (yummy-toast? random-toast)
  (cond
    [(and (symbol=? (toast-type random-toast) 'white) (> (toast-level random-toast) 5) (< (toast-level random-toast) 9)) #t]
    [else #f]))

;;Check-expects
(check-expect (yummy-toast? breakfast) #true)
(check-expect (yummy-toast? midnight-snack) #false)





;; toast-more : Toast -> Toast
;; Takes in a toast and outputs further toasted bread if possible

(define (toast-more some-toast)
  (cond
    [(<= (toast-level some-toast) 9) (make-toast (toast-type some-toast) (+ (toast-level some-toast) 1))]
    [else some-toast]))

(check-expect (toast-more breakfast) (make-toast 'white 7))
(check-expect (toast-more midnight-snack) (make-toast 'wheat 10))





;; A bagel is a (make-bagel Boolean Boolean Number)
;; sliced? is boolean whether the bagel is sliced or not
;; cheese? is a boolean whether there is cheese present
;; crunch is how toasted the bagel is from 0 to 10 

(define-struct bagel (sliced? cheese? crunch))

;;Test bagels :D
(define lunch (make-bagel #true #true 4)) ;;Good bagel
(define afternoon-snack (make-bagel #false #true 6)) ;;Pretty fine bagel
(define dinner (make-bagel #false #false 10)) ;;Burnt sad bagel :(
(define raw-bagel (make-bagel #false #false 0)) ;;5-second rule!


;; yummy-bagel : Bagel -> Bool
;; This will judge whether the bagel if up to my standards:
;; 1. I don't care whether it is sliced or not tbh
;; 2. I always want cheese
;; 3. Toasted levels should be greater than 2, but less than 7 in my opinion :P
;; 4. Don't pick up the bagel from a school cafeteria floor bruh
(define (yummy-bagel? random-bagel)
  (cond
    [(and (boolean=? (bagel-cheese? random-bagel) #true) (> (bagel-crunch random-bagel) 2) (< (bagel-crunch random-bagel) 7)) #true]
    [else #false]))

(check-expect (yummy-bagel? lunch) #true)
(check-expect (yummy-bagel? afternoon-snack) #true)
(check-expect (yummy-bagel? dinner) #false)
(check-expect (yummy-bagel? raw-bagel) #false)


;; Manhattan Distance, Toast, Bagels :D
;;March 1st, 2026
