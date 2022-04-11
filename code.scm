;;; Project 1
;;; Group member: Baishaki Debi, Yi Lin
;;; Spring 2022

;;; draft, with/without complete proof
;;; some helper function

;;; for future use (maybe?)
(define (count n)
  (+ 1 (floor (log n 10))))


;;; a helper function determine whether a number n is prime
;;; if the number n is prime, returns #t, otherwise #f

;;; Note that, if a number n is not a prime, the there should be a factor a and b, that n=a*b
;;; where a <= sqrt(n) and b >= sqrt(n),
;;; then we only need to track whether there exists a number k <= sqrt(n) so that n can be excact divide by k (without remainder)
;;; if the number does not exists, then the number is a prime

;;; there should be a faster solution using Sieve of Eratosthenes, but for now I'm not using this
;;; may implement later
(define (prime? n)
  (define (iter n i)
    (let ((sqrt-n (floor (sqrt n))))
      (cond ((> i sqrt-n) #t)
            ((= (remainder n i) 0) #f)
            (else (iter n (+ i 1))))))
  
  (cond ((< n 2) #f)
        (else (iter n 2))))

;;; Specification: a helper function that input a prime number, returns the index
;;; 2 => 5
;;; 5 => 13
;;; if input number is not prime, it will return -1
;;; otherwise it will return the index of prime number
(define (prime_index n)
  (define (iter n i result)
    (cond ((> i n) result)
          ((prime? i) (iter n (+ i 1) (+ result 1)))
          (else (iter n (+ i 1) result))))
  (cond ((prime? n) (iter n 2 -1))
        (else -1)))

;;; Specification: a helper function that input a index k, then it returns the k-th prime number
;;; pre-condition: input a non-negetive index k
;;; post condition:  returns the k-th prime number

;;; Design Idea:
;;; using a iterative process, find the k-th prime number
;;; we start with a counter j, that start counting from 0, to determine whether current j is a prime, and another counter i,
;;; that keep track of how many prime number that we go through so far, and a variable result, keep track of the last prime number's value
;;; the program will terminate when i > k, that is when we go through all the k-th prime number, then it will returns the last prime number's value that we go through

;;; Then our GI will be: i = largest index of prime number <= j, result = i-th prime number

;;; Here is our GUESS CODE:

(define (k-th_prime k)
  (define (iter k i j result)
    (cond ((> i k) result)
          ((prime? j) (iter k (+ i 1) (+ j 1) j))
          (else (iter k i (+ j 1) result))))
  (iter k 0 0 2))

;;; as we can see, when the iterative process start, we pass i j to be 0 (initially), and result to be 2
;;; Proof.
;;; weak enough? When the iterative process start, the result is 2, and both i and j are 0, then our GI: i is the largest index of prime number <= 0 which is 0, and
;;;              result is 0-th prime number, which is 2, which is correct

;;; strong enough? When the prgram termintates, that is j go through all the k-th prime number, which means i>k, and i is the largest index of prime number <= j
;;;                then i will be k-th prime number, the result will be the value of i-th prime number, which in this case, is also the k-th prime number, which is correct

;;; maintained? if current j is prime, then (i+1) would be the largest index of prime number <= (j+1) and result = j (which in this case, the (i+1)th prime number
;;;             if current j is not prime, then i would be the largest index of prime number <= (j+1) and result is still maintained the previous result value
;;;             which in this case, maintained our GI to be True

;;; testing data
;;; 0 => 2
;;; 4 => 11
;;; 2 => 5
;;; Then we can say our program to be true


;;; -----------------------------------------------------------------------------------------------------------------------------------
;;; -- a function myequal? which inputs numbers n representing a list s and m representing a list t, and which checks whether s and
;;; -----------------------------------------------------------------------------------------------------------------------------------

;;; the input number n represent list s and the input number m represent list t
;;; NOTE THAT this appoach won't work if lists have 0 at the tail, (since it only allows positive integer in list)
;;;           consider list (0 0) and (0 0 0 0)
;;;           the constructed number of the two lists are both 1,
;;;           we can not distinguish which list it represents since it contruct the same number. 
;;;           

;;; IDEA: s=t iff n=m
;;; then we only need to directly compare n with m, then it will return the same result of comparision of s and t.
(define (myequal? n m)
  (cond ((= n m) #t)
        (else #f)))

;;; proof.
;;; 1. if s=t then n=m
;;;    this one is trivial since we construct n by s, and construct m by t, since s=t and prime number index never change,
;;;    therefore, the statement is true.
;;; 2. if n=m then s=t
;;;    proof by contradiction
;;;    Assume s not equal to t, but n = m
;;;    we can note p[k] as k-th prime number
;;;    then we have n = p[0]^s[0] * p[1]^s[1] * p[2]^s[2] ... * p[i]^s[i]
;;;    and similarly m = p[0]^t[0] * p[1]^t[1] * p[2]^t[2] ... * p[j]^t[j]
;;;    and since n = m
;;;    therefore n-m = 0, then:
;;;    PI(p[i]^s[i]) - PI(p[j]^t[j]) = 0
;;;    p[0]^s[0] * p[1]^s[1] * p[2]^s[2] ... * p[i]^s[i] - p[0]^t[0] * p[1]^t[1] * p[2]^t[2] ... * p[j]^t[j] = 0
;;;    factor out p[0]^s[0] * p[1]^s[1] * p[2]^s[2] ... * p[i]^s[i] then we consider
;;;    (1) if i < j:
;;;        p[0]^s[0] * p[1]^s[1] * p[2]^s[2] ... * p[i]^s[i] * (1 - p[0]^(t[0]-s[0]) * ... * p[i]^(t[i]-s[i]) * ... * p[j]^t[j]) = 0
;;;        we know p[0]^s[0] * p[1]^s[1] * p[2]^s[2] ... * p[i]^s[i] = n which is non-zero
;;;        then (1 - p[0]^(t[0]-s[0]) * ... * p[i]^(t[i]-s[i]) * ... * p[j]^t[j]) = 0
;;;        then p[0]^(t[0]-s[0]) * ... * p[i]^(t[i]-s[i]) * ... * p[j]^t[j] = 1

;;;        here we consider all the exponantial part of LHS, if all of them are > 0, then the whole LHS must be > 1

;;;        if some of them are > 0 and some of them are < 0, then we can write our LHS as a fraction
;;;        some prime P ^ some power / some prime Q ^ some power, since P and Q are prime, then there will not be any common divisor between them,
;;;        hence the whole term can't be elimintate as a integer, therefore it can not be 1

;;;        the only way to make the left term to be 1 is that all the exponential part of LHS to be 0, but we know the list t is a list that do not have 0 at the tail (See NOTE THAT),
;;;        hence t[j] can't be 0, therefore, the whole LHS can not be 1
;;;        hence this case is not possible

;;;    (2) if j < i:
;;;        this case is very similar to case (1), but we can factor out p[0]^t[0] * p[1]^t[1] * p[2]^t[2] ... * p[j]^t[j] instead, then we can see that this case is also not possible

;;;    (3) if i = j:
;;;        also, like case (1), we can factor out p[0]^s[0] * p[1]^s[1] * p[2]^s[2] ... * p[i]^s[i], then we have
;;;        p[0]^s[0] * ... * p[i]^s[i] * (1 - p[0]^(t[0]-s[0]) * ... * p[i]^(t[i]-s[i])), since p[0]^s[0] * ... * p[i]^s[i] = n which is non-zero
;;;        then p[0]^(t[0]-s[0]) * ... * p[i]^(t[i]-s[i]) = 1
;;;        similar discussion as we did in case (1)
;;;        the only way to make the LHS to be 1 is that all the exponential part of LHS to be 0, that is s[0] = t[0], s[1] = t[1], ... , s[i] = t[i], then the list s = list t,
;;;        it contradict to our assumption, hence this case is also not possible

;;;     We discuss all the possible 3 case of i compare to j, none of the cases are possible, hence, here comes a contradiction, therefore, we proved
;;;     The list s is equal to t iff n is equal to m. Q.E.D.


;;; --------------------------------------------------------------------------------------------------------------
;;; -- a function head which inputs a number n which represents a list s and which returns the number in the
;;;    first position of s, that is, the head of s
;;; head function
;;; ----------------------------------------------------------------------------------------------------------------
;;; Specification
;;; pre-condition: input a legal number n that represent a positive integer list
;;; post-condition: returns the first element of the list

;;; Design Idea
;;; Since the number n represent list s, which is constructed by 2^k0 + 3^k1 + 5^k2 + ...., then we only need to figure out what k0 is
;;; and also the number n is constructed by the multiplication of prime number
;;; we only need to figure out how many factor of 2 (which is the first prime number) that the number n has
;;; here we design a iterative process, which contains a counter i, keep counting how many 2s is divisible (without remainder) by the number n,
;;; and n keep reducing its value
;;; the program will terminate when the remainder of n/2 is not 0, which means n is no longer divisible (without remainder) by 2

;;; we can call our original (before the first call) input to be N, then our GI can be n*2^i = N
;;; now we have our Guess Code:
(define (head n)
  (define (iter n i)
    (cond ((= (remainder n 2) 0) (iter (quotient n 2) (+ i 1)))
          (else i)))

  (iter n 0))


;;; As we can see, when the remainder of n/2 is not 0, it will returns the counter i,
;;; Poof.
;;; GI: n*2^i = N
;;; weak enough? the initial call n to be N, and counter i to be 0, then in this case, N*2^0 = N, which is true
;;; strong enough? the stopping condition is, as we mentioned before, when n is no longer divisible by 2 without remainder, and our GI is n*2^i = N, note that in this case,
;;;                the stopping condition and GI gives us as that our i to be the largest number that N is divisible by 2^i, which means, it returns the number of 2 that construct
;;;                the number N, which represent the first element of the list
;;; maintained? since every iteritive call reduce n by 2, and add i by 1, now consider the GI: (n/2)*2^(i+1) = n*2^i = N, which is stay the same, then it is maintained. 

;;; here is the testing data
;;; 5512500000 (5 2 8 2)=> 5
;;;     360    (3 2 1)  => 3
;;; 6251175 (0 4 2 3 1) => 0
;;; as we can see, the program is true. 





;;; testing data
;;; 5512500000 (5 2 8 2) 2 => 8
;;;     360    (3 2 1) 1   => 2
;;; 7640325 (0 4 2 3 1) 3  => 3
(define (ref n k)
  (define (iter n k i)
    (let ((p (k-th_prime k)))
      (cond ((= (remainder n p) 0) (iter (quotient n p) k (+ i 1)))
            (else i))))
  (iter n k 0))
;;; TODO PROOF


;;; testing data
;;; 5512500000 (5 2 8 2) => 2
;;;     360    (3 2 1)   => 1
;;; 7640325 (0 4 2 3 1)  => 1
(define (tail n)
  (define (iter n curr i)
    (let ((h (ref n i)) ;;; h for current head
          (cp (k-th_prime i))) ;; cp for current prime
      (cond ((= n 1) curr)
            (else (iter (/ n (expt cp h)) h (+ i 1))))))
  (iter n (head n) 0))

;;; TODO PROOF


;;; 5512500000 (5 2 8 2) 2 => 16950244380300
;;; 360 (3 2 1) 3 => 37800 (3 3 2 1)
;;; 7640325 (0 4 2 3 1) 8  => 135655520000 (8 0 4 2 3 1)
(define (insert-at-head n p)
  (define (iter n result i) ;;; the result is the constructed #
    (let ((h (ref n i))
          (p (k-th_prime i))
          (np (k-th_prime (+ i 1)))) ;; next prime
      (cond ((= n 1) result)
            (else (iter (/ n (expt p h)) (* result (expt np h)) (+ i 1))))))
  (iter n (expt 2 p) 0)) ;;; initial pass 2^p to result since 2 is the first prime



;;; testing data
;;; 5512500000 (5 2 8 2) => 4
;;;     360    (3 2 1)   => 3
;;; 7640325 (0 4 2 3 1)  => 5
(define (len n)
  (define (iter n i)
    (let ((h (ref n i))
          (p (k-th_prime i)))
      (cond ((= n 1) i)
            (else (iter (/ n (expt p h)) (+ i 1))))))
  (iter n 0))
;;; TODO PROOF



;;; testing data
;;; 5512500000 (5 2 8 2) 2 => 667012500000 (5 2 8 2 2)
;;;     360    (3 2 1) 3   => 123480 (3 2 1 3)
;;; 7640325 (0 4 2 3 1) 8  => 6232447820924325 (0 4 2 3 1 8)
(define (snoc n q)
  (let ((l (len n)))
    (* n (expt (k-th_prime l) q))))


;;; ?????????????????????????????????????????????????????????????
;;; I think the result should be the same as the tail function???
;;; whats the diff between last and tail?????????????????????????
;;; ?????????????????????????????????????????????????????????????

;;; testing data
;;; 5512500000 (5 2 8 2) => 2
;;;     360    (3 2 1)   => 1
;;; 7640325 (0 4 2 3 1)  => 1

(define (last n)
  (let ((lp (k-th_prime (- (len n) 1))))
    (define (rec n p)
      (cond ((not (= (remainder n p) 0)) 0)
            (else (+ (rec (/ n p) p) 1))))
    (rec n lp)))

;;; TODO: PROOF


;;; the general idea of insert is going to construct the output
;;; the first part of the result would be # before position y
;;; then we add our inserted value to the tail of our list
;;; and last we continue our constructing

;;; testing data
;;; 5512500000 (5 2 8 2) 7 8 => 15694670722500000 (5 2 7 8 2)
;;;     360    (3 2 1)   => 1
;;; 7640325 (0 4 2 3 1)  => 1

(define (insert-at n x y)
  (define (iter n result i j x y) ;;; i is index of original list, j is index of constructed list
    (let ((p (k-th_prime j))
          (curr (ref n i))
          (l (len n)))
      (cond ((> j l) result)
            ((= j y) (iter n (* result (expt p x)) i (+ j 1) x y))
            (else (iter n (* result (expt p curr)) (+ i 1) (+ j 1) x y)))))
  (iter n 1 0 0 x y))



;;; the idea would be go through the list t
;;; and then one by one append the number value to the back of the list s
;(define (my-append s t)
;  (define (iter s t result i)
;    (let ((l (len result))
;          (p (



;; half done, will be finish very soon
