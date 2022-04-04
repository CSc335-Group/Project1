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

;;; a helper function that input a prime number, returns the index
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

;;; find k-th prime
;;; input index to find prime number
;;; 4 => 11
;;; 2 => 5
(define (k-th_prime k)
  (define (iter k i j result)
    (cond ((> i k) result)
          ((prime? j) (iter k (+ i 1) (+ j 1) j))
          (else (iter k i (+ j 1) result))))
  (iter k 0 0 0))


;;; the input number n represent list s and the input number m represent list t
;;; NOTE THAT this appoach won't work if lists have 0 at the tail, 
;;;           consider list (0 0) and (0 0 0 0)
;;;           the constructed number of the two lists are both 1,
;;;           we can not distinguish which list it represents since it contruct the same number. 
;;;           
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

;;; since s=t iff n=m
;;; then we only need to directly compare n with m, then it will return the same result of comparision of s and t.
(define (myequal? n m)
  (cond ((= n m) #t)
        (else (#f))))


  


    
