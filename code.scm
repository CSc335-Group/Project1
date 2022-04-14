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
        (else #f)))


;;; head function

;;; testing data
;;; 5512500000 (5 2 8 2)=> 5
;;;     360    (3 2 1)  => 3
;;; 6251175 (0 4 2 3 1) => 0
(define (head n)
  (define (iter n i)
    (cond ((= (remainder n 2) 0) (iter (quotient n 2) (+ i 1)))
          (else i)))

  (iter n 0))

;;; TODO PROOF

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


;;; --------------------------------------------------------------------------------------------------------------
;;; TAIL FUNCTION
;;; Specification:
;;; Pre-condition: inputs an integer n>=1 that represents a list s
;;; Post-condition: returns an integer representing the list obtained from s by removing it's first element

;;; DESIGN IDEA:
;;; NOTE: S is the original inputed list. N is the the number representing list S.
;;; We use an iterative approach. We'll have an index variable, i, that iterates through each index in list S from 1 to
;;; (length of S)-1, and another variable, result, that will hold our new constructed list. To construct the new list,
;;; we will simply extract the element at index i from n, where
;;;         n = N / (2^(element at index 0 of S))*(3^(element at index 1))*...*(((i-1)th prime)^(element at i-1))
;;; and add it to the (i-1)th position in the new list result.
;;; In other words, result will be all elements in S (except the first element) shifted to the left by one.

;;; GUESS INVARIANT (GI):
;;; result = number representing the list containing all elements in S from 1 to i-1

;;; GUESS CODE:
;;; Local Variable Explanations:
;;;    h: the element in S at position i
;;;    cp: the ith prime
;;;    prev_p: the (i-1)th prime

(define (tail n)
  (define (iter n result i)
    (let ((h (ref n i)) (cp (k-th_prime i)) (prev_p (k-th_prime (- i 1))))
      (cond ((= n 1) result)
            (else (iter (/ n (expt cp h)) (* result (expt prev_p h)) (+ i 1))))))
  (iter (/ n (expt 2 (head n))) 1 1))

;;; TESTS:
;;; WEAK ENOUGH? In the very first call, i is set to 1 and result is set to the empty list 1. The GI says that result =
;;; the # representing the list containing all elements in S from 1 to 0, in other words. There are no such
;;; elements in S from index 1 to 0 because the indexing doesn't make sense so initially result represents the empty list.
;;; Thus, our GI is true in the first call.

;;; STRONG ENOUGH? The program terminates there are no more elements in S, at which point i is (length of S).
;;; The termination condition combined w/ the GI = result represents the list containing all elements
;;; from 1 to (length of S)-1 implies the Post-condition.

;;; PRESERVABLE? Preserving the GI is simple. We make sure at each call to iter we increment i by 1 and multiply
;;; result with ((i-1)th prime)^(element at i in S)

;;; testing data
;;; 5512500000 (5 2 8 2) => 656100 (2 8 2)
;;;     360    (3 2 1)   => 12 (2 1)
;;; 7640325 (0 4 2 3 1)  => 126000 (4 2 3 1)
;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; MYAPPEND FUNCTION
;;; Specification:
;;; Pre-condition: inputs two integers m>=1 and n>=1 that represent lists s and t, respectively
;;; Post-condition: returns an integer representing the list obtained from appending s and t

;;; DESIGN IDEA:
;;; NOTE: S and T are the original inputed lists. M and N represent S and T, respectively
;;; We use an iterative approach. The idea is to extract each element in T and append
;;; them to the end of S to create a new list. We'll have two variables: m and n. m will be the number
;;; representing the appended list so far (the list we're constructing) and n will the number representing
;;; the list containing all the elements in T we have not yet appended.

;;; GUESS INVARIANT (GI):
;;; m = number representing the appended list so far &&
;;; n = number representing the list containing all the elements in T not yet appended

;;; GUESS CODE:
;;; Local Variable Explanations:
;;;    ls: the length of the appended list so far
;;;    first_elt: the first element in the list containing all elements in T not yet processed
;;;    rest: the list containing all elements in T not yet processed except the first element

(define (myappend m n)
  (define (iter m n)
    (let ((ls (len m)) (first_elt (head n)) (rest (tail n)))
      (cond ((= n 1) m)
            (else (iter (* m (expt (k-th_prime ls) first_elt)) rest)))))
  (iter m n))

;;; TESTS:
;;; WEAK ENOUGH? In the first call to iter, m is set to M and n is set to N. The GI remains true because
;;; the appended list so far is just the list S since we have not yet appended the list T. Also, In the beginning,
;;; the elements we still need to process are all the elements in T which is saved to n.

;;; STRONG ENOUGH? The GI states that m is the appended list so far and n is the list with all elements not yet
;;; processed. When the termination triggers, the list represented by n is empty, meaning all elements in T have
;;; been processed and m really represents the list formed by appending S and T. Thus the termination condition
;;; and the GI implies the post-condition.

;;; PRESERVABLE? Preserving the GI is simply a matter of appending first_elt to the list represented by m and setting n
;;; to rest. To preserve m, we find the prime at the index ls (which is the length of the appended list so far),
;;; raising it to the first_elt power, and multiplying the result with m. Now the remaining elements of T that
;;; need to be processed are in the variable rest, so to preserve the GI we set n to rest.

;;; testing data
;;; 288 (5 2) and 18 (1 2) => 70560 (5 2 1 2)
;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; MYREVERSE FUNCTION
;;; Specification:
;;; Pre-condition: inputs an integer n>=1 that represents a list s
;;; Post-condition: returns an integer representing the list obtained by reversing s

;;; DESIGN IDEA:
;;; NOTE: S is the original inputed list. N is the the number representing list S.
;;; We use an iterative approach. The idea is to iterate through S backwards using an index variable called
;;; index and append each element to a new list, represented by the variable result, at the (length of S)-1-index
;;; position.

;;; GUESS INVARIANT (GI):
;;; result = number representing the reversed list so far containing all elements
;;; from position (length of S)-1 to index+1

;;; GUESS CODE:
;;; Local Variable Explanations:
;;;    l: index of the last element in S

(define (myreverse n)
  (let ((l (- (len n) 1)))
    (define (iter n index result)
      (cond ((< index 0) result)
            (else (iter n
                        (- index 1)
                        (* result (expt (k-th_prime (- l index)) (ref n index)))))))
    (iter n l 1)))

;;; TESTS:
;;; WEAK ENOUGH? In the first call to iter, n is set N and the program never changes it; index is set to the index of
;;; the last element in S; and result is set to 1. The GI is true in the first call because we have
;;; not yet processed any of the elements in S so it is correct that result represents the empty list.

;;; STRONG ENOUGH? The GI states that result represents the reversed list so far from position (length of S)-1 to index+1.
;;; When the termination triggers, index becomes an invalid index (-1) and result therefore represents the list
;;; containing the elements in S from position (length of S)-1 to 0. Thus, both the GI and termination condition
;;; imply the post-condition.

;;; PRESERVABLE? Preserving the GI is simply a matter of appending the element at index to the (length of S)-1-index
;;; position of the new list represented by result

;;; testing data
;;; 900000 (5 2 5) => 900000 (5 2 5)
;;;     360    (3 2 1) => 2250 (1 2 3)
;;; 126000 (4 2 3 1) => 3241350 (1 3 2 4)
;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; PALIN FUNCTION
;;; Specification:
;;; Pre-condition: inputs an integer n>=1 that represents a list s
;;; Post-condition: returns #t if s is a palindrome, #f otherwise

;;; DESIGN IDEA:
;;; NOTE: S is the original inputed list. N is the the number representing list S.
;;; We use an iterative approach. The idea is to iterate through S using a variable called index from
;;; position 0 to (length of S)/2 and compare it with it's corresponding pair at the end of the list
;;; located at the position (length of S)-1-index. Save the result of the comparison in a variable called
;;; result that we maintain through each iteration.

;;; GUESS INVARIANT (GI):
;;; result = S is a palindrome so far from position 0 to index-1

;;; GUESS CODE:
;;; Local Variable Explanations:
;;;    l: length of S

(define (palin? n)
  (define (iter n index result)
    (let ((l (len n)))
      (cond ((equal? result #f) result)
            ((> index (/ l 2)) result)
            ((= (ref n index) (ref n (- (- l 1) index))) (iter n (+ index 1) #t))
            (else (iter n (+ index 1) #f)))))
  (iter n 0 #t))

;;; TESTS:
;;; WEAK ENOUGH? In the first call to iter, n is set N and the program never changes it; index is set to 0;
;;; and result is set to #t. The GI is true in the first call because we have
;;; not yet processed any of the elements in S and the empty string is a palindrome.

;;; STRONG ENOUGH? The GI states that result returns true if S is a palindrome from position 0 to index-1. There are
;;; two termination conditions. If processing the element at the current index resulted in S no longer being a palin-
;;; drome we immediately stop checking and return; S is not a palindrome from position 0 to index, thus the post-cond-
;;; ition is implied. The other termination condition triggers when index is greater than (length of S)/2. In that case,
;;; the GI is result = S is a palindrome so far from 0 to (length of S)/2, returns true. Thus, anding both the
;;; termination condition and the GI implies the post-condition.

;;; PRESERVABLE? To preserve the GI, at each iteration we compare the element at position index to the element at
;;; position (length of S)-1-index in S and save the result to the variable result.
;;; removes element at index i from the given list

;;; testing data
;;; 900000 (5 2 5) => #t
;;;     2    (1) => #t
;;; 126000 (4 2 3 1) => #f
;;; --------------------------------------------------------------------------------------------------------------


;;; HELPER FUNCTIONS FOR SORT
;;; --------------------------------------------------------------------------------------------------------------
;;; REMOVE-AT FUNCTION
;;; Specification:
;;; Pre-condition: inputs an integer n>=1 that represents a list s and an index 0<=i<=(len s)-1
;;; Post-condition: returns a number representing the list obtained from removing the element
;;; at index i from s

;;; DESIGN IDEA:
;;; NOTE: S is the original inputed list. N is the the number representing list S.
;;; We use an iterative approach. The idea is to build a new list, represented by the variable
;;; result, that will hold all the elements in S except the element at i. We'll do this by
;;; having two index variables: nIndex and resIndex. nIndex will be used to step through list S, and
;;; resIndex will be used to build the new list (represented by result).

;;; GUESS INVARIANT (GI):
;;; result = number representing the list containing all elements in S at position nIndex
;;;          except when nIndex=i

;;; GUESS CODE:
;;; Local Variable Explanations:
;;;    p: the prime at index resIndex
;;;    value: the value in S at position nIndex

(define (remove-at n i)
  (define (iter n result nIndex resIndex)
    (let ((p (k-th_prime resIndex)) (value (ref n nIndex)))
      (cond ((= nIndex (len n)) result)
            ((= nIndex i) (iter n result (+ nIndex 1) resIndex))
            (else (iter n (* result (expt p value)) (+ nIndex 1) (+ resIndex 1))))))
  (iter n 1 0 0))

;;; TESTS:
;;; WEAK ENOUGH? In the first call to iter, n is set N and the program never changes it; result is
;;; set to 1 and nIndex and resIndex are both set to 0. In this first call, the GI is true because we
;;; have not yet processed any of the elements in S so result represents the empty list.

;;; STRONG ENOUGH? The GI states that result represents the list containing all elements in S
;;; at index nIndex except when nIndex=i. The termination condition triggers when we've iterated through
;;; all the elements in S. Combining the GI with the termination condition, result will represent
;;; the list containing all the elements in S except the element at i; thus our post-condition is true.

;;; PRESERVABLE? To preserve the GI, at each iteration we check if nIndex equals i. If not, we simply add
;;; the element at nIndex in S to result and increment nIndex and resIndex. Else, we increment nIndex
;;; but do nothing to result or resIndex.

;;; testing data
;;; 112500000 (5 2 8) i=1 => 209952 (5 8)
;;;     360    (3 2 1) i=0 => 12 (2 1)
;;; 126000 (4 2 3 1) p=3  => 18000 (4 2 3)
;;; --------------------------------------------------------------------------------------------------------------


;;; min function to find min of list
; testing data
;; 112500000 (5 2 8) => index = 1
;;     360    (3 2 1) => index = 2
;; 126000 (4 2 3 1) => index = 3
(define (myMin n)
  (define (iter n currMin index counter)
    (let ((first_elt (head n)) (rest (tail n)))
      (cond ((= n 1) index)
            ((< first_elt currMin) (iter rest first_elt counter (+ counter 1)))
            (else (iter rest currMin index (+ counter 1))))))
  (iter n (head n) 0 0))

;;; second sort
;;; using selection sort
;; testing data
;; 5512500000 (5 2 8 2) => 648540112500 (2 2 5 8)
;;     360    (3 2 1)   => 2250 (1 2 3)
;; 126000 (4 2 3 1)  => 5402250 (1 2 3 4)
(define (sort n)
  (define (iter unsorted sorted counter)
    (let ((min_index (myMin unsorted)))
      (cond ((= unsorted 1) sorted)
            (else (iter (remove-at unsorted min_index)
                        (* sorted (expt (k-th_prime counter) (ref unsorted min_index)))
                        (+ counter 1))))))
  (iter n 1 0))



;;; SET FUNCTIONS
;;; To implement sets, we will still use lists but now everytime we add an element to a list
;;; we will first check if that element is already part of the list. If not, we add it. Otherwise,
;;; just return the original list

;;; myset function
;;; takes in a number representing a list that may not necessarily be a set (ie. contains duplicate elements)
;;; mySet returns the list w/o any duplicate elements
;; testing data
;; 5512500000 (5 2 8 2) => 112500000 (5 2 8)
;;     37800    (3 3 2 1)   => 360 (3 2 1)
;; 126000 (4 2 3 1)  => 126000 (4 2 3 1)
(define (mySet n)
  (define (iter n result counter)
    (let ((first_elt (head n)) (rest (tail n)))
      (cond ((= n 1) result)
            ((element-of? result first_elt) (iter rest result counter))
            (else (iter rest (* result (expt (k-th_prime counter) first_elt)) (+ counter 1))))))
  (iter n 1 0))


;; element-of? function
; testing data
;; 112500000 (5 2 8) p=8 => #t
;;     360    (3 2 1) p=11 => #f
;; 126000 (4 2 3 1) p=1  => #t
(define (element-of? n p)
  (define (iter n p counter result)
    (cond ((equal? result #t) result)
          ((= counter (len n)) result)
          ((= p (ref n counter)) (iter n p (+ counter 1) #t))
          (else (iter n p (+ counter 1) #f))))
  (iter n p 0 #f))

;; subset-of? function
;; check if m is a subset of n
; testing data
;; n=112500000 (5 2 8), m=26244 (2 8) => #t
;;     360    (3 2 1), m=8 (3) => #t
;; 126000 (4 2 3 1) m=1350000 (4 3 5)  => #f
(define (subset-of? m n)
  (let ((last_index (- (len m) 1)) (last_elt (last m)))
    (cond ((= (len m) 1) (element-of? n last_elt))
          ((and (element-of? n last_elt)
                (subset-of? (/ m (expt (k-th_prime last_index) last_elt)) n))
           #t)
          (else #f))))

;; equal-sets? function
;; idea is to check if the sets have the same length
;; if not, immediately return false
;; else, check if one set is a subset of the other
; testing data
;; n=112500000 (5 2 8), m=26244 (2 8) => #f
;;     360    (3 2 1), m=1500 (2 1 3) => #t
;;     360    (3 2 1), m=360 (3 2 1) => #t
(define (equal-sets? m n)
  (cond ((not (= (len m) (len n))) #f)
        (else (subset-of? m n))))



;; union function
;; approach: append each element in n to the back of  m
;; before appending an element first check if it's already in n
;; if so, do not append that element
;; if not, do append that element
; testing data
;; m=112500000 (5 2 8), n=26244 (2 8) => 112500000 (5 2 8)
;;     360    (3 2 1), n=8 (3) => 360 (3 2 1)
;; 360    (3 2 1), n=5000 (3 4) =>   864360  (3 2 1 4)
(define (union m n)
  (define (iter m n rsf counter)
    (let ((len_n (len n)) (counter_val_n (ref n counter)))
      (cond ((= counter len_n) rsf)
            ((element-of? m counter_val_n) (iter m n rsf (+ counter 1)))
            (else (iter m n (myappend rsf (expt 2 counter_val_n)) (+ counter 1))))))
  (iter m n m 0))


;; intersection function
;; approach: find the list with the smallest length
;; iterate through all of the elements in that list to check if they are in the other list
;; if the check passes, append each element
; testing data
;; m=112500000 (5 2 8), n=26244 (2 8) => 26244 (2 8)
;;     360    (3 2 1), n=8 (3) => 8 (3)
;; 360    (3 2 1), n=2592 (5 4) =>  1 (empty set)
(define (intersection m n)
  (define (iter maxList minList rsf counter)
    (let ((len_minList (len minList)) (counter_val (ref minList counter)))
      (cond ((= counter len_minList) rsf)
            ((element-of? maxList counter_val) (iter maxList minList (myappend rsf (expt 2 counter_val)) (+ counter 1)))
            (else (iter maxList minList rsf (+ counter 1))))))
  (let ((min_len (min (len m) (len n))))
    (cond ((= min_len (len m)) (iter n m 1 0))
          (else (iter m n 1 0)))))
