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
;;; Post-condition: returns an integer representing the list obtained from s by removing its first element

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
;;; elements in S from index 1 to 0 because the indexing doesn't make sense. So, initially result represents the empty
;;; list. Thus, our GI is true in the first call.

;;; STRONG ENOUGH? The program terminates when there are no more elements in S, at which point i is (length of S).
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
;;; the appended list so far is just the list S since we have not yet appended the list T. Also, in the beginning,
;;; the elements we still need to process are all the elements in T which is saved to n.

;;; STRONG ENOUGH? The GI states that m is the appended list so far and n is the list with all elements not yet
;;; processed. When the termination condiiton triggers, the list represented by n is empty, meaning all elements in
;;; T have been processed and m really represents the list formed by appending S and T. Thus, the termination condition
;;; and the GI imply the post-condition.

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
;;;          from position (length of S)-1 to index+1

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
;;; WEAK ENOUGH? In the first call to iter, n is set to N and the program never changes it; index is set to the index of
;;; the last element in S; and result is set to 1. The GI is true in the first call because we have
;;; not yet processed any of the elements in S so it is correct that result represents the empty list.

;;; STRONG ENOUGH? The GI states that result represents the reversed list so far from position (length of S)-1 to index+1.
;;; When the termination condition triggers, index becomes an invalid index (-1) and result therefore represents the list
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
;;; position 0 to (length of S)/2 and compare it with its corresponding pair at the end of the list
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
            ((>= index (/ l 2)) result)
            ((= (ref n index) (ref n (- (- l 1) index))) (iter n (+ index 1) #t))
            (else (iter n (+ index 1) #f)))))
  (iter n 0 #t))

;;; TESTS:
;;; WEAK ENOUGH? In the first call to iter, n is set to N and the program never changes it; index is set to 0;
;;; and result is set to #t. The GI is true in the first call because we have
;;; not yet processed any of the elements in S and the empty string is a palindrome.

;;; STRONG ENOUGH? The GI states that result returns true if S is a palindrome from position 0 to index-1. There are
;;; two termination conditions. If processing the element at the current index resulted in S no longer being a palin-
;;; drome, we immediately stop checking and return; S is not a palindrome from position 0 to index, thus the post-cond-
;;; ition is implied. The other termination condition triggers when index is greater than or equal to (length of S)/2.
;;; In that case, the GI is result = S is a palindrome so far from 0 to (length of S)/2, returns true. Thus, anding
;;; both the termination condition and the GI implies the post-condition.

;;; PRESERVABLE? To preserve the GI, at each iteration we compare the element at position index to the element at
;;; position (length of S)-1-index in S and save the result to the variable result.

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
;;;    value: the element in S at position nIndex

(define (remove-at n i)
  (define (iter n result nIndex resIndex)
    (let ((p (k-th_prime resIndex)) (value (ref n nIndex)))
      (cond ((= nIndex (len n)) result)
            ((= nIndex i) (iter n result (+ nIndex 1) resIndex))
            (else (iter n (* result (expt p value)) (+ nIndex 1) (+ resIndex 1))))))
  (iter n 1 0 0))

;;; TESTS:
;;; WEAK ENOUGH? In the first call to iter, n is set to N and the program never changes it; result is
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


;;; --------------------------------------------------------------------------------------------------------------
;;; MYMIN FUNCTION
;;; Specification:
;;; Pre-condition: inputs an integer n>1 that represents a list s
;;; Post-condition: returns the index of the minimum element in list s

;;; DESIGN IDEA:
;;; NOTE: S is the original inputed list. N is the the number representing list S.
;;; We use an iterative approach. The idea is to iterate through each element in S starting from
;;; the first element and compare it to the current minimum value which we'll save to a variable
;;; called currMin. Also, we'll have a variable named index to store the index of currMin. Finally,
;;; we'll have a counter variable that steps through each position in S. currMin is initialized to
;;; the first element in S and every subsequent element we process, we'll compare it to currMin in
;;; the following ways:
;;;    1. If element we're processing is < currMin, then change currMin to the element we're processing
;;;       and index to the position of the element we're processing which is stored in the counter variable
;;;    2. Else, element we're processing is >= currMin, then leave currMin and index unchanged

;;; GUESS INVARIANT (GI):
;;; currMin = minimum value of S so far && index = index of minimum value so far

;;; GUESS CODE:
;;; Local Variable Explanations:
;;;    first_elt: the element in S we're currently processing
;;;    rest: number representing the list containing all elements in S after first_elt

(define (myMin n)
  (define (iter n currMin index counter)
    (let ((first_elt (head n)) (rest (tail n)))
      (cond ((= n 1) index)
            ((< first_elt currMin) (iter rest first_elt counter (+ counter 1)))
            (else (iter rest currMin index (+ counter 1))))))
  (iter n (head n) 0 0))

;;; TESTS:
;;; WEAK ENOUGH? In the first call to iter, n is set to N; currMin is set to the first element in S,
;;; and both index and counter are set to 0. In this first call, the GI is true because the only
;;; element we've processed so far is the first element. The minimum of a single number is just itself
;;; so currMin and index are set to the correct values.

;;; STRONG ENOUGH? The GI states that currMin is the minimum value of S so far and index is the index of
;;; the current minimum value. The termination condition triggers when we've iterated through
;;; all the elements in S. Combining the GI with the termination condition, we've processed all the
;;; elements in S and currMin is set to the actual minimum element in S and index is set to the position
;;; of the actual minimum element. Thus, the GI and termination condition imply the post-condition.

;;; PRESERVABLE? To preserve the GI, at each iteration we compare currMin with the element we're
;;; processing. Based on the results of the comparison, we make the necessary changes to the variables
;;; as listed in the DESIGN IDEA portion of this proof.

;;; testing data
;;; 112500000 (5 2 8) => index = 1
;;;     360    (3 2 1) => index = 2
;;; 126000 (4 2 3 1) => index = 3
;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; SORT FUNCTION
;;; Specification:
;;; Pre-condition: inputs an integer n>=1 that represents a list s
;;; Post-condition: returns the number representing the list formed by sorting (smallest to largest) the
;;;                 elements of s

;;; DESIGN IDEA:
;;; The algorithm implemented to sort s is selection sort. We keep two sublists of s: unsorted and sorted. unsorted
;;; contains all the elements that have not yet been sorted (put in their proper position) and sorted contains
;;; all elements that have been processed and are in their correct sorted position. The reason for choosing selection
;;; sort over another sorting algorithm such as insertion sort is that both selection and insertion sort have
;;; quadratic time complexity and selection sort tends to perform better on small lists. Most of the lists we're
;;; working with are small so selection sort will do.

;;; GUESS INVARIANT (GI):
;;; sorted = list containing elements from s that are already sorted &&
;;; unsorted = list containing elements from s that are not yet sorted

;;; GUESS CODE:
;;; Local Variable Explanations:
;;;    min_index: the index of the smallest element in unsorted sublist

(define (sort n)
  (define (iter unsorted sorted counter)
    (let ((min_index (myMin unsorted)))
      (cond ((= unsorted 1) sorted)
            (else (iter (remove-at unsorted min_index)
                        (* sorted (expt (k-th_prime counter) (ref unsorted min_index)))
                        (+ counter 1))))))
  (iter n 1 0))

;;; TESTS:
;;; WEAK ENOUGH? In the first call to iter, unsorted is set to n, sorted is set to 1, and counter is set 0.
;;; In the first call, the GI is true because we have not yet processed any of the elements in s, so our sorted
;;; list is the empty list and unsorted is the list containing all elements in s that need to be sorted.

;;; STRONG ENOUGH? The GI states sorted is the list containing all elements in s that are already sorted and unsorted
;;; is the list containing all elements in s that still need to be sorted. The termination condition occurs when
;;; unsorted is empty, meaning there are no more elements in s left to process. Combining the GI and the termination
;;; condition, sorted will be the list containing all elements in s in their correct sorted position. Thus, the post-
;;; condition is achieved.

;;; PRESERVABLE? To preserve the GI, at each iteration find the index of the minimum element in the unsorted sublist,
;;; remove it, and add it to the sorted sublist in its correct position using the counter variable.

;;; testing data
;;; 5512500000 (5 2 8 2) => 648540112500 (2 2 5 8)
;;;     360    (3 2 1)   => 2250 (1 2 3)
;;; 126000 (4 2 3 1)  => 5402250 (1 2 3 4)
;;; --------------------------------------------------------------------------------------------------------------




;;; SET FUNCTIONS
;;; --------------------------------------------------------------------------------------------------------------
;;; To implement sets, we will still use lists but we will prohibit any duplicate elements to be in the lists


;;; --------------------------------------------------------------------------------------------------------------
;;; MYSET FUNCTION
;;; Specification:
;;; Pre-condition: inputs an integer n>=1 that represents a list s that may contain duplicate elements
;;; Post-condition: returns the number representing the list formed by removing all duplicate elements from s

;;; DESIGN IDEA:
;;; NOTE: S is the original inputed list. N is the the number representing list S.
;;; We use an iterative approach. The idea is to iterate through each element in S and append it to a new list
;;; we are building which we will call result. However, before we append an element to result, we will first check
;;; to make sure that element is not already part of result. If the check is successful, we append the element. If not,
;;; we do not append the new element and move forward to the next element in S. We will use a counter variable to step
;;; through S.

;;; GUESS INVARIANT (GI):
;;; result =  the set containing elements from S from index 0 to counter-1

;;; GUESS CODE:
;;; Local Variable Explanations:
;;;    first_elt: the element in S we're currently processing
;;;    rest: number representing the list containing all elements in S after first_elt

(define (mySet n)
  (define (iter n result counter)
    (let ((first_elt (head n)) (rest (tail n)))
      (cond ((= n 1) result)
            ((element-of? result first_elt) (iter rest result counter))
            (else (iter rest (* result (expt (k-th_prime counter) first_elt)) (+ counter 1))))))
  (iter n 1 0))

;;; TESTS:
;;; WEAK ENOUGH? In the first call to iter, result is the empty set and counter is 0. The GI is true in this first call
;;; because we have not yet iterated through any of the elements in S so our set should be empty.

;;; STRONG ENOUGH? The GI states that result is the set containing elements in S from index 0 to counter-1. The
;;; termination condition happens when there are no more elements in S to process and counter is equal to (length of S).
;;; When the termination condition triggers, the GI will be that result is the set containing elements from 0 to
;;; (length of S)-1. Anding the termination condition and GI gives us the list containing all elements in S except
;;; duplicate elements.

;;; PRESERVABLE? To preserve the GI, at each iteration before adding the element we're currently processing to result,
;;; check if it is already in result. If not, add it to result at the counter position, else move on to the next
;;; element in S.

;;; testing data
;;; 5512500000 (5 2 8 2) => 112500000 (5 2 8)
;;;     37800    (3 3 2 1)   => 360 (3 2 1)
;;; 126000 (4 2 3 1)  => 126000 (4 2 3 1)
;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; ELEMENT-OF FUNCTION
;;; Specification:
;;; Pre-condition: inputs an integer n>=1 that represents a set s and an integer p>=1
;;; Post-condition: returns #t if p is an element of s, #f otherwise

;;; DESIGN IDEA:
;;; NOTE: S is the original inputed set. N is the the number representing set S.
;;; We use an iterative approach. The idea is to iterate through each element in S using a counter variable initialized
;;; to the first index 0. Compare each element at counter with p to check if they are equal and save the outcome to a
;;; variable called result. The moment the comparison returns true, meaning result == #t, we stop searching and return.

;;; GUESS INVARIANT (GI):
;;; result =  p has been found in S between index 0 and counter-1

;;; GUESS CODE:
;;; Local Variable Explanations:
;;;    none

(define (element-of? n p)
  (define (iter n p counter result)
    (cond ((equal? result #t) result)
          ((= counter (len n)) result)
          ((= p (ref n counter)) (iter n p (+ counter 1) #t))
          (else (iter n p (+ counter 1) #f))))
  (iter n p 0 #f))

;;; TESTS:
;;; WEAK ENOUGH? In the first call to iter, result is set to #f and counter is set to 0. The GI is true in this first
;;; call because we have not yet started to check the elements of S for p. It makes sense to initialize result to #f.

;;; STRONG ENOUGH? The GI states that result is the outcome of the statement: p has been found in S between index 0
;;; and counter-1. result is #t if we've found p between 0 and counter-1, #f otherwise. There are two termination
;;; conditions. The first condition occurs when result is equal to #t. If we combine the GI with this termination
;;; condition, we have found p in S between index 0 and counter-1 so no need to keep checking and just return. Thus,
;;; our post-condition becomes true. The other termination condition occurs when counter is equal to the length of S.
;;; When this happens, it means we haven't found p in S (or else we would have returned earlier) so result has been
;;; unchanged from it's initial value #f. Combining this termination condition with the GI, we return the correct
;;; value of result (#f). Therefore, our post-condition is satisfied.

;;; PRESERVABLE? To preserve the GI, at each iteration check if the element located at counter is equal to p.
;;; If equal, change result to #t since we have found p in S and increment counter. Otherwise, just increment counter
;;; and leave result unchanged.

;;; testing data
;;; 112500000 (5 2 8) p=8 => #t
;;;     360    (3 2 1) p=11 => #f
;;; 126000 (4 2 3 1) p=1  => #t
;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; SUBSET-OF FUNCTION
;;; Specification:
;;; Pre-condition: inputs integers m>=1 and n>=1 that represent sets s and t, respectively
;;; Post-condition: returns #t if s is a subset of t, #f otherwise

;;; DIVIDE & CONQUER STRATEGY:
;;; NOTE: S and T are the original inputed sets. M and N represent S and T, respectively
;;; We'll use a recursive approach. Suppose set S has k elements and we somehow know the answer to whether the
;;; first k-1 elements of S are a subset of T or not. Then, all that's left is to check if that last element of S is
;;; an element of T. If it turns out that that last element is an element of T && all the k-1 elements form a subset
;;; of T, then we return #t, #f otherwise. Note that the empty set is a subset of all sets.

;;; GUESS CODE:
;;; Local Variable Explanations:
;;;    last_index = the index of the last element in m
;;;    last_elt = the last element in m

(define (subset-of? m n)
  (cond ((= m 1) #t)
        (else (let ((last_index (- (len m) 1)) (last_elt (last m)))
                (cond ((= (len m) 1) (element-of? n last_elt))
                      ((and (element-of? n last_elt)
                            (subset-of? (/ m (expt (k-th_prime last_index) last_elt)) n))
                       #t)
                      (else #f))))))

;;; BASIS STEP:
;;; Basis step occurs when S has just one element. In that case, to check if S is a subset of T, we just check if
;;; that one element is an element of T and return the result.

;;; INDUCTION HYPOTHESIS:
;;; We are inducting on the number of elements in S. Our induction hypothesis assumes we know the outcome of whether
;;; the first k-1 elements of S are a subset of T or not given that the precondition holds. The program changes m by
;;; removing the last element from s. It does this by dividing m by (prime at position last_index)^last_elt which is
;;; a positive integer. The resulting number is still a positive integer greater than or equal to 1 that represents a
;;; set so our precondition holds. The program does nothing to n so n is still greater than or equal to 1 and
;;; represents the set T. Thus, we can assume that the recursive call will return the correct result for the
;;; k-1 elements.

;;; INDUCTION STEP:
;;; The induction step occurs when we check if the last element is a member of T && the result of the recursive
;;; call. If both return #t, then S is a subset of T, so the program returns #t. If either one of them are #f, program
;;; returns #f; S is not a subset of T.

;;; TERMINATION ARGUMENT:
;;; The program terminates once all elements of S have been processed

;;; testing data
;;; n=112500000 (5 2 8), m=26244 (2 8) => #t
;;;     360    (3 2 1), m=8 (3) => #t
;;; 126000 (4 2 3 1) m=1350000 (4 3 5)  => #f
;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; EQUAL-SETS FUNCTION
;;; Specification:
;;; Pre-condition: inputs an integers m>=1, n>=1 that represent sets s and t, respectively
;;; Post-condition: returns #t if s and t are equal sets, #f otherwise

;;; DESIGN IDEA:
;;; The idea is to check if the two sets first have equal length. If not, then there are some elements in one set that
;;; aren't in the other set so s and t can never be equal. If, however, the two sets are of the same size, then check if
;;; one of the sets are a subset of the other. Meaning, are all the elements of s in t? If so, then they are equal.
;;; Otherwise, s contains some element that's not in t and the two sets are not equal.

;;; GUESS CODE:
;;; Local Variable Explanations:
;;;    none

(define (equal-sets? m n)
  (cond ((not (= (len m) (len n))) #f)
        (else (subset-of? m n))))

;;; testing data
;;; n=112500000 (5 2 8), m=26244 (2 8) => #f
;;;     360    (3 2 1), m=1500 (2 1 3) => #t
;;;     360    (3 2 1), m=360 (3 2 1) => #t
;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; UNION FUNCTION
;;; Specification:
;;; Pre-condition: inputs integers m>=1, n>=1 that represent sets s and t, respectively
;;; Post-condition: returns the number representing the union of s and t

;;; DESIGN IDEA:
;;; NOTE: S and T are the original inputed sets. M and N are the the numbers representing S and T, respectively.
;;; We use an iterative approach. The idea is to build a new set which will be represented by the number rsf and initially
;;; set to M. Then we will iterate through each element in T using a counter variable and append it to rsf. But, before
;;; appending it we will check if that element is already in S. If not, we append. Otherwise, move forward to the next
;;; element in T since we do not want duplicate elements.

;;; GUESS INVARIANT (GI):
;;; rsf =  the number representing the union of S and T so far from index 0 to counter-1

;;; GUESS CODE:
;;; Local Variable Explanations:
;;;    len_n = length of the set represented by n
;;;    counter_val_n = element in set T at index counter

(define (union m n)
  (define (iter m n rsf counter)
    (let ((len_n (len n)) (counter_val_n (ref n counter)))
      (cond ((= counter len_n) rsf)
            ((element-of? m counter_val_n) (iter m n rsf (+ counter 1)))
            (else (iter m n (myappend rsf (expt 2 counter_val_n)) (+ counter 1))))))
  (iter m n m 0))

;;; TESTS:
;;; WEAK ENOUGH? In the first call to iter, rsf is set to M and counter is set to 0. The GI is true in this first call
;;; because we have not yet processed any of the elements in T so the situation is similar to taking the union of a
;;; set with the empty set. 

;;; STRONG ENOUGH? The GI states that rsf is the number representing the union of S and T from index 0 to counter-1.
;;; The termination condition occurs when counter is equal to the length of T. Combining the GI with the termination
;;; condition we have that rsf represents the union of S and T from index 0 to (length of T)-1. This indicates that we
;;; have processed all the elements of T and rsf truly is the union set. Thus, our post-condition is satisfied.

;;; PRESERVABLE? To preserve the GI, at each iteration we check if the element in T at index counter is already in S.
;;; If not, we add it to the union set represented by rsf. Otherwise, do not add the element and move on to the next
;;; element in T.

;;; testing data
;;; m=112500000 (5 2 8), n=26244 (2 8) => 112500000 (5 2 8)
;;;     360    (3 2 1), n=8 (3) => 360 (3 2 1)
;;;     360    (3 2 1), n=5000 (3 4) =>   864360  (3 2 1 4)
;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; INTERSECTION FUNCTION
;;; Specification:
;;; Pre-condition: inputs integers m>=1, n>=1 that represent sets s and t, respectively
;;; Post-condition: returns the number representing the intersection of s and t

;;; DESIGN IDEA:
;;; NOTE: S and T are the original inputed sets. M and N are the the numbers representing S and T, respectively.
;;; We use an iterative approach. The idea is to first find the set with the smallest length, we'll represent that with
;;; a variable called minList. The reason we do this is the length of the intersection set can only be as large, if not
;;; smaller, than the smallest set out of the two sets we're taking the intersection of. The other set will be
;;; represented by maxList. We'll step through all the elements in minList using a counter variable and append that
;;; element to a new intersection set we're building which we will represent with the variable rsf. However, we will
;;; only append the element to the new set if that element is also a member of maxList.

;;; GUESS INVARIANT (GI):
;;; rsf =  the number representing the intersection of S and T so far from index 0 to counter-1

;;; GUESS CODE:
;;; Local Variable Explanations:
;;;    len_minList = length of the set with the smallest length
;;;    counter_val = element in set minList at index counter

(define (intersection m n)
  (define (iter maxList minList rsf counter)
    (let ((len_minList (len minList)) (counter_val (ref minList counter)))
      (cond ((= counter len_minList) rsf)
            ((element-of? maxList counter_val) (iter maxList minList (myappend rsf (expt 2 counter_val)) (+ counter 1)))
            (else (iter maxList minList rsf (+ counter 1))))))
  (let ((min_len (min (len m) (len n))))
    (cond ((= min_len (len m)) (iter n m 1 0))
          (else (iter m n 1 0)))))

;;; TESTS:
;;; WEAK ENOUGH? In the first call to iter, maxList is set to the set with the largest length and minList is set to the
;;; set with the smallest length. rsf is set to 1 (empty set) and counter is set to 0. The GI is true in this first call
;;; because we have not yet processed any of the elements in minList so the situation is similar to taking the intersection
;;; of a set with the empty set. 

;;; STRONG ENOUGH? The GI states that rsf is the number representing the intersection of S and T from index 0 to
;;; counter-1. The termination condition occurs when counter is equal to the length of minList. Combining the GI with the
;;; termination condition we have that rsf represents the intersection of S and T from index 0 to (length of minList)-1.
;;; This indicates that we have processed all the elements of minList and rsf truly represents the intersection set. Thus,
;;; our post-condition is satisfied.

;;; PRESERVABLE? To preserve the GI, at each iteration we check if the element in minList at index counter is a member of
;;; maxList. If so, we add it to the intersection set represented by rsf. Otherwise, do not add the element and move on to
;;; the next element in minList.

;;; testing data
;;; m=112500000 (5 2 8), n=26244 (2 8) => 26244 (2 8)
;;;     360    (3 2 1), n=8 (3) => 8 (3)
;;;     360    (3 2 1), n=2592 (5 4) =>  1 (empty set)
;;; --------------------------------------------------------------------------------------------------------------