;;; Project 1
;;; Group member: Baishaki Debi, Yi Lin
;;; Email Addresses: bdebi000@citymail.cuny.edu, ylin026@citymail.cuny.edu
;;; Spring 2022

;;; --------------------------------------------------------------------------------------------------------------
;;; INTRODUCTION:

;;; All list and set functions are completed and work (to the best of our knowledge). We were somewhat able to 
;;; figure out how to implement lists of lists and were not able to figure out how to implement lists containing both 
;;; positive integers and lists. For this latter part, we don't think it can be implemented by our current method of 
;;; using product of prime numbers to implement lists. We have given a counter-example to explain our thinking. We 
;;; have not considered a way to implement lists that contain zero as an element.

;;; Most functions have been implemented using iteration except for the subset-of? function which was created using 
;;; recursion. Functions are separated by horizontal lines.

;;; --------------------------------------------------------------------------------------------------------------



;;; START OF FUNCTION DECLARATIONS AND CODE
;;; --------------------------------------------------------------------------------------------------------------
;;; a helper function determine whether a number n is prime
;;; if the number n is prime, returns #t, otherwise #f

;;; Specification
;;; pre-cond: input a integer n
;;; post-cond: returns whether n is prime, either #t or #f

;;; Note that, if a number n is not a prime, the there should be a factor a and b, that n=a*b
;;; where a <= sqrt(n) and b >= sqrt(n),
;;; then we only need to track whether there exists a number k <= sqrt(n) so that n can be excact divide by k (without remainder)
;;; if the number does not exists, then the number is a prime

;;; design idea
;;; Implement it by iterative
;;; start with a counter i from 2 to sqrt(n), once n is divisible( without remainder) by i, then it returns #f immediately
;;; if none of the number from 2 to sqrt(n) that n can be exact divided by, then the process will return #t
;;; so the termination argument would be i > sqrt(n), or find a i that n can be exact divided by

;;; GI: n can not be divisible by any number from 2 to i-1
;;; GUESS CODE:

(define (prime? n)
  (define (iter n i)
    (let ((sqrt-n (floor (sqrt n))))
      (cond ((> i sqrt-n) #t)
            ((= (remainder n i) 0) #f)
            (else (iter n (+ i 1))))))
  
  (cond ((< n 2) #f)
        (else (iter n 2))))

;;; PROOF.
;;; when the iterative process start we pass 2 to i
;;; Weak enough? when the iterative process start, i=2, this case is trivial, since i-1 = 1 < 2, then we can say our GI to be true
;;; Strong enough? when the program terminates, that is
;;;                (1) i > sqrt(n) (or i= sqrt(n)+1), and our GI: n can not be divisible by any number from 2 to i-1,
;;;                    then n can not be divisible by any number from 2 to sqrt(n), then the program returns #t, which is correct (see NOTE that)
;;;                (2) n is divisible by i, and our GI: n can not be divisible by any number from 2 to i-1, but since i <= sqrt(n) then the program returns #f (see NOTE that)
;;;                    which is also true
;;; Maintained? if the program does not terminate, that is i has not hit the sqrt(n)+1 and n is not divisible by i, and the previous call states that n is not divisible by 2 to i-1
;;;             then for i+1, our GI: n can not be divisible by any number from 2 to i
;;;             which is true.

;;; then our GI is true
;;; here is our tesing data
;;; 2 => #t
;;; 4 => #f
;;; 11 => #t
;;; 1 => #f
;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
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
;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; -- a function myequal? which inputs numbers n representing a list s and m representing a list t, and which 
;;;    checks whether s and
;;; --------------------------------------------------------------------------------------------------------------
;;; SPECIFICATION
;;; pre condition: inputs numbers n representing list s and numbers m representing lists t, n and m should be valid (can be factorize by prime numbers or 1)
;;; post condition: output whether the list n represented and the list that represented are the same

;;; the input number n represent list s and the input number m represent list t
;;; NOTE THAT this appoach won't work if lists have 0 at the tail, (since it only allows positive integer in list)
;;;           consider list (0 0) and (0 0 0 0)
;;;           the constructed number of the two lists are both 1,
;;;           we can not distinguish which list it represents since it contruct the same number. 
;;;           

;;; IDEA: s=t iff n=m (see proof)
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


;;; --------------------------------------------------------------------------------------------------------------
;;; -- a function head which inputs a number n which represents a list s and which returns the number in the
;;;    first position of s, that is, the head of s
;;; --------------------------------------------------------------------------------------------------------------
;;; Specification
;;; pre-condition: input a legal number n that represent a positive integer list
;;; post-condition: returns the first element of the list

;;; Design Idea
;;; Since the number n represent list s, which is constructed by 2^k0 + 3^k1 + 5^k2 + ...., then we only need to figure out what k0 is.
;;; the number n is constructed by the multiplication of prime number
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
;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; -- more generally, a function ref which inputs a number n representing a list s and which returns the number
;;;    in the kth position of s
;;; --------------------------------------------------------------------------------------------------------------
;;; Specification
;;; pre-condition: inputs a number n representing a list s and a number k>=0 to be index, the index k and number n should be valid (n should represent a list and k < length of list)
;;; post-condition: returns the k-th position of s

;;; DESIGN IdEA
;;; design a iterative process
;;; start with a counter i, very similar to head function, but instead of we retreive 2, we retreive the k-th prime number
;;; Since the number n represent list s, which is constructed by 2^j0 + 3^j1 + 5^j2 + ....some prime ^ jk
;;; then we only need to figure out what jk is
;;; the number n is constructed by the multiplication of prime number
;;; we only need to figure out how many factor of k-th prime that the number n has
;;; here we design a iterative process, which contains a counter i, keep counting how many k-th prime is divisible (without remainder) by the number n,
;;; and n keep reducing its value
;;; the program will terminate when the remainder of n/k-th prime is not 0, which means n is no longer divisible (without remainder) by k-th prime.

;;; GUESS INVARIANT: we can call our original (before the first call) input to be N, k-th prime to be pk, then our GI can be n*(pk)^i = N
;;; now we have our Guess Code:

(define (ref n k)
  (define (iter n k i)
    (let ((p (k-th_prime k)))
      (cond ((= (remainder n p) 0) (iter (quotient n p) k (+ i 1)))
            (else i))))
  (iter n k 0))

;;; As we can see, when the remainder of n/pk is not 0, it will returns the counter i,
;;; Poof.
;;; GI: n*pk^i = N
;;; weak enough? the initial call n to be N, and counter i to be 0, then in this case, N*pk^0 = N, which is true
;;; strong enough? the stopping condition is, as we mentioned before, when n is no longer divisible by pk without remainder, and our GI is n*pk^i = N, note that in this case,
;;;                the stopping condition and GI gives us as that our i to be the largest number that N is divisible by pk^i, which means, it returns the number of pk that construct
;;;                the number N, which represent the first element of the list
;;; preservable? since every iteritive call reduce n by pk, and add i by 1, now consider the GI: (n/pk)*pk^(i+1) = n*pk^i = N, which is stay the same, then it is maintained. 

;;; then our GI is true

;;; here is our testing data
;;; 5512500000 (5 2 8 2) 2 => 8
;;;     360    (3 2 1) 1   => 2
;;; --------------------------------------------------------------------------------------------------------------


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
;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; -- a function insert-at-head which inputs a number n representing a list s and a second number p, and which 
;;;    returns the number representing the list obtained by inserting p at the head of the list s 
;;; --------------------------------------------------------------------------------------------------------------
;;; Specification
;;; pre condition: inputs a number n representing a list s and a number p>=0 to be number to be inserted, number n should be valid (can be represent as factor of prime)
;;; post condition: return a number that represent the list of inserting p at the head of list s

;;; Design Idea
;;; the design idea is going to construct the number that represent the list of inserting p at the head of list s, we use a iterative procedure to implement this.
;;; We first use a counter i, that represent the i-th place of the newly constructed list, then we also need a result, to store the current constructed result.
;;; So, as we state in the specification, we need to construct a number that represetn the list of inserting p at the head of list s, then the initial call of the
;;; iterative process, result would be 2^p and i should be 0, since 2 is the first prime number and p should be at 0's place of new list. Then we start "copy" the list s
;;; to our newly constructed list (let's call it t), in this procedure, we called h to be i-th elements of n, p to be current i-th prime number, and np to be i+1 prime number,
;;; then the iterative will keep dividing n by p^h, and result times np^h, that is retreiving i-th place element of s and add it to the (i+1)-th place of t, and the counter adds up by 1 each time
;;; So when the program terminates, that is when n = 1, which means, all the elements in s are all retreived, the copy process finished

;;; now here is our GUESS INVARIANT:
;;; Let S to be the list that initial n represeted, Let R to be list (p)+S
;;; Let s to be the list that current n represented and t to be current result repesented,
;;; then the GI would be, t+s = R

;;; Here we have our GUESS CODE:

(define (insert-at-head n p)
  (define (iter n result i) ;;; the result is the constructed #
    (let ((h (ref n i))
          (p (k-th_prime i))
          (np (k-th_prime (+ i 1)))) ;; next prime
      (cond ((= n 1) result)
            (else (iter (/ n (expt p h)) (* result (expt np h)) (+ i 1))))))
  (iter n (expt 2 p) 0)) ;;; initial pass 2^p to result since 2 is the first prime

;;; PROOF
;;; weak enough? when the iterative procedure start, s to be the list that initial n represented which is S, the list t of result represent is (p) since result=2^p, then t+s = (p)+S = R
;;;              as defined, so this case is true;
;;; strong enough?  when the iterative process terminates, that is when n = 1, which means the s is empty, note that, for each iterative process, s retreive i-th place element to t's i+1-th place,
;;;                 therefore, in this case, when s is  empty, that means all elements in S are goes into t, which means, current t = (p)+S, then our GI: t+s = (p)+S is also true.
;;; preservable? consider in an iterative call, for the next iterative call, we retreive i-th place data of s to the i+1-th place of t, then the GI: t+s will stay unchanged since it
;;;              just move a single current first elements of s to the last elements of t, then our GI is true

;;; hence we can say that our GUESS INVARIANT is proved to be true.

;;; Here is our testing data for GUESS CODE
;;; 5512500000 (5 2 8 2) 2 => 16950244380300 (2 5 2 8 2)
;;; 360 (3 2 1) 3 => 37800 (3 3 2 1)
;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; -- a function len which inputs a number n which represents a list s and which returns the number of elements 
;;;    of s
;;; --------------------------------------------------------------------------------------------------------------
;;; SPECIFICATION
;;; pre-condition: input a number n that represents a list s, number n should be valid (can be represent as factor of prime)
;;; post-condition:  return a number that is the length of the list s that represented by n

;;; DESIGN IDEA:
;;; The design idea is using a iterative process, we first need a counter i to keep track of the current length of n that we already count,
;;; we let h to be i-th place element of list s, p to be i-th prime number, during the iterative process, the number n keep deviding by h,
;;; which means s is retreiving its i-th element and meanwhile the counter i counts up by 1, so when the process terminates,
;;; that is when n=1, s is empty, which means it retreived all the elements, and i is the length of the list S which is initial n represented.
;;; initially we pass 0 to the counter i since before the iterative process, the retreived part is none. 

;;; ----------------------------------------
;;; 0 0 ... retreived ... 0 | not retreived
;;; ----------------------------------------
;;; 0 1 .................... i ....... length of S

;;; GUESS INVARIANT:
;;; Let S to be the list that initial n represented, s to be the list that current n represented
;;; Then our GI: the counter i = length of retreived part of s
;;;
;;; GUESS CODE

(define (len n)
  (define (iter n i)
    (let ((h (ref n i))
          (p (k-th_prime i)))
      (cond ((= n 1) i)
            (else (iter (/ n (expt p h)) (+ i 1))))))
  (iter n 0))

;;; PROOF.
;;; weak enough? when the initial call of the iterative process, the retreived part of S is none, hence i=0, which is true since length of retreived part of s is going to be 0
;;; strong enough? when the iterative process terminates, that is whne n = 1, s finished retreiving, to be as a empty list (0 0 ... 0), here is the diagram
;;;                -----------------------
;;;                0 0 ... retreived ... 0
;;;                -----------------------
;;;                0 1 ................... i
;;;                                    length of S
;;;                here in this case, our GI: i = length of retreived part of s, since in this case, all the elements in S is retreived, then i is going to be the length of S,
;;;                hence our GI is true.
;;; preservable? in the middle of iterative call, i is the counter of lenght of retreived part of s, then for the next iterative call, n is dividing by p^(element of i-th place of s),
;;;              that is retreiving i-th element of s and replace it to be 0, then our GI: the counter (i+1) to be length of retreived part of s, since the retreived part adds 1 elements,
;;;              then the GI is true in this case

;;; hence our GI is true

;;; TEST
;;; Here is our testing data for GUESS CODE:
;;; 5512500000 (5 2 8 2) => 4
;;;     360    (3 2 1)   => 3
;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; -- a function snoc which inputs a number n which represents a list s and a second number q, and which returns 
;;;    the number representing the list obtained by inserting q at the end of the list s
;;; --------------------------------------------------------------------------------------------------------------
;;; SPECIFICATION
;;; pre-condition: inputs a number n that represents a list s, and a number q
;;; post-condition: returns a number that represents a list t where t=s+(q)

;;; DESIGN IDEA:
;;; Since we implements the len function and k-th prime function, we can simply construct the number that represents t by n*((len n)-th prime number)^q
;;; since initial n is representing a list s that indexing from 0 to (len n)-1, then we just need to add q to the (len n) place of s, it will gives our the list t

;;; GUESS CODE:

(define (snoc n q)
  (let ((l (len n)))
    (* n (expt (k-th_prime l) q))))

;;; TEST
;;; 5512500000 (5 2 8 2) 2 => 667012500000 (5 2 8 2 2)
;;;     360    (3 2 1) 3   => 123480 (3 2 1 3)
;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; -- a function last which inputs a number n which represents a non-empty list s and which returns the rightmost 
;;;    element of s
;;; --------------------------------------------------------------------------------------------------------------
;;; SPECIFICATION
;;; pre-condition: intputs a number n which represents a non-empty list s, number n should be valid (can be represent as factor of prime)
;;; post-condition: outputs the rightmost(last) element of s

;;; DESIGN IDEA
;;; A tentative solution is going to use the recursive process, very similar to what we did for ref function:
;;;
;;;(define (last n)
;;;  (let ((lp (k-th_prime (- (len n) 1))))
;;;    (define (rec n p)
;;;      (cond ((not (= (remainder n p) 0)) 0)
;;;            (else (+ (rec (/ n p) p) 1))))
;;;    (rec n lp)))

;;; but since we implemented the ref function and len function
;;; here is a extremely easy solution that is take the ref of n of ((len n)-1) place

;;; GUESS CODE

(define (last n)
    (ref n (- (len n) 1)))

;;; TEST
;;; here is the testing data
;;; 5512500000 (5 2 8 2) => 2
;;;     360    (3 2 1)   => 1
;;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; -- a function insert-at which inputs a number n representing a list s, a second number x, and a third number y 
;;;    and which returns the number representing the list obtained by inserting x in the yth position of s.  You 
;;;    will need preconditions to ensure that the number y makes sense as a position in s
;;; --------------------------------------------------------------------------------------------------------------
;;; SPECIFICATION
;;; pre-condition: inputs a number n representing a list s, where number n should be valid (can be represent as factor of prime), a number x > 0 to be the elements to be inserted
;;;                a number y > 0 to be the position of s to be inserted
;;; post-condition: return a number that representing the list by inserting x in y-th position of s

;;; DESIGN IDEA:
;;; we use a iterative process to implement this function, the general idea of insert is going to construct the output, 
;;; the first part of the result would be number of list before position y, then we add elements x at position y, then we continue our constructing, copying the rest of s after position
;;; y to the constructed list
;;; here is our diagram of how our list constructed

;;; --------------------------------------------------------------------
;;;  elems before position y       | x |  elems after position y
;;; --------------------------------------------------------------------
;;;  constructed      |        not yet constructed
;;; --------------------------------------------------------------------
;;; 0 1 .............. j ..........  y  .............................len(n)

;;; here is the diagram of original list:
;;; ------------------------------------------------
;;;   copied               | not yet copied
;;; ------------------------------------------------
;;; 0 1 ................... i ................. len(n)-1

;;; our design idea is pretty intuitively, we use 2 arguments i and j as 2 index pointers for original list and constructed list, a result argument to store the number representing constructed list so far, 
;;; we let p to be j-th prime, which prepared for the next elements that we want to add to the constructed list, curr to be current i-th element of list s (represented by n),
;;; so as we can see from above diagram for constructed list, when j is greater then len(n), that means the new list construction is done, then it will return the result, which is our termination arguments,
;;; and since we want to insert x at y postion, so when j hit y position, we insert x, and increase j, but we will not increase i in this case, since i is the index for not yet copied, and this case we did
;;; not copy anything from original list; for other case, that is the copy step, we copy curr to constructed list, and increase i and j by 1.

;;; GUESS INVARIANT:
;;; let final constructed list to be T, original list to be S, the current constructed list to be t
;;; then our GI is:
;;; j is the first index of not constructed part of T and i is the first index of not yet copied of S and the constructed part of T(which is t) is true

;;; Here is our GUESS CODE:

(define (insert-at n x y)
  (define (iter n result i j x y) ;;; i is index of original list, j is index of constructed list
    (let ((p (k-th_prime j))
          (curr (ref n i))
          (l (len n)))
      (cond ((> j l) result)
            ((= j y) (iter n (* result (expt p x)) i (+ j 1) x y))
            (else (iter n (* result (expt p curr)) (+ i 1) (+ j 1) x y)))))
  (iter n 1 0 0 x y))

;;; PROOF.
;;; weak enough? before the first call of the iterative process, we pass result to be 1, that is t to be empty, i and j both to be 0, which represent the index of not yet constructed and
;;;              the first index of not yet copied, in this case, since t is still empty, the not yet constructed index is 0, and since we havn't start copying, the not yet copy index is
;;;              also to be 0; and the current constructed part of t which is empty, is true, this case is trivial, the GI is true.
;;; strong enough? when the iterative process terminates, that is when j hits (len n)+1, the iterative process terminates, then in this case, the first elements of not yet constructed is
;;;                going to be len(n)+1, or we can say, the last elements of constructed part of T is going to be j-1 which is len(n), note that the len(T) should be len(n)+1 since T is
;;;                going to be elems before y + x + elems after y therefor j is still the first index of not constructed part of T; Similarly, i is still the first index of not yet copied
;;;                part of S (i-1 is the last index of copied part), and since when the process terminates, t would be elems before y + x + elems after y, then in this case, t=T, hence t
;;;                is correct, therefore, our GI will be correct.
;;; preservable? During an iterative process called, if the j is not y, then it will increase the i and j, and copy the current element of i-th position of S to the last elements of t,
;;;;             Hence our GI is true since its simply copy and construct new list and increase both index, j+1 will be the first index of new not constructed part of T and i+1 will be the
;;;              first index of not copied part of S, and t will add one copied element and stay true.
;;;              If j=y, then it will add x to the t and only increase j, now j+1 will be the first index of new not constructed part of T since we add an element x, and i stays the same to
;;;              be the the first index of not yet copied part of S since we did not copy anything from S this case, and t stays true, since we add x to y position of t.
;;;              Hence the GI in this case is true.

;;; Therefore, our GUESS INVARIANT is true
;;;
;;; TEST
;;; Here is our testing data

;;; 5512500000 (5 2 8 2) 7 8 => 15694670722500000 (5 2 7 8 2)
;;;     360    (3 2 1) 6 2   => 7875000 (3 2 6 1)
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


;;; --------------------------------------------------------------------------------------------------------------
;;; Can this method be used to represent lists of lists of positive integers, such as ((1) (2 3) (3 1 5))?
;;; If so, show with a developed scheme program, how you would do it.  If not, explain why, in detail.

;;; Yes, I think this approach can be used to represent lists of lists of positive integers
;;;
;;; its very similar to the lists of positive integers, but instead, the power part of each prime number should also be a number that representing a list
;;; for example ((1) (2 3) (3 1 5))
;;; can be represented as (2^(2^1) * 3^(2^2*3^3) * 5^(2^3*3^1*5^5))
;;; then the approach is pretty similar to previous one,
;;; if we want to develop the scheme program, we could simply use the same program that we developed to implement this, but we just need to slightly change our pre-condition and post-condition

;;; the pre-condition would be, input a number n that n could be factorize as multiplication of primes as p0^k0*p1^k1*...*pi^ki, where k0, k1, ... ki can all be factorize as
;;; multiplication of primes as p0^j0*p1^j1*...*pi^ji
;;; the post-condition would be, returns a numbers that representing a list/list of lists (depends on what function)

;;; for example
;;; ref function
;;; pre-condition: input a number n>1 that represents list of lists, that n could be factorize as multiplication of primes as p0^k0*p1^k1*...*pi^ki, where k0, k1, ... ki can all be factorize as
;;;                multiplication of primes as p0^j0*p1^j1*...*pi^ji
;;; post-condition: returns a numbers > 0 that representing a list, that the number could be factorize as multiplication of primes as p0^k0*p1^k1*...*pi^ki

;;; the implementation would be exactly the same as the ref
(define (ref2 n k)
  (define (iter n k i)
    (let ((p (k-th_prime k)))
      (cond ((= (remainder n p) 0) (iter (quotient n p) k (+ i 1)))
            (else i))))
  (iter n k 0))

;;; test
;;; (ref2 (* (expt 2 (expt 2 0)) (expt 3 (* (expt 2 2) (expt 3 3))) (expt 5 (* (expt 2 3) (expt 3 1) (expt 5 5)))) 2) => 75000
;;; since 2^3*3^1*5^5 = 75000, which is correct
;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; Can this method be used to represent lists which contain both positive integers and lists of positive integers?
;;; If so, explain briefly how you would do it. If not, explain why, in detail.

;;; It is not possible to use this method to represent lists that contain both positive integers and lists of
;;; positive integers because currently we have no way to distinguish when a list element is just a number and when
;;; it is a list. Let's examine the list (4 (2)). Representing this list using our current method yields:
;;; 2^4 * 3^(2^2) = 2^4 * 3^4. Here's where we run into a problem. The prime representation of (4, (2)) is exactly the
;;; same as the list (4 4) but these two lists are clearly not the same!

;;; Our prime representation strips away the information that a positive integer such as 4 is just an integer and not
;;; the list (2) and vice versa. Thus, we've come across a counter-example that disproves the claim that our current
;;; method of representing lists can represent lists with both positive integers and lists of positive integers.
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
;;; m=26244 (2 8), n=112500000 (5 2 8) => #t
;;; m=8 (3), n=360 (3 2 1)) => #t
;;; m=1350000 (4 3 5), n=126000 (4 2 3 1)   => #f
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

