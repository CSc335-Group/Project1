;;; Project 1
;;; Group member: Baishaki Debi, Yi Lin
;;; Spring 2022

;;; draft, with/without complete proof
;;; some helper function

;;; for future use (maybe?)
(define (count n)
  (+ 1 (floor (log n 10))))

;;; ---------------------------------------------------------------------------------------------------------------------
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

;;;

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
;;; then we can state that our code is true.

;;; -----------------------------------------------------------------------------------------------------------
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

;;; ---------------------------------------------------------------------------------------------------------
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
;;; 6251175 (0 4 2 3 1) => 0
;;; as we can see, the program is true. 



;;; --------------------------------------------------------------------------------------------------------------
;;; -- more generally, a function ref which inputs a number n representing a list s and which returns the number
;;;    in the kth position of s
;;; --------------------------------------------------------------------------------------------------------------
;;; Specification
;;; pre-condition: inputs a number n representing a list s and a number k>=0 to be index, the index k and number n should be valid
;;; post-condition: returns the k-th position of s

;;; Design Idea
;;; design a iterative process
;;; start with a counter i, very similar to head function, but instead of we retreive 2, we retreive the k-th prime number
;;; Since the number n represent list s, which is constructed by 2^j0 + 3^j1 + 5^j2 + ....some prime ^ jk
;;; then we only need to figure out what jk is
;;; the number n is constructed by the multiplication of prime number
;;; we only need to figure out how many factor of k-th prime that the number n has
;;; here we design a iterative process, which contains a counter i, keep counting how many k-th prime is divisible (without remainder) by the number n,
;;; and n keep reducing its value
;;; the program will terminate when the remainder of n/k-th prime is not 0, which means n is no longer divisible (without remainder) by k-th prime.

;;; we can call our original (before the first call) input to be N, k-th prime to be pk, then our GI can be n*(pk)^i = N
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
;;; maintained? since every iteritive call reduce n by pk, and add i by 1, now consider the GI: (n/pk)*pk^(i+1) = n*pk^i = N, which is stay the same, then it is maintained. 

;;; then our GI is true


;;; here is our testing data
;;; 5512500000 (5 2 8 2) 2 => 8
;;;     360    (3 2 1) 1   => 2
;;; 7640325 (0 4 2 3 1) 3  => 3
;;; then we can state that our Code is true






;;; --------------------------------------------
;;; use your tail function

;;; --------------------------------------------



;;; ---------------------------------------------------------------------------------------------------------------------------------
;;; -- a function insert-at-head which inputs a number n representing a list s and a second number p, and which returns the number
;;;    representing the list obtained by inserting p at the head of the list s 
;;; ---------------------------------------------------------------------------------------------------------------------------------
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
;;; preservable? consider a single iterative call, for the next iterative call, we retreive i-th place data of s to the i+1-th place of t, then the GI: t+s will stay unchanged since it
;;;              just move a single current first elements of s to the last elements of t, then our GI is true

;;; hence we can say that our GUESS INVARIANT is proved to be true.

;;; Here is our testing data for GUESS CODE
;;; 5512500000 (5 2 8 2) 2 => 16950244380300
;;; 360 (3 2 1) 3 => 37800 (3 3 2 1)
;;; 7640325 (0 4 2 3 1) 8  => 135655520000 (8 0 4 2 3 1)

;;; We can state that OUR GUESS CODE is true. 


;;; ------------------------------------------------------------------------------------------------------------------
;;; -- a function len which inputs a number n which represents a list s and which returns the number of elements of s
;;; ------------------------------------------------------------------------------------------------------------------

;;; SPECIFICATION
;;; pre-condition: input a number n that represents a list s, number n should be valid (can be represent as factor of prime)
;;; post-condition:  return the length of the list s that represented by n

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
;;; 7640325 (0 4 2 3 1)  => 5
;;; We can state that OUR GUESS CODE is true. 




;;; -------------------------------------------------------------------------------------------------------------------------
;;; -- a function snoc which inputs a number n which represents a list s and a second number q, and which returns the number
;;;    representing the list obtained by inserting q at the end of the list s
;;; -------------------------------------------------------------------------------------------------------------------------

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
;;; 7640325 (0 4 2 3 1) 8  => 6232447820924325 (0 4 2 3 1 8)



;;; ---------------------------------------------------------------------------------------------------------------------------
;;; -- a function last which inputs a number n which represents a non-empty list s and which returns the rightmost element of s
;;; ---------------------------------------------------------------------------------------------------------------------------

;;; SPECIFICATION
;;; pre-condition: intputs a number n which represents a non-empty list s, number n should be valid (can be represent as factor of prime)
;;; post-condition: outputs the rightmost(last) element of s

;;; DESIGN IDEA
;;; A tentative solution is going to use the recursive process, very similar to what we did for ref function:
;;;(define (last n)
;;;  (let ((lp (k-th_prime (- (len n) 1))))
;;;    (define (rec n p)
;;;      (cond ((not (= (remainder n p) 0)) 0)
;;;            (else (+ (rec (/ n p) p) 1))))
;;;    (rec n lp)))

;;; but since we implemented the ref function and len function
;;; here is a extremely easy solution that is take the ref of n of ((len n)-1) place

;;; GUESS CODE
(define (last2 n)
    (ref n (- (len n) 1)))

;;; TEST
;;; here is the testing data
;;; 5512500000 (5 2 8 2) => 2
;;;     360    (3 2 1)   => 1
;;; 7640325 (0 4 2 3 1)  => 1





;;; --------------------------------------------------------------------------------------------------------------------------------
;;; -- a function insert-at which inputs a number n representing a list s, a second number x, and a third number y and which returns
;;;    the number representing the list obtained by inserting x in the yth position of s.  You will need preconditions
;;;    to ensure that the number y makes sense as a position in s
;;; --------------------------------------------------------------------------------------------------------------------------------

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
;;; 7640325 (0 4 2 3 1) 9 3  => 1413932885390025 (0 4 2 9 3 1)

;;; After test, we can state that our GUESS CODE is TRUE

;;; ((2) 4)
;;; 3^4 = 3^2^2



;; Can this method be used to represent lists of lists of positive integers, such as ((1) (2 3) (3 1 5))?
;; If so, show with a developed scheme program, how you would do it.  If not, explain why, in detail.

;;; Yes, I think this approach can be used to represent lists of lists of positive integers
;;;
;;; its very similar to the lists of positive integers, but instead, the power part of each prime number should also be a number that representing a list
;;; for example ((1) (2 3) (3 1 5))
;;; can be represented as (2^(2^1) * 3^(2^2*3^3) * 5^(2^3*3^1*5^5))
;;; tehn the approach is pretty similar to privious one,
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

;;; (* (expt 2 2) (expt 3 36) (expt 5 75000))

