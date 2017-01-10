#lang racket

;**********************************************************
; Name: Harmanpreet Singh
; Email address: harmanpreet.singh@yale.edu
; Topics: wires, gates, and circuits
;**********************************************************

(provide hours
         gate gate? gate-type gate-inputs gate-output
         ckt ckt? ckt-inputs ckt-outputs ckt-gates
         good-gate? good-circuit?
         all-wires find-gate
         ha-ckt fa-ckt
         entry entry? entry-key entry-value
         next-value
         next-config
         stable? all-stable-configs output-values init-config
         simulate
         final-config
         add-ckt
         dff-ckt
         timing-ckt)

(define hours 20)

;**********************************************************
; A wire is identified by a Racket symbol, for example 'x, 'y0, or 'next.
; Strings (eg, "x" or "next") are not permitted wire names.

; A gate is a struct with three fields: 
; 1) a symbol indicating the type of gate, one of:
;       not, and, or, xor, nand, nor
; 2) a list of the wire identifiers of the inputs
; 3) the output wire identifier

(struct gate (type inputs output) #:transparent)

; Examples of gates:

(define gate1 (gate 'and '(x y) 'z))
(define gate2 (gate 'or '(x v) 'v))
(define gate3 (gate 'not '(s) 't))
(define gate4 (gate 'nand '(x x) 'z))

; A circuit is a struct with three fields
; (1) a list of input wire identifiers
; (2) a list of output wire identifiers
; (3) a list of gates

(struct ckt (inputs outputs gates) #:transparent)

; Examples of circuits

; Here is a circuit to compare the values of its two inputs
; and output 1 if they are equal, 0 if they are unequal.
; This computes one-bit compare for equality, and implements
; the sum-of-products representation.  This is a combinational
; circuit (no cycle of wires and gates.)

(define eq1-ckt
  (ckt
   '(x y)
   '(z)
   (list
    (gate 'not '(x) 'cx)
    (gate 'not '(y) 'cy)
    (gate 'and '(x y) 't1)
    (gate 'and '(cx cy) 't2)
    (gate 'or '(t1 t2) 'z))))
 
; This is interpreted as follows:
; the inputs of the circuit are the wires x and y,
; the outputs of the circuit consist of just the wire z,
; there are five gates specified as follows:
; wire cx is the output of a NOT gate with input x,
; wire cy is the output of a NOT gate with input y,
; wire t1 is the output of an AND gate with inputs x and y,
; wire t2 is the output of an AND gate with inputs cx and cy,
; wire z is the output of an OR gate with inputs t1 and t2.

; Here is another implementation of comparing two bits for equality.
; This uses the implementation as the NOT of (x XOR y).
; This is also a combinational circuit.
; The inputs and output of this circuit are named as in eq1-ckt.

(define eq2-ckt
  (ckt
   '(x y)
   '(z)
   (list
    (gate 'xor '(x y) 'w)
    (gate 'not '(w) 'z))))

; Here is a two-bit selector whose Boolean expressions are as follows.

; z_1 = x_1 * s' + y_1 * s
; z_0 = x_0 * s' + y_0 * s

; For this circuit, z_1 and z_0 are
; equal to x_1 and x_0 if s = 0, and
; z_1 and z_0 are equal to y_1 and y_0
; if s = 1.

; This is also a combinational circuit.

(define sel-ckt
  (ckt
   '(x1 x0 y1 y0 s)
   '(z1 z0)
   (list
    (gate 'not '(s) 'sc)
    (gate 'and '(x1 sc) 'u1)
    (gate 'and '(y1 s) 'v1)
    (gate 'or '(u1 v1) 'z1)
    (gate 'and '(x0 sc) 'u0)
    (gate 'and '(y0 s) 'v0)
    (gate 'or '(u0 v0) 'z0))))

; This is a NAND latch, used to store one bit.
; It is a sequential (not combinational) circuit,
; because it has a loop from a wire to itself through
; other wires and gates.

(define latch-ckt
  (ckt
   '(x y)
   '(q u)
   (list
    (gate 'nand '(x u) 'q)
    (gate 'nand '(y q) 'u))))

; The following is also a sequential circuit, with
; an OR gate one of whose inputs is its output.
; (The "Garden of Eden" circuit.)

(define seq-or-ckt
  (ckt
   '(x)
   '(z)
   (list
    (gate 'or '(x z) 'z))))

; The next is also a sequential circuit.
; It could serve as a clock.
; Note that this circuit has *no* inputs, but
; does have an output.

(define clock-ckt
  (ckt
   '()
   '(z)
   (list
    (gate 'not '(z) 'z))))

;**********************************************************

(define wire? symbol?)

; (good-gate? value) takes an arbitrary value
; and returns #t if it is a well-formed gate,
; and #f otherwise.
; To be a well-formed gate, the value must be
; a gate struct whose three fields satisfy
; (1) the type field is one of the gate symbols:
;     'not, 'and, 'or, 'xor, 'nand, 'nor,
; (2) the inputs field is a list of wire identifiers
; (3) the output field is a single wire identifier

; In addition, the number of inputs should be correct for each gate type.
; A gate of type 'not has 1 input, while
; gates of types 'and, 'or, 'xor, 'nand, 'nor have 2 inputs.

(define (good-gate? value)
  (if (equal? #t (cond
                   [(equal? (gate-type value) 'not) (cond
                                                      [(equal? (length (flatten (list (gate-inputs value)))) 1) #t]
                                                      [else #f])]
                   [(equal? (gate-type value) 'and) (cond
                                                      [(equal? (length (gate-inputs value)) 2) #t]
                                                      [else #f])]
                   [(equal? (gate-type value) 'or)  (cond
                                                      [(equal? (length (gate-inputs value)) 2) #t]
                                                      [else #f])]
                   [(equal? (gate-type value) 'xor) (cond
                                                      [(equal? (length (gate-inputs value)) 2) #t]
                                                      [else #f])]
                   [(equal? (gate-type value) 'nand)(cond
                                                      [(equal? (length (gate-inputs value)) 2) #t]
                                                      [else #f])]
                   [(equal? (gate-type value) 'nor) (cond
                                                      [(equal? (length (gate-inputs value)) 2) #t]
                                                      [else #f])]
                   [else #f]))
      (if (list? (gate-inputs value))
          (if (wire? (gate-output value))
              #t
              #f)
          #f)
      #f))

; symbols? checks if circuit has symbols or not
(define (symbols? lst)
  (empty? (filter (lambda (x) (not (symbol? x))) lst)))

; (good-circuit? value) takes an arbitrary value and
; returns #t if value is a well-formed circuit, 
; and returns #f otherwise.

; To be a well-formed circuit, it must be a ckt struct
; and its inputs field must be a list of wires,
; its outputs field must be a list of wires, and
; its gates field must be a list of gates that
; are well-formed according to the good-gate? procedure.

; In addition, the circuit must satisfy the conditions:
; (1) no input of the circuit is the output of a gate,
; (2) every input of a gate is either 
; an input of the circuit or the output of a gate,
; (3) no wire is the output of two or more gates,
; (4) every output of the circuit is either an input
; of the circuit or the output of a gate.

; (good-circuit? sel-ckt) => #t
; (.. and similarly for eq1-ckt, eq2-ckt, latch-ckt, seq-or-ckt, clock-ckt)
; (good-circuit? 'hi) => #f
; (good-circuit? (ckt '() '() '())) => #t
; (good-circuit? (ckt '(x y) '(z) (list (gate 'and '(x y) 'x) (gate 'or '(x y) 'z)))) => #f
; (good-circuit? (ckt '(x y) '(z) (list (gate 'nor '(x y) 'z) (gate 'nand '(x y) 'z)))) => #f
; (good-circuit? (ckt '(x y) '(u z) (list (gate 'or '(x y) 'z)))) => #f

(define (good-circuit? value)
  (define (valid-inputs-outputs? value)
         (if (and (symbols? (ckt-inputs value)) (list? (ckt-inputs value)))
             (if (symbols? (ckt-outputs value))
                 #t
                 #f)
             #f))
  
  ; checks if a given wire is defined as an output in a list of gates
  (define (output? wire gates)
    (member wire (map (lambda (x) (gate-output x)) gates)))

  ; checks if a given wire is defined as an input in a list of gates
  (define (input? wire gates)
    (member wire (append* (map (lambda (x) (gate-inputs x)) gates))))

  ; checks if a given list of wires are defined as inputs in a list of gates
  (define (inputs? wires gates)
    (cond
      [(empty? wires) #t]
      [(input? (car wires) gates) (inputs? (cdr wires) gates)]
      [else #f]))
  
  ; checks if a wire exists as an input or output of a gate in a circuit
  (define (wire-exists? wire)
    (and (not (and (member wire (ckt-inputs value)) (output? wire (ckt-gates value)))) (or (member wire (ckt-inputs value)) (output? wire (ckt-gates value)))))

  ; checks if a list of wires exist
  (define (wires-exist? wires)
    (cond
      [(empty? wires) #t]
      [(wire-exists? (car wires)) (wires-exist? (cdr wires))]
      [else #f]
      ))

  ; Returns true if a list of booleans are all true
  (define (compress booleans)
    (cond
      [(empty? booleans) #t]
      [(car booleans) (compress (cdr booleans))]
      [else #f]))
  
  ; Checks the following conditions for each gate:
  ; - output of a given gate is not the output of (rest gates)
  ; - gate inputs exist
  (define (valid-gates? gates)
    (if (compress (filter boolean? (map good-gate? gates)))
        (cond
          [(empty? gates) #t]
          [(and (not (output? (gate-output (car gates)) (cdr gates))) (wires-exist? (gate-inputs (car gates)))) (valid-gates? (cdr gates))]
          [else #f])
        #f))
  
  ;check that the object is a ckt, and make sure all output wires are defined
  (if (and (and (ckt? value) (valid-gates? (ckt-gates value))) (wires-exist? (ckt-outputs value)))
      #t
      #f))
          
; extracts the outputs of all the gates in a circuit and presents them as a list
(define (extract-gate-outputs gates)
  (if (empty? gates)
      '()
      (cons (gate-output (first gates)) (extract-gate-outputs (rest gates)))))

; (all-wires circuit) to return the list of all the wire names that appear
;      in the circuit, as circuit inputs, circuit outputs, gate
;      inputs or gate outputs, in that order, with duplicates removed.
(define (all-wires circuit) 
  (remove-duplicates (append (ckt-inputs circuit) (ckt-outputs circuit) (extract-gate-outputs (ckt-gates circuit)))))

(define (find-gate-aux wire gates)
  (if (empty? gates)
      #f
      (if (equal? wire (gate-output (first gates)))
          (first gates)
          (find-gate-aux wire (rest gates)))))

; (find-gate wire circuit) to return the gate in the circuit with the given
;      output wire, or #f if there is no such gate.
(define (find-gate wire circuit)
  (find-gate-aux wire (ckt-gates circuit)))

;half adder circuit
(define ha-ckt
  (ckt
   '(x y)
   '(z co)
   (list
    (gate 'xor '(x y) 'z)
    (gate 'and '(x y) 'co))))

;full adder circuit
(define fa-ckt
  (ckt
   '(x y ci)
   '(z co)
   (list
    (gate 'xor '(x y) 't)
    (gate 'and '(s t) 'w)
    (gate 'and '(p x) 'r)
    (gate 'and '(y ci) 'q)
    (gate 'or '(y ci) 'p)
    (gate 'not '(t) 'u)
    (gate 'not '(ci) 's)
    (gate 'and '(u ci) 'v)
    (gate 'or '(w v) 'z)
    (gate 'or '(r q) 'co))))

; A configuration of a circuit is a table giving a value (0 or 1) for each wire in the circuit.  
; A table is a list of entries, each entry containing a key (the wire name) and a value (0 or 1).

(struct entry (key value) #:transparent)

(define eq1-config1
  (list
   (entry 'x 0)
   (entry 'y 1)
   (entry 'z 0)
   (entry 'cx 0)
   (entry 'cy 0)
   (entry 't1 0)
   (entry 't2 0)))

(define eq1-config2 
  (list
   (entry 'x 0)
   (entry 'y 0)
   (entry 'z 0)
   (entry 'cx 1)
   (entry 'cy 1)
   (entry 't1 0)
   (entry 't2 0)))

; Two configurations of the wires of the sel-ckt

(define sel-config1
  (list
   (entry 'x1 0)
   (entry 'x0 1)
   (entry 'y1 1)
   (entry 'y0 0)
   (entry 's 1)
   (entry 'z1 0)
   (entry 'z0 0)
   (entry 'sc 0)
   (entry 'u1 0)
   (entry 'v1 0)
   (entry 'u0 0)
   (entry 'v0 0)))

(define sel-config2
  (list
   (entry 'x1 1)
   (entry 'x0 1)
   (entry 'y1 0)
   (entry 'y0 0)
   (entry 's 0)
   (entry 'z1 0)
   (entry 'z0 0)
   (entry 'sc 1)
   (entry 'u1 0)
   (entry 'v1 0)
   (entry 'u0 0)
   (entry 'v0 0)))

; Two configurations of the wires of the latch-ckt

(define latch-config1
  (list
   (entry 'x 0)
   (entry 'y 0)
   (entry 'q 0)
   (entry 'u 0)))

(define latch-config2
  (list
   (entry 'x 0)
   (entry 'y 1)
   (entry 'q 1)
   (entry 'u 0)))

;(next-value wire circuit config) returns the value on the given wire of the given circuit,
; *after one gate delay* starting with the given configuration config of the circuit.

; Assuming that
; (1) circuit is a well-formed circuit, according to the specifications in problem 1,
; (2) the given wire is one of the wires of circuit, and
; (3) the given configuration config specifies a value for every wire in the circuit.

; If the given wire is an input wire of the circuit,
; its next value is just its value in the configuration config.

; If the given wire is the output wire of a gate,
; its next value is obtained by finding the gate of circuit for which it is the output wire, 
; looking up the values of the input wires of the gate in the configuration config,
; and applying the appropriate function of the gate to the input values.

; Note that this doesn't compute the *eventual* value (if any) of the wire, 
; just the *next* value of the wire, after one gate delay.

(define (search key table)
  (cond
    [(empty? table) #f]
    [(equal? key (entry-key (car table)))
     (entry-value (car table))]
    [else (search key (cdr table))]))

(define (config-value vars config)
      (cond
        [(empty? vars) '()]
        [else (cons (search (car vars) config) (config-value (cdr vars) config))]))

(define (next-value-aux type lst)
  (case type
    [(not) (case (car lst)
             [(1) 0]
             [(0) 1])]
    [(and) (case lst
             [((0 0)) 0]
             [((1 0)) 0]
             [((0 1)) 0]
             [((1 1)) 1])]
    [(or) (case lst
            [((0 0)) 0]
            [((1 0)) 1]
            [((0 1)) 1]
            [((1 1)) 1])]
    [(nand) (case lst
              [((0 0)) 1]
              [((1 0)) 1]
              [((0 1)) 1]
              [((1 1)) 0])]
    [(xor) (case lst
             [((0 0)) 0]
             [((1 0)) 1]
             [((0 1)) 1]
             [((1 1)) 0])]
    [(nor) (case lst
             [((0 0)) 1]
             [((1 0)) 0]
             [((0 1)) 0]
             [((1 1)) 0])]))

(define (next-value wire circuit config)
  (cond
    [(set-member? (ckt-inputs circuit) wire) (search wire config)]
    [else (next-value-aux (gate-type (find-gate wire circuit)) (config-value (gate-inputs (find-gate wire circuit)) config))]))

; (next-config circuit config) takes a circuit and a current configuration config
; and returns the "next" configuration of the circuit, after *one gate delay* has elapsed.

; In the "next" configuration of the circuit the value of each wire is the result
; of applying the next-value procedure to the wire, circuit and the configuration config.
; Note that only the values of wires in config are used for inputs, not the new values.

; Thus, values on the input wires do not change, and each wire that is the output
; of a gate has the value determined by its gate function applied the values
; of its input wires in the configuration config.

; This is a rather simplified model of the time-varying behavior of wires and gates.

(define (next-config circuit config)
  (map (lambda (n)
         (entry n (next-value n circuit config)))
       (all-wires circuit)))

; (stable? circuit config)
; returns #t if the next configuration of the circuit after the configuration config
; is the same as config, ie, this configuration is stable for the circuit.

(define (stable? circuit config)
  (if (equal? (next-config circuit config) config)
      #t
      #f))

(define (build-config keys vals)
    (if (empty? keys)
        '()
        (append (list (entry (car keys) (car vals))) (build-config (cdr keys) (cdr vals)))))

; (all-stable-configs circuit)
; returns a list of all the stable configurations of the circuit.
; The wires in the configurations should be listed in the same order as (all-wires circuit),
; and the values in the configurations list should be in increasing order, considered as
; binary numbers.

(define (all-stable-configs circuit)
  (define (all-combs n)
    (if (equal? 0 n)
        (list empty)
        (append* (map (lambda (x)
                        (list (append x '(0)) (append x '(1))))
                      (all-combs (- n 1))))))
  
  (filter (lambda (x) (stable? circuit x))
          (map (lambda (x)
                 (build-config (all-wires circuit) x))
               (all-combs (length (all-wires circuit))))))

; (output-values circuit config)
; returns a list giving the Boolean values of each of the output wires of
; the circuit in the configuration config.
; The order is the same as the list of output wires of the circuit.

(define (output-values circuit config)
  (map (lambda (x)
         (next-value x circuit config))
       (ckt-outputs circuit)))

; (init-config circuit input-values)
; takes a circuit and a list input-values of Boolean values
; which has the same length as the number of inputs of the circuit
; and returns a configuration in which the circuit input wires have the values 
; specified (in order) by the list inputs, and all other wires have the value 0.

(define (init-config-aux lst1 lst2)
  (cond
    [(empty? lst2) lst1]
    [(search (entry-key (car lst2)) lst1) (init-config-aux lst1 (cdr lst2))]
    [else (init-config-aux (append lst1 (list (car lst2))) (cdr lst2))]))

(define (init-config circuit input-values)
  (init-config-aux (build-config (ckt-inputs circuit) input-values) (map (lambda (x)
                                                                           (entry x 0))
                                                                         (all-wires circuit))))

; (simulate circuit config n) simulates the given circuit from the given configuration 
; by repeatedly calling next-config until either the configuration reached is stable,
; or next-config has been called n times, whichever occurs first.

(define (simulate circuit config n)
  (if (or (stable? circuit config) (<= n 0))
      (list config)
      (append (list config) (simulate circuit (next-config circuit config) (- n 1)))))

; (final-config circuit config) takes a circuit and a configuration config for the circuit.
; If the circuit would eventually reach a stable configuration from config, then
; (final-config circuit config) returns the stable configuration of the circuit that would be reached.
; Otherwise, (final-config circuit config) returns the symbol 'none.

(define (final-config circuit config)
  (define (simulate2 circuit config n)
    (cond
      [(<= n 0) 'none]
      [(stable? circuit config) config]
      [else (simulate2 circuit (next-config circuit config) (- n 1))]))
  (simulate2 circuit config (length (all-wires circuit))))

; 4-bit ripple-carry adder circuit -- add-ckt

; Its inputs are x3, x2, x1, x0, y3, y2, y1, y0  (in order)
; Its outputs are z4, z3, z2, z1, z0 (in order)
; What it computes is the sum of the two 4-bit binary numbers
; represented by the x's and the y's.
; For example, if the inputs are 
; x3 = 1, x2 = 0, x1 = 0, x0 = 1    (representing 9 in binary)
; y3 = 1, y2 = 1, y1 = 0, y0 = 1    (representing 13 in binary)
; then the output should be
; z4 = 1, z3 = 0, z2 = 1, z1 = 1, z0 = 0 (representing 22 in binary)

(define (create-list n max)
  (define (h s n) (string->symbol (string-append s (number->string n))))
  (if (= 0 n)
      (list
       (gate 'xor '(x0 y0) 'z0)
       (gate 'and '(x0 y0) 'c0))
      (append (create-list (- n 1) max)
              (list
               (gate 'xor (list (h "x" n) (h "y" n)) (h "s" n))
               (gate 'xor (list (h "s" n) (h "c" (- n 1))) (h "z" n))
               (gate 'and (list (h "x" n) (h "y" n)) (h "a" n))
               (gate 'and (list (h "x" n) (h "c" (- n 1))) (h "b" n))
               (gate 'and (list (h "c" (- n 1)) (h "y" n)) (h "d" n))
               (gate 'xor (list (h "a" n) (h "b" n)) (h "e" n))
               (if (= max n)
                   (gate 'xor (list (h "d" n) (h "e" n)) (h "z" (+ n 1)))
                   (gate 'xor (list (h "d" n) (h "e" n)) (h "c" n)))))))


(define add-ckt
  (ckt
   '(x3 x2 x1 x0 y3 y2 y1 y0)
   '(z4 z3 z2 z1 z0)
   (create-list 3 3)))

; D-flipflop as a circuit -- dff-ckt.
; It has inputs:  s, d (in order) and outputs q, qc (in order)

(define dff-ckt
  (ckt
   '(s d)
   '(q qc)
   (list
    (gate 'not '(qc) 'q)
    (gate 'xor '(t d) 'qc)
    (gate 'or '(s qc) 't))))

; Timing circuit that has no inputs and one output 't. When the circuit is started
; in the initial (all zero) configuration, after a few configurations,
; the output is 1 for one step, then 0 for 4 steps, then 1 for
; one step, then 0 for 4 steps, then 1 for one step, and so on.

(define timing-ckt
  (ckt
   '()
   '(t)
   (list
    (gate 'not '(t) 'w)
    (gate 'and '(y w) 'z)
    (gate 'and '(x w) 'y)
    (gate 'nor '(t w) 'x)
    (gate 'and '(z w) 't))))

; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #f)
(define error display)

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    '***OK***
			    '***X***)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))
	
(test 'hours hours (lambda (x) (> x 0)))



(test 'good-gate? (good-gate? gate1) #t)
(test 'good-gate? (good-gate? gate2) #t)
(test 'good-gate? (good-gate? gate3) #t)
(test 'good-gate? (good-gate? gate4) #t)

(test 'good-gate? (good-gate? (gate 'not 'x 'y)) #f)
(test 'good-gate? (good-gate? (gate 'nor '("x" "y") "z")) #f)
(test 'good-gate? (good-gate? (gate 'and '(1 2) 3)) #f)
(test 'good-gate? (good-gate? (gate 'equal '(x y) 'z)) #f)
(test 'good-gate? (good-gate? (gate 'or '(w x y) 'z)) #f)

(test 'good-circuit? (good-circuit? sel-ckt) #t)
(test 'good-circuit? (good-circuit? eq1-ckt) #t)
(test 'good-circuit? (good-circuit? eq2-ckt) #t)
(test 'good-circuit? (good-circuit? latch-ckt) #t)
(test 'good-circuit? (good-circuit? seq-or-ckt) #t)
(test 'good-circuit? (good-circuit? clock-ckt) #t)

(test 'good-circuit? (good-circuit? 'hi) #f)
(test 'good-circuit? (good-circuit? (ckt '() '() '())) #t)
(test 'good-circuit? (good-circuit? (ckt '(x y) '(z) (list (gate 'and '(x y) 'x) (gate 'or '(x y) 'z)))) #f)
(test 'good-circuit? (good-circuit? (ckt '(x y) '(z) (list (gate 'nor '(x y) 'z) (gate 'nand '(x y) 'z)))) #f)
(test 'good-circuit? (good-circuit? (ckt '(x y) '(u z) (list (gate 'or '(x y) 'z)))) #f)


(test 'all-wires (all-wires eq1-ckt) '(x y z cx cy t1 t2))
(test 'all-wires (all-wires sel-ckt) '(x1 x0 y1 y0 s z1 z0 sc u1 v1 u0 v0))
(test 'find-gate (find-gate 't2 eq1-ckt) (gate 'and '(cx cy) 't2))
(test 'find-gate (find-gate 'w eq2-ckt) (gate 'xor '(x y) 'w))
(test 'find-gate (find-gate 'y sel-ckt) #f)

(test 'ha-ckt (good-circuit? ha-ckt) #t)
(test 'fa-ckt (good-circuit? fa-ckt) #t)
(test 'ha-ckt (ckt-inputs ha-ckt) '(x y))
(test 'ha-ckt (ckt-outputs ha-ckt) '(z co))
(test 'fa-ckt (ckt-inputs fa-ckt) '(x y ci))
(test 'fa-ckt (ckt-outputs fa-ckt) '(z co))

(test 'ha-ckt (output-values ha-ckt 
			     (final-config ha-ckt 
					   (init-config ha-ckt '(1 1)))) 
      '(0 1))
(test 'fa-ckt (output-values fa-ckt (final-config fa-ckt (init-config fa-ckt '(1 1 1)))) '(1 1))

(test 'next-value (next-value 'cx eq1-ckt eq1-config1) 1)
(test 'next-value (next-value 't2 eq1-ckt eq1-config1) 0)
(test 'next-value (next-value 'z eq1-ckt eq1-config2) 0)
(test 'next-value (next-value 'x0 sel-ckt sel-config1) 1)
(test 'next-value (next-value 'v1 sel-ckt sel-config1) 1)
(test 'next-value (next-value 'v0 sel-ckt sel-config2) 0)


(test 'next-config (next-config eq1-ckt eq1-config1)
      (list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'cx 1) (entry 'cy 0) (entry 't1 0) (entry 't2 0)))

(test 'next-config (next-config eq1-ckt eq1-config2)
      (list (entry 'x 0) (entry 'y 0) (entry 'z 0) (entry 'cx 1) (entry 'cy 1) (entry 't1 0) (entry 't2 1)))

(test 'next-config (next-config sel-ckt sel-config1)
      (list
       (entry 'x1 0)
       (entry 'x0 1)
       (entry 'y1 1)
       (entry 'y0 0)
       (entry 's 1)
       (entry 'z1 0)
       (entry 'z0 0)
       (entry 'sc 0)
       (entry 'u1 0)
       (entry 'v1 1)
       (entry 'u0 0)
       (entry 'v0 0)))

(test 'next-config (next-config sel-ckt (next-config sel-ckt sel-config1))
      (list
       (entry 'x1 0)
       (entry 'x0 1)
       (entry 'y1 1)
       (entry 'y0 0)
       (entry 's 1)
       (entry 'z1 1)
       (entry 'z0 0)
       (entry 'sc 0)
       (entry 'u1 0)
       (entry 'v1 1)
       (entry 'u0 0)
       (entry 'v0 0)))

(test 'next-config (next-config latch-ckt latch-config1) (list (entry 'x 0) (entry 'y 0) (entry 'q 1) (entry 'u 1)))

(test 'next-config (next-config latch-ckt latch-config2) (list (entry 'x 0) (entry 'y 1) (entry 'q 1) (entry 'u 0)))


(test 'stable? (stable? eq1-ckt (list (entry 'x 0) (entry 'y 0) (entry 'z 1) (entry 'cx 1) (entry 'cy 1) (entry 't1 0) (entry 't2 1))) #t)
(test 'stable? (stable? eq1-ckt (list (entry 'x 0) (entry 'y 0) (entry 'z 0) (entry 'cx 1) (entry 'cy 0) (entry 't1 1) (entry 't2 0))) #f)

(test 'all-stable-configs (all-stable-configs eq2-ckt)
      (list
       (list (entry 'x 0) (entry 'y 0) (entry 'z 1) (entry 'w 0))
       (list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'w 1))
       (list (entry 'x 1) (entry 'y 0) (entry 'z 0) (entry 'w 1))
       (list (entry 'x 1) (entry 'y 1) (entry 'z 1) (entry 'w 0))))

(test 'all-stable-configs (all-stable-configs seq-or-ckt)
      (list (list (entry 'x 0) (entry 'z 0)) (list (entry 'x 0) (entry 'z 1)) (list (entry 'x 1) (entry 'z 1))))

(test 'output-values (output-values eq1-ckt eq1-config2) '(0))
(test 'output-values (output-values latch-ckt latch-config2) '(1 0))
(test 'init-config (init-config eq1-ckt '(1 0)) (list (entry 'x 1) (entry 'y 0) (entry 'z 0) (entry 'cx 0) (entry 'cy 0) (entry 't1 0) (entry 't2 0)))
(test 'init-config (init-config clock-ckt '()) (list (entry 'z 0)))


(test 'simulate (simulate clock-ckt (list (entry 'z 0)) 4)
      (list
       (list (entry 'z 0))
       (list (entry 'z 1))
       (list (entry 'z 0))
       (list (entry 'z 1))
       (list (entry 'z 0))))

(test 'simulate (simulate eq1-ckt eq1-config1 5)
      (list
       (list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'cx 0) (entry 'cy 0) (entry 't1 0) (entry 't2 0))
       (list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'cx 1) (entry 'cy 0) (entry 't1 0) (entry 't2 0))))

(test 'simulate (simulate sel-ckt sel-config1 5)
      (list
       (list
	(entry 'x1 0)
	(entry 'x0 1)
	(entry 'y1 1)
	(entry 'y0 0)
	(entry 's 1)
	(entry 'z1 0)
	(entry 'z0 0)
	(entry 'sc 0)
	(entry 'u1 0)
	(entry 'v1 0)
	(entry 'u0 0)
	(entry 'v0 0))
       (list
	(entry 'x1 0)
	(entry 'x0 1)
	(entry 'y1 1)
	(entry 'y0 0)
	(entry 's 1)
	(entry 'z1 0)
	(entry 'z0 0)
	(entry 'sc 0)
	(entry 'u1 0)
	(entry 'v1 1)
	(entry 'u0 0)
	(entry 'v0 0))
       (list
	(entry 'x1 0)
	(entry 'x0 1)
	(entry 'y1 1)
	(entry 'y0 0)
	(entry 's 1)
	(entry 'z1 1)
	(entry 'z0 0)
	(entry 'sc 0)
	(entry 'u1 0)
	(entry 'v1 1)
	(entry 'u0 0)
	(entry 'v0 0))))

(test 'simulate (simulate latch-ckt latch-config2 3)
      (list (list (entry 'x 0) (entry 'y 1) (entry 'q 1) (entry 'u 0))))

(test 'simulate (simulate eq2-ckt (init-config eq2-ckt '(0 1)) 5)
      (list
       (list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'w 0))
       (list (entry 'x 0) (entry 'y 1) (entry 'z 1) (entry 'w 1))
       (list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'w 1))))

(test 'final-config (final-config clock-ckt (list (entry 'z 0)))
      'none)

(test 'final-config (final-config eq1-ckt (list (entry 'x 1) (entry 'y 1) (entry 'z 0) (entry 'cx 0) (entry 'cy 0) (entry 't1 0) (entry 't2 0)))
      (list (entry 'x 1) (entry 'y 1) (entry 'z 1) (entry 'cx 0) (entry 'cy 0) (entry 't1 1) (entry 't2 0)))

(test 'final-config (final-config sel-ckt (list (entry 'x1 0) (entry 'x0 0) (entry 'y1 1) (entry 'y0 0) (entry 's 0) (entry 'z1 1) (entry 'z0 1) (entry 'sc 0) (entry 'u1 1) (entry 'v1 1) (entry 'u0 0) (entry 'v0 1)))
      (list
       (entry 'x1 0)
       (entry 'x0 0)
       (entry 'y1 1)
       (entry 'y0 0)
       (entry 's 0)
       (entry 'z1 0)
       (entry 'z0 0)
       (entry 'sc 1)
       (entry 'u1 0)
       (entry 'v1 0)
       (entry 'u0 0)
       (entry 'v0 0)))
      
(test 'final-config (final-config latch-ckt (list (entry 'x 1) (entry 'y 1) (entry 'q 0) (entry 'u 0)))
      'none)

(test 'final-config (final-config latch-ckt (list (entry 'x 1) (entry 'y 1) (entry 'q 0) (entry 'u 1)))
      (list (entry 'x 1) (entry 'y 1) (entry 'q 0) (entry 'u 1)))


(test 'add-ckt (good-circuit? add-ckt) #t)
(test 'add-ckt (ckt-inputs add-ckt) '(x3 x2 x1 x0 y3 y2 y1 y0))
(test 'add-ckt (ckt-outputs add-ckt) '(z4 z3 z2 z1 z0))
(test 'add-ckt (output-values add-ckt (final-config add-ckt (init-config add-ckt '(1 0 0 1 1 1 0 1)))) '(1 0 1 1 0))
(test 'add-ckt (output-values add-ckt (final-config add-ckt (init-config add-ckt '(0 1 1 1 0 1 1 0)))) '(0 1 1 0 1))


(test 'dff-ckt (good-circuit? dff-ckt) #t)
(test 'dff-ckt (ckt-inputs dff-ckt) '(s d))
(test 'dff-ckt (ckt-outputs dff-ckt) '(q qc))
(test 'dff-ckt (output-values dff-ckt (final-config dff-ckt (init-config dff-ckt '(1 0)))) '(0 1))
(test 'dff-ckt (output-values dff-ckt (final-config dff-ckt (init-config dff-ckt '(1 1)))) '(1 0))

 
(test 'timing-ckt (good-circuit? timing-ckt)  #t)
(test 'timing-ckt (ckt-inputs timing-ckt)  '())
(test 'timing-ckt (ckt-outputs timing-ckt)  '(t))
(test 'timing-ckt (map (lambda (config) (output-values timing-ckt config)) (simulate timing-ckt (init-config timing-ckt '()) 20))
      '((0) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1)))