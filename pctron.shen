\\ The single layer perceptron
\\
\\ Dr Antti J Ylikoski 2016-05-15
\\
\\ for the Lambda Associates
\\
\\ If the reader wishes to get a good introduction to the perceptron,
\\ the author recommends the books
\\
\\ Stuart Russell -- Peter Norvig: Artificial Intelligence,
\\ A Modern Approach, Pearson, ISBN 978-0-13-207148-2
\\ and Chapter 18.7: Artificial Neural Networks
\\
\\ and/or,
\\
\\ George F Luger: Artificial Intelligence, Pearson--Addison-Wesley,
\\ the author has the 4th edition, ISBN 0-201-64866-0
\\ and Chapter 10: Machine Learning: Connectionist
\\
\\ For a good thorough scientific presentation of the perceptron,
\\ I recommend that the reader get the book
\\
\\ Simon Haykin: NEURAL NETWORKS, A Comprehensive Foundation
\\ the author has the 2nd edition
\\ Prentice--Hall, ISBN 0-13-273350-1
\\ and that the reader delve into Chapters 1--4.
\\
\\ Usage:
\\
\\ Load the defstruct file:
\\
\\ (load "defstruct.shen")
\\
\\ Load the (for ...) macro file:
\\
\\ (load "tc_for.shen") (or corresponding)
\\
\\ load the perceptron file:
\\
\\ (load "pctron.shen")
\\
\\ In this file, there also is here a working demo for the
\\ single-layer perceptron.
\\
\\ Use of the author's ANN functions:
\\
\\ 1) load the system
\\ 2) make the neuron(s) with mk-pctron
\\ 3) make the connections of the neurons
\\ 4) run the applications functions of the user
\\
\\
\\ The training material, ie the samples are stored in a list of
\\ lists:
\\
\\ [ [ [ ...input items 1... ] [ ...output-values1... ] ] \\ Item #1
\\   [ [ ...input items 2... ] [ ...output-values2... ] ] \\ Item #2
\\   [ [ ...input items 3... ] [ ...output-values3... ] ] ]
\\ 
\\
\\ such as the below for the logical XOR function:
\\ The truth table for the exclusive OR
\\
\\ (set xor [ [ [ 0 0 ] [ 0 ] ] \\ Truth table row #1
\\            [ [ 1 0 ] [ 1 ] ] \\ Truth table row #2
\\            [ [ 0 1 ] [ 1 ] ] \\ Truth table row #3
\\            [ [ 1 1 ] [ 0 ] ] ] )
\\
\\ Note that the samples have the type
\\ (list (list (list number)))
\\


\\ ------------------------------------------------------------
\\ The definition of the neuron as a defstruct
\\ From the general purpose neuron of the author
\\ There is a bias, it is one of the ANN pctron defstruct fields
\\ The bias is invariably constant, it is not changed as the net
\\ is trained.  But the activation level weight of the bias is trained.
\\


(defstruct pctron
  (neuron-name string)    \\ Symbolic name of this neuron
  (inputs (list (list unit))) \\ the inputs,
	                   \\ lists [ [ input1 neuron-from ]
                           \\         [ input2 neuron-from ] ]
  (bias number)            \\ the bias of the neuron
  (weights (list number))  \\ the weights, a number list
  (activation-level number) \\ the computed activation
  (threshold-function (number --> number))
  (neuron-output number)    \\ the output, after the threshold function
  (outputs (list (list unit)))) \\ a list of, lists:
                          \\ [ [ <output neuron1> <input # of neuron1> ]
                          \\   [ <output neuron2> <input # of neuron2> ] ]


\\ ------------------------------------------------------------

\\ The functionality as to firing neurons
\\

\\ Calling (transfer-function N) where N is a neuron, is equivalent
\\ with firing the neuron N, ie. computing its output
\\
\\ Another paradigm in Winston: Artificial Intelligence, 3rd edition
\\ Winston uses demons, to fire ANNs, but the author considers
\\ this method better
\\



(define transfer-function
  { pctron --> number }
  N ->
    (let
      AL (activation-level N)
      _  (pctron-activation-level-> N AL)
      F  (pctron-threshold-function N)
      _  (pctron-neuron-output-> N (F (pctron-activation-level N)))
      (pctron-neuron-output N)))


\\ The activation function of the perceptron
\\
\\ NOTA BENE: the last element of the inputs list is the bias

(define activation-level
  { pctron --> number }
  N -> (activation-level-h
           (append (pctron-inputs N) [[(pctron-bias N) null-neuron]])
	   (pctron-weights N)
	   0))
	   

(define activation-level-h
  { (list (list unit)) --> (list (list unit)) --> number --> number }
  [] _ Sum -> Sum \\ All partial sums done?
  _ [] Sum -> Sum \\ All partial sums done?
  [[I NF1] | T1] [W | T2] Sum ->
    (activation-level-h
        T1 T2
        (+ Sum (* I W))))



\\ The perceptron treshold function -- the signum function (modified)

(define threshold
  { number --> number }
  Activation -> 1 where (>= Activation 0)
  Activation -> -1)


\\ ------------------------------------------------------------
\\ The items to train the perceptron with:


(datatype training-items

________________________________
(value data-set) : (list (list (list number)));

)


\\ In this file, there also is here a working demo for the
\\ single-layer perceptron.
\\
\\ Exercise #2 from George F Luger: ARTIFICIAL INTELLIGENCE, 4th
\\ Edition
\\ Pearson--Addison-Wesley, ISBN 0-201-64866-0
\\
\\ Build a perceptron net (which only has one neuron) in Shen
\\ and run it on the classification example of Section 10.2.2.
\\


(set data-set
  [ [ [ 1.0 1.0] [ 1 ] ]
    [ [ 9.4 6.4] [ -1 ] ]
    [ [ 2.5 2.1] [ 1 ] ]
    [ [ 8.0 7.7] [ -1 ] ]
    [ [ 0.5 2.2] [ 1 ] ]
    [ [ 7.9 8.4] [ -1 ] ]
    [ [ 7.0 7.0] [ -1 ] ]
    [ [ 2.8 0.8] [ 1 ] ]
    [ [ 1.2 3.0] [ 1 ] ]
    [ [ 7.8 6.1] [ -1 ] ]
  ])


\\ Create the perceptron net in question: It only consists of
\\ one ANN neuron
\\


(datatype perceptron-net

________________________________
(value pcn) : pctron;

)


(set pcn
    (mk-pctron
        "Luger textbook exercise #2, Ch. 10 perceptron"
	[[0 null-neuron] [0 null-neuron]] \\ x1, x2, the bias is separate
	1.0 \\ the bias of the neuron
	[ 0.75 0.5 -0.6 ] \\ The weights are an ordinary list
	0  \\ Dummy initialization
	(function threshold) \\ The perceptron threshold function
	0  \\ Dummy initialization
	[[null-neuron 1]] \\ Dummy initialization
	)
)



\\ ------------------------------------------------------------
\\
\\ The functionality to train perceptrons


(define train-perceptron
    { pctron --> number --> (list (list (list number))) --> pctron }
    N Iterations DataSet ->
        (let
	    _ (for (Counter 1 1 Iterations)
	          (for (Indx 1 1 (length DataSet))
		  \\ Let the weights list to be trained, to be
		  \\ W(time): The perceptron convergence rule:
                  \\ W(t) = W(t-1) + c * (d(t-1) - PercOut(t-1)) * X(t-1)
		  \\
                  \\ W(t) == the weights vector at the time t
                  \\ W(t-1) == the weights vector at the time t-1
                  \\ c      == learning parameter, 0.2
                  \\ d(t-1) == the desired output at t-1, from the DataSet
                  \\ PercOut(t-1) == the perceptron output at t-1,
                  \\             from firing the perceptron
                  \\ X(t-1) == the input vector at t-1, from the DataSet
		  \\
		  \\ See the above mentioned Luger textbook, Page 422.
		  (let
		      WW (pctron-weights N) \\ These weights are trained
		      C 0.2 \\ The parameter value in Luger
		      T (nth Indx DataSet) \\ The current training item
		      D (hd (hd (tl T))) \\ The desired output
		      X (hd T) \\ The input list at the Time t-1
		      X1 (append X [(pctron-bias N)]) \\ last input = bias
		      Inp (modify-input X1) \\ Correct for input format
		      _ (pctron-inputs-> N Inp) \\ Assign input to N
		      PO (transfer-function N) \\ Fire the perceptron
		      NewW (pctron-learn WW C D PO X1) \\ The learning algorithm
		      (pctron-weights-> N NewW)) \\ The new weights to N
		  ))
	    N) \\ Return the trained perceptron
) \\ end function



(define modify-input
    { (list number) --> (list (list unit)) }
    [] -> []
    [ Num | T ] -> [[ Num null-neuron ] | (modify-input T)])



\\ NOTA BENE: The function (pctron-learn ...) returns something that
\\ can be assigned to the weights of a neuron, ie a (list number)
\\ Inputs have state to indicate origin of input.


(define pctron-learn
    { (list number) --> number --> number --> number --> (list number)
      --> (list number) }
    Weights Parameter Desired POutput InputsLst
    -> (pctron-learn-aux Weights Parameter Desired POutput InputsLst []))


(define pctron-learn-aux
    { (list number) --> number --> number --> number --> (list number)
      --> (list number)
      --> (list number) }
    []  Parameter Desired POutput [] Acc -> Acc \\ all processed?
    [WN | T1] Parameter Desired POutput [I | IT] Acc
    ->
        (pctron-learn-aux
	    T1
	    Parameter
	    Desired
	    POutput
	    IT
	    \\ Append to the new weights list, finally return (list number)
	    (append Acc [(+ WN
	                   (* Parameter
			      (- Desired POutput)
			      I))]
			 )))



\\ ------------------------------------------------------------
\\

\\ Below are the functions to train the Luger exercise perceptron,
\\ and to display the results
\\
\\ For the exercise and the demo:
\\ Train the perceptron by calling
\\ (pctron-demo)
\\
\\ and in order to see how the newly trained perceptron works:
\\ (show-demo)
\\
\\ The result: 100% correct convergence in every training set item
\\



(datatype perceptron-demo

________________________________
(value result) : pctron;

)


(define pctron-demo
    { --> (list A) }
    ->
    (do
         (time (set result (train-perceptron (value pcn)
	                                     500
			                     (value data-set))))
	 []))


\\ ------------------------------------------------------------



(define show-demo
  { --> (list A) }
  ->
  (do (show-ann (value result) (value data-set)) []))



(define show-ann
  { pctron --> (list (list (list number))) --> (list A) }
  P DataSet ->
    (for (Indx 1 1 (length DataSet))
      (let
        I (nth Indx DataSet) \\ Get the training input
        X1 (nth 1 (hd I))
        X2 (nth 2 (hd I))
        X3 1
        In [[X1 null-neuron] [X2 null-neuron] [X3 bias]]
        _ (pctron-inputs-> P In) \\ Input vec ==> perceptron
        _ (transfer-function P) \\ Fire (trained) neuron
        _ (output "~%")
        _ (output "~%Input vector:   ~A" (pctron-inputs P))
        _ (output "~%Weights vector: ~A" (pctron-weights P))
        _ (output "~%Output:         ~A" (pctron-neuron-output P))
        _ (output "~%Desired out:    ~A" (hd (hd (tl I))))
        _ (output "~%")
        [])))


\\ ------------------------------------------------------------

