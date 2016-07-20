\\
\\ The FOR macro by Ramil Farkshatov 2015-10-14
\\
\\ The version of the FOR macro, accepted by the Shen type system
\\ by A. J. Y. 2016-05-08
\\
\\ I have used the Shen type "unit" ie. any type
\\


(define for-expand
  { symbol --> number --> number --> number --> (list unit) --> (list unit) }
  Var Start Step End Code
      -> (let F (gensym (protect A))
              Self (protect Self)
                 [let F [/. Self Var [if [<= Var End]
                                         [do [do | Code]
                                             [Self Self [+ Var Step]]]
                                         []]]
                   [F F Start]]))


(defmacro for-macro
  [for [Var Start Step End] | Code] -> (for-expand Var Start Step End Code)
  [for [Var Start End] | Code] -> (for-expand Var Start 1 End Code)
  [for [Var End] | Code] -> (for-expand Var 0 1 End Code))


\*

Usage: (for (Var Start Step End) Code...)
       (for (Var Start End) Code...) \\ Step is 1
       (for (Var End) Code...) \\ Start is 0, Step is 1

(for (X 0 2 30) (output "~A~%" X))
(for (Y 0 10) (output "~A~%" Y))
(for (Z 5) (output "~A~%" Z))

*\

