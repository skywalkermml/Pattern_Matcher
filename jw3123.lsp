; Author : Joshua Wang. 
; Date 10/01/2013
; code reference:
; (1) main part: notes on lecture website. URL: https://www.cs.columbia.edu/~jvoris/AI/notes/assignment_1/assignment_1.htm
; (2) is(somefeature)() functions: URL: http://wsh4346.googlecode.com/svn/trunk/AI/proj1/trunk/sw2583.lisp
; (3) Syntaxes : (a) slides on lecture:
;                (b) The Common Lisp Cookbook. URL: http://cl-cookbook.sourceforge.net/index.html


(defun isv (x)   ; check whether x is a varible.code reference: http://wsh4346.googlecode.com/svn/trunk/AI/proj1/trunk/sw2583.lisp
  (    
  if (symbolp x)
    (if (equal (subseq (string x) 0 1) "?" )
      (if (not( equal (subseq (string x) 1 2) "") ) 
        t nil
      )  nil      
    )  nil
    
  
  )
)

(defun isex (x)   ; check whether x has ! in front reference: http://wsh4346.googlecode.com/svn/trunk/AI/proj1/trunk/sw2583.lisp
  (    
  if (symbolp x)
    (if (equal (subseq (string x) 0 1) "!" )
      (if (not( equal (subseq (string x) 1 2) "") ) 
        t nil
      )  nil      
    )  nil
    
  
  )
)

(defun isgreater (x)   ;  check whether x has > .code reference: http://wsh4346.googlecode.com/svn/trunk/AI/proj1/trunk/sw2583.lisp
  (    
  if (symbolp x)
    (if (equal (subseq (string x) 0 1) ">" )
      (if (not( equal (subseq (string x) 1 ) "") ) 
        t nil
      )  nil      
    )  nil
    
  
  )
)

(defun issmaller (x)   ; code  if x has < .code reference: http://wsh4346.googlecode.com/svn/trunk/AI/proj1/trunk/sw2583.lisp
  (    
  if (symbolp x)
    (if (equal (subseq (string x) 0 1) "<" )
      (if (not( equal (subseq (string x) 1 2) "") ) 
        t nil
      )  nil      
    )  nil
    
  
  )
)



(defun subpm(p d a)  ; subfunction of "match", return association list a

(Cond 

   
 ( (and (null p) (null d))  ( cond ((not (equal a (list '() ))) a)   (t  '(t) )) ) ; case 1: at the end of successful matching, 
                                                                                   ;return association list or t

  
 ( (and ( equal '* (first p) ) (null d) ) ( subpm (cdr p) d a)  )  ; case 2 : before case 3,
                                                                   ; there is a special condition that p is * d is null. Just pass *

 ( (or (null p) (null d))   nil ) ; case 3: after case 1. one of them finish, the othe not,  return failure


 ( (equal (car p) '?) (subpm (rest p) (rest d) a ))  ;case 4: pattern is ?, do the rest of matching.  

 
   ( (isv (car p)) ; case 5 we've encountered a pattern variable 
       
      ( 
       if (not ( null (matchvar(car p) (car d) a)))  ; case 5.1 if car p match car d, do the rest of matching, 
                                                      ;with new association list. 
         (
          subpm (cdr p) (cdr d) (matchvar(car p) (car d) a) 
         )
         nil                                ;case 5.2 else retun nil
      ) 
    )     
      
   
    (  (eq '* (first p) )  ;case 6 kleene star
         (  
         append  (subpm (cdr p) d a)   (subpm  p (cdr d) a) ; append two possible association lists, which are advanced p and d.  
         )  
    )  
  
     
    (   (isex (first p))  ; case 7 excalmation: update the a list, if it's nil, return n, else continue cdr p cdr d with new a
      ( 
        if (not ( null (exclude (intern(string-upcase (substitute #\? #\! (string (car p)) :end 1 )))  (car d) a ))) 
         (                                                                            
          subpm (cdr p) (cdr d) (exclude (intern(string-upcase (substitute #\? #\! (string (car p)) :end 1))) (car d) a)
         )     ; input subsituted car p so that matching a list and data will be easier.
         nil
      )
       
    )

       ( (isgreater (car p)) ; case 8 greater similar as case 7 except ! is replaced by > 
                              ;and we have to check wheter the data is a number 
        ( 
          if (numberp (car d))
           (
             if (not ( null (intern(string-upcase (substitute #\? #\> (string (car p)) :end 1 )))))
             (
             subpm (cdr p) (cdr d) (greater (intern(string-upcase (substitute #\? #\> (string (car p)) :end 1 ))) (car d) a)
             )
             nil
           )
          nil
         ) 
       )       

  
     ( (issmaller (car p)) ; case 9 smaller, similar as case 8 except > is replaced by <
       
        ( 
          if (numberp (car d))
          (
            if (not ( null(intern(string-upcase (substitute #\? #\< (string (car p)) :end 1 ))) ))
            (
             subpm (cdr p) (cdr d) (smaller (intern(string-upcase (substitute #\? #\< (string (car p)) :end 1 ))) (car d) a)
            )
            nil
          )
         nil
         ) 
       )       


   ( (listp (car p))              ;case 10 encounter a list: it might be a constraint case (with & at head 
                                  ;,several variables correspond to a same datum) 
                                  ;or sub-list case (does not have &, variables correspond to differnet data )
    ( let ((su (car p) ))
      ( if (equal '& (car su))    ; constraint case 
         (  
           subpm (cdr p) (cdr d) ( bindhandler (cdr su) (car d) a)  ;  bindhandler check whether the conditions
                                                                    ;  in the list is satidfied. and update a
         ) 
         (
          if (listp (car d))                    ;else in the case of sublist
          (subpm (cdr p) (cdr d) ( subpm su  (car d) a)) ; recusively call the sublist, input the new a to the next resursion
           nil
         )
      )
    )
   ) 
   
   
     (t  ; case 11 default "assert" we've encounter a symbol : add other condition in front of it, or the p will be seen as a symbol 
        (
        if (equal (car p)(car d) ) (subpm (rest p)(rest d) a) NIL
        )

     ) 



 )) ; end cond and function subpm 


(defun matchvar(p d a)  ;matching in every association list.

(if (equal a (list()) ) ; inital, empty a list is (())
(list (cons  (list p d) (car a)))  ;add the match to the sublist

(  ; else
   let ((cara (car a)) (cdra (cdr a)))   ;cara is the assocition list a under inspection
     
    (cond 
     ( ( not (null cara)) 
         (cond 
           (  (not ( null (assoc p cara)) )  ;  
               
            
               (cond  
                ((equal d (second (assoc p cara))) ;   
                (cons cara (matchvar p d cdra ) )) ; case: comply previou match 
                (t (matchvar p d cdra ) )  ;case:  does not comply previous  match , delete this a then continue.
                ) 
             
            )  
          
           (t (cons (cons  (list p d) cara) (matchvar p d cdra )))      ; case new match, add to a
           
         )  
      )
    )
)))  ; we dont so anything when a is NIL. So that once a is NIL, it retain so in the end andthe result is  NIL




(defun exclude(np d a)  ; converse to  matchvar()

(
   let ((cara (car a)) (cdra (cdr a)) ) 
     
    (cond 
     ( ( not (null cara)) 
         (cond 
           ( ( null (assoc np cara) )  ; 
                (cons cara (exclude np d cdra )) )  ; case: datum do not match, retain this a and continue
           ( (equal (second (assoc np cara)) d )  
                (exclude np d cdra )  ; datum match, exclude the list and continue
               )                                 
              (t (cons cara (exclude np d cdra )) )   ; else do not change the list
            )
           )  
          
            
           
         ) ; end outer condition  
      ) ;end let
    ) ; end function


(defun greater(np d a)     ; similar as function  matchvar(p d a). except the condition of equaling the data is replaced by >  

(
   let ((cara (car a)) (cdra (cdr a)) ) 
     
    (cond 
     ( ( not (null cara)) 
         (cond 
           
           ( ( null (assoc np cara) )  ; no matching data
                (greater np d cdra)     ) ; encounter unseen variable, return nil and continue.  
           ( (> d (second (assoc np cara)) ) 
                (cons cara (greater np d cdra ))  ; do not change the list and continue
           )   
                                       
            (t (greater np d cdra ) )   ; else exclude the list and continue.
            )
           )  
          
            
           
         ) ; end outer condition  
      ) ;let
    ) ; function


(defun smaller(np d a) ;similar as function greater except > is replaced by <

(
   let ((cara (car a)) (cdra (cdr a)) ) 
     
    (cond 
     ( ( not (null cara)) 
         (cond 
             
           ( ( null (assoc np cara) )   
                (smaller np d cdra)     )  ; encounter unseen variable, return nil and continue.  
           ( (< d (second (assoc np cara)) ) 
                (cons cara (smaller np d cdra ))  ; do not change the list and ocntinue
               )                                 
              (t (smaller np d cdra ) )   ; else exclude
            )
           )  
          
            
           
         ) ; end outer condition  
      ) ;let
    ) ; function



(defun bindhandler (p d a)  ;handling  the list of the form (&  )
  
  (if (null p) a  ;base case

  (cond  ;else
   ((isv (car p))  (bindhandler (cdr p) d  (matchvar (car p) d a)  )  ) ;if its "?x", update a as matchvar and continue

    ((isex (car p) )  
       (bindhandler (cdr p) d  (exclude (intern(string-upcase (substitute #\? #\! (string (car p)) :end 1)))  d a) ))
                                                                       ; !x, same rules as exclue, then continue.
    ((isgreater (car p) ) 
        (bindhandler (cdr p) d  (greater (intern(string-upcase (substitute #\? #\> (string (car p)) :end 1)))  d a)  )  )
                                                                       ; >x , same rule as greater. update and pass a 
    ((issmaller (car p)) 
        (bindhandler (cdr p) d  (smaller (intern(string-upcase (substitute #\? #\< (string (car p)) :end 1)))  d a)  )  )
                                                                         ;"<x" same rules as smaller, update a pass a 
     (t nil) ; else retun nil
   )

   )
)

(defun match (p d)   ; thr main function to be called
  (let ((result (subpm p d (list NIL))))  ; see what subpm returns
    (cond 
     ((null result)  nil) ; get nil, retun nil  
     ((hast result) t ) ; get a t in any solution, return t
     ((null (second result)) (car result) ) ; get only one solution, return that solution (remove tha outer list)
     (t result)  ; else return the result of multiple solution 
    )
  )
)



(defun hast (a) ; to check whether the multiple solutions a have one solition t
(if (null a)
  nil
  
  (if (equal t (car a)) 
    t
  (hast (cdr a))
  )
)

)

