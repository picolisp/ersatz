# 27aug25 Software Lab. Alexander Burger

(setq
   *CPU "JVM"
   *OS (java (java "java.lang.System" "getProperty" "os.name")) )

############ lib.l ############

(de task (Key . Prg)
   (nond
      (Prg (del (assoc Key *Run) '*Run))
      ((num? Key) (quit "Bad Key" Key))
      ((assoc Key *Run)
         (push '*Run
            (conc
               (make
                  (when (lt0 (link Key))
                     (link (+ (eval (++ Prg) 1))) ) )
               (ifn (sym? (car Prg))
                  Prg
                  (cons
                     (cons 'job
                        (cons
                           (lit
                              (make
                                 (while (atom (car Prg))
                                    (link
                                       (cons (++ Prg) (eval (++ Prg) 1)) ) ) ) )
                           Prg ) ) ) ) ) ) )
      (NIL (quit "Key conflict" Key)) ) )

(de timeout (N)
   (if2 N (assoc -1 *Run)
      (set (cdr @) (+ N))
      (push '*Run (list -1 (+ N) '(bye)))
      (del @ '*Run) ) )

(de macro "Prg"
   (run (fill "Prg")) )

(de recur recurse
   (run (cdr recurse)) )

(de curry "Z"
   (let ("X" (++ "Z")  "Y" (++ "Z")  "P" (filter pat? "X"))
      (if2 "P" (diff "X" "P")
         (list "Y" (cons 'job (lit (env @)) (fill "Z" "P")))
         (cons "Y" (fill "Z" "P"))
         (list "Y" (cons 'job (lit (env @)) "Z"))
         (cons "Y" "Z") ) ) )

### Definitions ###
(de expr ("F")
   (set "F"
      (list '@ (list 'pass (box (getd "F")))) ) )

(de subr ("F")
   (set "F" (getd (cadadr (getd "F")))) )

(de undef ("X" "C")
   (when (pair "X")
      (setq  "C" (cdr "X")  "X" (car "X")) )
   (ifn "C"
      (prog1 (val "X") (set "X"))
      (prog1
         (cdr (asoq "X" (val "C")))
         (set "C"
            (delq (asoq "X" (val "C")) (val "C")) ) ) ) )

(de redef "Lst"
   (let ("Old" (car "Lst")  "New" (name "Old"))
      (set
         "New" (getd "Old")
         "Old" "New"
         "Old" (fill (cdr "Lst") "Old") )
      "New" ) )

(de daemon ("X" . Prg)
   (prog1
      (nond
         ((pair "X")
            (or (pair (getd "X")) (expr "X")) )
         ((pair (cdr "X"))
            (method (car "X") (cdr "X")) )
         (NIL
            (method (car "X") (get (or (cddr "X") *Class) (cadr "X"))) ) )
      (con @ (append Prg (cdr @))) ) )

(de patch ("Lst" "Pat" . "Prg")
   (bind (fish pat? "Pat")
      (recur ("Lst")
         (loop
            (cond
               ((match "Pat" (car "Lst"))
                  (set "Lst" (run "Prg")) )
               ((pair (car "Lst"))
                  (recurse @) ) )
            (NIL (cdr "Lst"))
            (T (atom (cdr "Lst"))
               (when (match "Pat" (cdr "Lst"))
                  (con "Lst" (run "Prg")) ) )
            (setq "Lst" (cdr "Lst")) ) ) ) )

(de cache ("Var" "X" . "Prg")
   (setq "X" (cons (char (hash "X")) "X"))
   (nond
      ("Prg" (caar (idx "Var" "X")))
      ((setq "Var" (caar (idx "Var" "X" T)))
         (set (car "X") (run "Prg" 1)) )
      ((n== "Var" (val "Var"))
         (set "Var" (run "Prg" 1)) )
      (NIL (val "Var")) ) )

### I/O ###
(de tab (Lst . @)
   (for N Lst
      (let V (next)
         (and (gt0 N) (space (- N (length V))))
         (prin V)
         (and (lt0 N) (args) (space (- 0 N (length V)))) ) )
   (prinl) )

(de beep ()
   (prin "^G")
   (flush)
   NIL )

(de msg (X . @)
   (out 2
      (print X)
      (pass prinl) )
   X )

(de script (File . @)
   (load File) )

(de once "Prg"
   (unless (idx '*Once (file) 0)
      (run "Prg" 1) ) )

(de finish Prg
   (apply push1 (reverse Prg) '*Bye) )

(de pil @
   (when (== "Pil" '"Pil")
      (call "mkdir" "-p" (setq "Pil" `(pack (sys "HOME") "/.pil/"))) )
   (pass pack "Pil") )

# Temporary Files
(de tmp @
   (unless *Tmp
      (finish (call "rm" "-r" *Tmp))
      (call "mkdir" "-p"
         (setq *Tmp (pack (path "~/.pil/tmp/") *Pid "/")) ) )
   (pass pack *Tmp) )

### String ###
(de pad (N Val)
   (pack (need N (chop Val) "0")) )

(de align (X . @)
   (pack
      (if (pair X)
         (mapcar
            '((X) (need X (chop (next)) " "))
            X )
         (need X (chop (next)) " ") ) ) )

(de center (X . @)
   (pack
      (if (pair X)
         (let R 0
            (mapcar
               '((X)
                  (let (S (chop (next))  N (>> 1 (+ X (length S))))
                     (prog1
                        (need (+ N R) S " ")
                        (setq R (- X N)) ) ) )
               X ) )
         (let S (chop (next))
            (need (>> 1 (+ X (length S))) S " ") ) ) ) )

### List ###
(de less (X N L)
   (default N 4)
   (cond
      ((atom X) X)
      ((=0 N) '(..))
      ((asoq X L) (cdr @))
      (T
         (prog1
            (cons (less (car X) N))
            (conc @
               (less
                  (cdr X)
                  (dec N)
                  (cons (cons X @) L) ) ) ) ) ) )

(de insert (N Lst X)
   (conc
      (cut (dec N) 'Lst)
      (cons X)
      Lst ) )

(de remove (N Lst)
   (conc
      (cut (dec N) 'Lst)
      (cdr Lst) ) )

(de place (N Lst X)
   (conc
      (cut (dec N) 'Lst)
      (cons X)
      (cdr Lst) ) )

(de group (Lst)
   (make
      (for X Lst
         (if (assoc (car X) (made))
            (conc @ (cons (cdr X)))
            (link (list (car X) (cdr X))) ) ) ) )

(de uniq (Lst)
   (let R NIL
      (filter
         '((X)
            (not (idx 'R (cons (hash X) X) T)) )
         Lst ) ) )

### Symbol ###
(de loc (S X)
   (if (and (str? X) (= S X))
      X
      (and
         (pair X)
         (or
            (loc S (car X))
            (loc S (cdr X)) ) ) ) )

(de -symbols ()
   (apply symbols (symbols) (intern (opt))) )

(de private ()
   (symbols '(priv) (read)) )

(de local ()
   (symbols (list (car (symbols)))
      (read) ) )

(de import Lst
   (for Sym Lst
      (unless (== Sym (intern Sym T))
         (quit "Import conflict" Sym) ) )
   Lst )

(de export Lst
   (for Sym Lst
      (unless (== Sym (intern Sym (cadr (symbols))))
         (quit "Export conflict" Sym) ) )
   Lst )

### Date/Time ###
(de *mon . (jan feb mar apr may jun jul aug sep oct nov dec .))

(de dat$ (Dat C)
   (when (date Dat)
      (pack (car @) C (pad 2 (cadr @)) C (pad 2 (caddr @))) ) )

(de tim$ (Tim F)
   (when (setq Tim (time Tim))
      (pack (pad 2 (car Tim)) ":" (pad 2 (cadr Tim))
         (and F ":")
         (and F (pad 2 (caddr Tim))) ) ) )

(de datSym (Dat)
   (when (date Dat)
      (pack
         (pad 2 (caddr @))
         (get *mon (cadr @))
         (pad 2 (% (car @) 100)) ) ) )

### OOP ###
(de class Lst
   (set (setq *Class (++ Lst)) (type *Class))
   (putl *Class)
   (def *Class Lst) )

(de object ("Sym" "Val" . @)
   (putl "Sym")
   (def "Sym" "Val")
   (while (args)
      (put "Sym" (next) (next)) )
   "Sym" )

(de extend X
   (setq *Class (car X)) )

# Class variables
(de var X
   (if (pair (car X))
      (def (cdar X) (car (++ X)) X)
      (def *Class (++ X) X) )
   X )

(de var: X
   (apply meta X This) )

### Math ###
(de scl ("N" . "Prg")
   (if "Prg"
      (let *Scl "N" (run "Prg"))
      (setq *Scl "N") ) )

(de sqrt (N F)
   (cond
      ((lt0 N) (quit "Bad argument" N))
      (N
         (and (num? F) (setq N (* N @)))
         (let (M 1  R 0)
            (while (>= N M)
               (setq M (>> -2 M)) )
            (loop
               (if (> (inc 'R M) N)
                  (dec 'R M)
                  (dec 'N R)
                  (inc 'R M) )
               (setq R (>> 1 R)  M (>> 2 M))
               (T (=0 M)) )
            (and F (> N R) (inc 'R))
            R ) ) ) )

# (Knuth Vol.2, p.442)
(de ** (X N)  # N th power of X
   (if (ge0 N)
      (let Y 1
         (loop
            (when (bit? 1 N)
               (setq Y (* Y X)) )
            (T (=0 (setq N (>> 1 N)))
               Y )
            (setq X (* X X)) ) )
      0 ) )

# Accumulate
(de accu ("Var" Key Val "Var2")
   (when Val
      (if
         (if "Var2"
            (lup (val "Var2") Key)
            (assoc Key (val "Var")) )
         (con @ (+ Val (cdr @)))
         (push "Var" (setq Val (cons Key Val)))
         (and "Var2" (idx @ Val 0)) ) ) )

### Pretty Printing ###
(de pretty (X N)
   (setq N (abs (space (or N 0))))
   (while (and (pair X) (== 'quote (car X)))
      (prin "'")
      (++ X) )
   (cond
      ((atom X) (print X))
      ((memq (car X) '(de dm redef))
         (_pretty
            (spPrt (++ X))
            (spPrt (++ X))
            (prtty1 X N Z) ) )
      ((memq (car X) '(let let?))
         (_pretty
            (cond
               ((atom (car X))
                  (spPrt (++ X))
                  (prtty? (++ X) N) )
               ((>= 12 (size (car X)))
                  (prin " (")
                  (let (P (circ? X)  Z (++ X))
                     (prtty2 Z NIL Z) )
                  (prin ")") )
               (T
                  (nlPrt N)
                  (prin "(")
                  (let (P (circ? X)  Z (++ X))
                     (prtty2 Z (+ N 3) Z) )
                  (prin " )") ) )
            (prtty1 X N Z) ) )
      ((== 'for (car X))
         (_pretty
            (cond
               ((or (atom (car X)) (atom (cdar X)))
                  (spPrt (++ X))
                  (prtty? (++ X) N) )
               ((>= 12 (size (car X)))
                  (spPrt (++ X)) )
               (T
                  (nlPrt N)
                  (prtty0 (++ X) (+ 3 N)) ) )
            (prtty1 X N Z) ) )
      ((== 'if2 (car X))
         (_pretty
            (when (>= 12 (size (head 2 X)))
               (spPrt (++ X))
               (spPrt (++ X)) )
            (prtty1 X N Z) ) )
      ((memq (car X) '(while until do state finally co))
         (prtty3 X N) )
      ((>= 12 (size X))
         (ifn (memq (car X) '(set setq default))
            (print X)
            (prin "(")
            (let (P (circ? X)  Z X)
               (printsp (++ X))
               (prtty2 X NIL Z) )
            (prin ")") ) )
      ((memq (car X) '(=: use later recur tco tab new))
         (_pretty
            (space)
            (print (++ X))
            (prtty1 X N Z) ) )
      ((memq (car X) '(setq default))
         (_pretty
            (if (cdddr X)
               (prog
                  (nlPrt N)
                  (prtty2 X N Z) )
               (spPrt (++ X))
               (nlPrt1 (++ X) N) ) ) )
      ((memq (car X) '(T NIL ! if ifn when unless case casq with catch throw push bind job rt in out err ctl))
         (prtty3 X N) )
      (T (prtty0 X N)) ) )

(de _pretty "Prg"
   (prin "(")
   (let (P (circ? X)  Z X)
      (print (++ X))
      (run "Prg") )
   (prin " )") )

(de prtty0 (X N)
   (prin "(")
   (let (P (circ? X)  Z X)
      (pretty (++ X) (- -3 N))
      (prtty1 X N Z) )
   (prin " )") )

(de prtty1 (X N Z)
   (loop
      (NIL X)
      (T (atom X)
         (prin " . ")
         (print X) )
      (T (== Z X)
         (prin " ." (unless P " )")) )
      (when (== P X)
         (nlPrt N)
         (inc 'N 3)
         (prin ". (")
         (setq Z P  P) )
      (nlPrt1 (++ X) N) ) )

(de prtty2 (X N Z)
   (loop
      (print (++ X))
      (NIL X)
      (T (atom X)
         (prin " . ")
         (print X) )
      (T (== Z X)
         (prin " ." (unless P " )")) )
      (when (== P X)
         (nlPrt N)
         (inc 'N 3)
         (prin ". (")
         (setq Z P  P) )
      (if N
         (prtty? (++ X) N)
         (space)
         (print (++ X)) )
      (NIL X)
      (T (atom X)
         (prin " . ")
         (print X) )
      (T (== Z X)
         (prin " ." (unless P " )")) )
      (when (== P X)
         (nlPrt N)
         (inc 'N 3)
         (prin ". (")
         (setq Z P  P) )
      (if N
         (nlPrt N)
         (space 2) ) ) )

(de prtty3 (X N)
   (prin "(")
   (let (P (circ? X)  Z X)
      (print (++ X))
      (when (or (atom (car X)) (>= 12 (size (car X))))
         (spPrt (++ X)) )
      (when X
         (prtty1 X N Z)
         (space) ) )
   (prin ")") )

(de prtty? (X N)
   (ifn (or (atom X) (>= 12 (size X)))
      (nlPrt1 X N)
      (spPrt X) ) )

(de spPrt (X)
   (space)
   (print X) )

(de nlPrt (N)
   (prinl)
   (space (+ 3 N)) )

(de nlPrt1 (X N)
   (prinl)
   (pretty X (+ 3 N)) )

(de pp ("X" C)
   (let *Dbg NIL
      (pretty
         (cond
            ((or C (pair "X"))
               (cons 'dm "X"
                  (if (pair "X")
                     (method (car "X") (cdr "X"))
                     (method "X" C) ) ) )
            ((getd "X") (cons 'de "X" @))
            (T (cons 'def (lit "X") (val "X"))) ) )
      (prinl)
      "X" ) )

(de show ("X" . @)
   (let *Dbg NIL
      (setq "X" (pass get "X"))
      (when (sym? "X")
         (print "X" (less (val "X")))
         (prinl)
         (maps
            '((X)
               (space 3)
               (if (atom X)
                  (prog (print X) (remark X))
                  (print (cdr X) (less (car X)))
                  (remark (car X)) )
               (prinl) )
            "X" ) )
      "X" ) )

(de view (X Y)
   (let *Dbg NIL
      (if (=T Y)
         (let N 0
            (recur (N X)
               (when X
                  (recurse (+ 3 N) (cddr X))
                  (space N)
                  (println (car X))
                  (recurse (+ 3 N) (cadr X)) ) ) )
         (let (P (circ? X)  Z X)
            (loop
               (T (atom X) (println X))
               (if (atom (car X))
                  (println '+-- (++ X))
                  (print '+---)
                  (view
                     (++ X)
                     (append Y (cons (if X "|   " "    "))) ) )
               (NIL X)
               (mapc prin Y)
               (T (== Z X) (println '*))
               (ifn (== P X)
                  (println '|)
                  (prinl "|*")
                  (setq Z P  P) )
               (mapc prin Y) ) ) ) ) )

### Check ###
# Assertions
(de assert Prg
   (when *Dbg
      (let A (if (cdr Prg) (cons 'and Prg) (car Prg))
         (cons
            (list 'unless A
               (list 'quit "'assert' failed" (lit A)) ) ) ) ) )

# Unit tests
(de test ("Pat" . "Prg")
   (bind (fish pat? "Pat")
      (unless (match "Pat" (run "Prg"))
         (msg "Prg")
         (quit "'test' failed" "Pat") ) ) )

############ lib/misc.l ############

# *Allow *Tmp

(de *Day . (Mon Tue Wed Thu Fri Sat Sun .))
(de *Mon . (Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec .))

### Locale ###
(de *Ctry)
(de *Lang)
(de *Sep0 . ".")
(de *Sep3 . ",")
(de *CtryCode)
(de *NatTrunkPrf)
(de *DateFmt @Y "-" @M "-" @D)
(de *DayFmt "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")
(de *MonFmt "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")

(de locale (Ctry Lang . @)  # "DE" "de" ["app/loc/" ..]
   (load (if (setq *Ctry Ctry) (pack "@loc/" @ ".l") "@loc/NIL.l"))
   (ifn (setq *Lang Lang)
      (for S (idx '*Uni)
         (set S S) )
      (====)
      (let L
         (sort
            (make
               ("loc" (pack "@loc/" Lang))
               (while (args)
                  ("loc" (pack (next) Lang)) ) ) )
         (balance '*Uni L T)
         (for S L
            (set (car (idx '*Uni S)) (val S)) ) )
      (====) ) )

(de "loc" (F)
   (when (info F)
      (in F
         (use X
            (while (setq X (read))
               (if (=T X)
                  ("loc" (read))
                  (set (link @) (name (read))) ) ) ) ) ) )

### String ###
(de wrap (Max X)
   (let R
      (make
         (for (Lst (split (chop X) " " "\n")  Lst)
            (let L (++ Lst)
               (while
                  (and Lst
                     (> Max
                        (+ (length (car Lst)) (sum length L)) ) )
                  (setq L (conc L (list " ") (++ Lst))) )
               (link L) ) ) )
      (if (atom X)
         (mapcar pack R)
         (glue "\n" R) ) ) )

### Number ###
(de money (N Cur)
   (if Cur
      (pack (format N 2 *Sep0 *Sep3) " " Cur)
      (format N 2 *Sep0 *Sep3) ) )

(de round (N D)
   (if (> *Scl (default D 3))
      (format (*/ N (** 10 (- *Scl D))) D *Sep0 *Sep3)
      (format N *Scl *Sep0 *Sep3) ) )

# Binary notation
(de bin (X I)
   (cond
      ((num? X)
         (let (S (and (lt0 X) '-)  L (& 1 X)  A (cons 0 I))
            (until (=0 (setq X (>> 1 X)))
               (at A (push 'L " "))
               (push 'L (& 1 X)) )
            (pack S L) ) )
      ((setq X
            (filter
               '((C) (not (sp? C)))
               (chop X) ) )
         (let (S (and (= '- (car X)) (++ X))  N 0  C)
            (loop
               (NIL (setq C (format (++ X))))
               (NIL (or (=0 C) (=1 C)))
               (setq N (| C (>> -1 N)))
               (NIL X (if S (- N) N)) ) ) ) ) )

# Octal notation
(de oct (X I)
   (cond
      ((num? X)
         (let (S (and (lt0 X) '-)  L (& 7 X)  A (cons 0 I))
            (until (=0 (setq X (>> 3 X)))
               (at A (push 'L " "))
               (push 'L (& 7 X)) )
            (pack S L) ) )
      ((setq X
            (filter
               '((C) (not (sp? C)))
               (chop X) ) )
         (let (S (and (= '- (car X)) (++ X))  N 0  C)
            (loop
               (NIL (setq C (format (++ X))))
               (NIL (>= 7 C 0))
               (setq N (| C (>> -3 N)))
               (NIL X (if S (- N) N)) ) ) ) ) )

# Hexadecimal notation
(de hex (X I)
   (cond
      ((num? X)
         (let (S (and (lt0 X) '-)  L (hex1 X)  A (cons 0 I))
            (until (=0 (setq X (>> 4 X)))
               (at A (push 'L " "))
               (push 'L (hex1 X)) )
            (pack S L) ) )
      ((setq X
            (filter
               '((C) (not (sp? C)))
               (chop X) ) )
         (let (S (and (= '- (car X)) (++ X))  N 0  C)
            (loop
               (NIL
                  (cdr
                     (rank
                        (setq C (char (++ X)))
                        '((48 . 48) (58) (65 . 55) (71) (97 . 87) (103)) ) ) )
               (setq N (| (- C @) (>> -4 N)))
               (NIL X (if S (- N) N)) ) ) ) ) )

(de hex1 (N)
   (let C (& 15 N)
      (and (> C 9) (inc 'C 7))
      (char (+ C `(char "0"))) ) )

### Tree ###
(de balance ("Var" "Lst" "Flg")
   (unless "Flg" (set "Var"))
   (let "Len" (length "Lst")
      (recur ("Lst" "Len")
         (unless (=0 "Len")
            (let ("N" (>> 1 (inc "Len"))  "L" (nth "Lst" "N"))
               (idx "Var" (car "L") T)
               (recurse "Lst" (dec "N"))
               (recurse (cdr "L") (- "Len" "N")) ) ) ) ) )

(de depth (Idx)  #> (max . average)
   (let (C 0  D 0  N 0)
      (cons
         (recur (Idx N)
            (ifn Idx
               0
               (inc 'C)
               (inc 'D (inc 'N))
               (inc
                  (max
                     (recurse (cadr Idx) N)
                     (recurse (cddr Idx) N) ) ) ) )
         (or (=0 (setq @@ C)) (*/ D C)) ) ) )

### Allow ###
(de allowed Lst
   (setq *Allow (cons NIL (car Lst)))
   (balance *Allow (sort (cdr Lst))) )

(de allow (X Flg)
   (nond
      (*Allow)
      (Flg (idx *Allow X T))
      ((member X (cdr *Allow)) (queue '*Allow X)) )
   X )

### Telephone ###
(de telStr (S)
   (cond
      ((not S))
      ((and *CtryCode (pre? (pack *CtryCode " ") S))
         (pack *NatTrunkPrf (cdr (member " " (chop S)))) )
      (T (pack "+" S)) ) )

(de expTel (S)
   (setq S
      (make
         (for (L (chop S) L)
            (ifn (sub? (car L) " -")
               (link (++ L))
               (let F NIL
                  (loop
                     (and (= '- (++ L)) (on F))
                     (NIL L)
                     (NIL (sub? (car L) " -")
                        (link (if F '- " ")) ) ) ) ) ) ) )
   (cond
      ((= "+" (car S)) (pack (cdr S)))
      ((head '("0" "0") S) (pack (cddr S)))
      (*CtryCode
         (let L *NatTrunkPrf
            (loop
               (NIL L (pack *CtryCode " " S))
               (NIL (= (++ L) (++ S))) ) ) ) ) )

### Date ###
(de $dat (S C)
   (if C
      (and
         (= 3
            (length (setq S (split (chop S) C))) )
         (date
            (format (car S))               # Year
            (or (format (cadr S)) 0)       # Month
            (or (format (caddr S)) 0) ) )  # Day
      (and
         (format S)
         (date
            (/ @ 10000)       # Year
            (% (/ @ 100) 100) # Month
            (% @ 100) ) ) ) )

# Localized
(de datStr (D F)
   (when (setq D (date D))
      (let
         (@Y (if F (pad 2 (% (car D) 100)) (pad 4 (car D)))
            @M (pad 2 (cadr D))
            @D (pad 2 (caddr D)) )
         (pack (fill *DateFmt)) ) ) )

(de strDat (S)
   (use (@Y @M @D)
      (and
         (match *DateFmt (chop S))
         (date
            (format @Y)
            (or (format @M) 0)
            (or (format @D) 0) ) ) ) )

(de expDat (S)
   (cond
      ((= "." S) (date))
      ((or (pre? "+" S) (pre? "-" S)) (+ (date) (format S)))
      (T
         (use (@Y @M @D X)
            (unless (match *DateFmt (setq S (chop S)))
               (if
                  (or
                     (cdr (setq S (split S ".")))
                     (>= 2 (length (car S))) )
                  (setq
                     @D (car S)
                     @M (cadr S)
                     @Y (caddr S) )
                  (setq
                     @D (head 2 (car S))
                     @M (head 2 (nth (car S) 3))
                     @Y (nth (car S) 5) ) ) )
            (and
               (setq @D (format @D))
               (date
                  (nond
                     (@Y (car (date (date))))
                     ((setq X (format @Y)))
                     ((>= X 100)
                        (+ X
                           (* 100 (/ (car (date (date))) 100)) ) )
                     (NIL X) )
                  (nond
                     (@M (cadr (date (date))))
                     ((setq X (format @M)) 0)
                     ((n0 X) (cadr (date (date))))
                     (NIL X) )
                  @D ) ) ) ) ) )

# Day of the week
(de day (Dat Lst)
   (when Dat
      (get
         (or Lst *DayFmt)
         (inc (% (inc Dat) 7)) ) ) )

# Week of the year
(de week (Dat)
   (let W
      (-
         (_week Dat)
         (_week (date (car (date Dat)) 1 4))
         -1 )
      (if (=0 W) 53 W) ) )

(de _week (Dat)
   (/ (- Dat (% (inc Dat) 7)) 7) )

# Last day of month
(de ultimo (Y M)
   (dec
      (if (= 12 M)
         (date (inc Y) 1 1)
         (date Y (inc M) 1) ) ) )

### Time ###
(de $tim (S)
   (setq S (split (chop S) ":"))
   (unless (or (cdr S) (>= 2 (length (car S))))
      (setq S
         (list
            (head 2 (car S))
            (head 2 (nth (car S) 3))
            (nth (car S) 5) ) ) )
   (when (format (car S))
      (time @
         (or (format (cadr S)) 0)
         (or (format (caddr S)) 0) ) ) )

(de stamp (Dat Tim)
   (and (=T Dat) (setq Dat (date T)))
   (default Dat (date)  Tim (time T))
   (pack (dat$ Dat "-") " " (tim$ Tim T)) )

(de dirname (F)
   (glue "/"
      (prog1
         (trim (split (chop F) "/"))
         (set (tail 1 @)) ) ) )

(de basename (F)
   (pack (last (trim (split (chop F) "/")))) )

# Echo here-documents
(de here (S)
   (skip)
   (echo S) )

# Print or eval
(de prEval ("Prg" "Ofs")
   (default "Ofs" 1)
   (for "X" "Prg"
      (if (atom "X")
         (prinl (eval "X" "Ofs"))
         (eval "X" "Ofs") ) ) )

############ Native Java ############

(de javac (Cls Ext Impl . @)
   (let (J (pack "tmp/" Cls ".java")  C (pack "tmp/" Cls ".class"))
      (call "mkdir" "-p" "tmp/")
      (out J
         (while (args)
            (prinl "import " (next) ";") )
         (prinl "public class " Cls
            (and Ext (pack " extends " @))
            (and Impl (pack " implements " (glue ", " Impl)))
            " {"  )
         (here "/**/")
         (prinl "}") )
      (call "javac" "-O" "-g:none" J)
      (push1 '*Bye (list 'call "rm" J C)) ) )

### Debug ###
`*Dbg

############ lib/debug.l ############

# Prompt
(de *Prompt
   (casq (car (symbols)) (pico) (T @)) )

# Browsing
(de doc (Sym Browser)
   (call (or Browser (sys "BROWSER") "w3m")
      (pack
         "file:"
         (and (= `(char '/) (char (path "@"))) "//")
         (path "@doc/ref")
         (if Sym
            (let (L (chop Sym)  C (car L))
               (and
                  (member C '("*" "+"))
                  (cadr L)
                  (setq C @) )
               (cond
                  ((>= "Z" C "A"))
                  ((>= "z" C "a") (setq C (uppc C)))
                  (T (setq C "_")) )
               (pack C ".html#" Sym) )
            ".html" ) ) ) )

(de more ("M" "Fun")
   (let *Dbg NIL
      (if (pair "M")
         ((default "Fun" print) (++ "M"))
         (println (type "M"))
         (setq
            "Fun" (list '(X) (list 'pp 'X (lit "M")))
            "M" (mapcar car (filter pair (val "M"))) ) )
      (loop
         (flush)
         (T (atom "M") (prinl))
         (T (line) T)
         ("Fun" (++ "M")) ) ) )

(de what (S)
   (let *Dbg NIL
      (setq S (chop S))
      (filter
         '(("X") (match S (chop "X")))
         (all) ) ) )


(de who ("X" . "*Prg")
   (let (*Dbg NIL  "Who" '("Who" @ @@ @@@))
      (make (mapc "who" (all))) ) )

(de "who" ("Y")
   (unless (or (ext? "Y") (memq "Y" "Who"))
      (push '"Who" "Y")
      (ifn (= `(char "+") (char "Y"))
         (and (pair (val "Y")) ("nest" @) (link "Y"))
         (for "Z" (pair (val "Y"))
            (if (atom "Z")
               (and ("match" "Z") (link "Y"))
               (when ("nest" (cdr "Z"))
                  (link (cons (car "Z") "Y")) ) ) )
         (maps
            '(("Z")
               (if (atom "Z")
                  (and ("match" "Z") (link "Y"))
                  (when ("nest" (car "Z"))
                     (link (cons (cdr "Z") "Y")) ) ) )
            "Y" ) ) ) )

(de "nest" ("Y")
   ("nst1" "Y")
   ("nst2" "Y") )

(de "nst1" ("Y")
   (let "Z" (setq "Y" (strip "Y"))
      (loop
         (T (atom "Y") (and (sym? "Y") ("who" "Y")))
         (and (sym? (car "Y")) ("who" (car "Y")))
         (and (pair (car "Y")) ("nst1" @))
         (T (== "Z" (setq "Y" (cdr "Y")))) ) ) )

(de "nst2" ("Y")
   (let "Z" (setq "Y" (strip "Y"))
      (loop
         (T (atom "Y") ("match" "Y"))
         (T (or ("match" (car "Y")) ("nst2" (car "Y")))
            T )
         (T (== "Z" (setq "Y" (cdr "Y")))) ) ) )

(de "match" ("D")
   (and
      (cond
         ((str? "X") (and (str? "D") (= "X" "D")))
         ((sym? "X") (== "X" "D"))
         (T (match "X" "D")) )
      (or
         (not "*Prg")
         (let *Dbg (up 2 *Dbg) (run "*Prg")) ) ) )

(de has ("X")
   (let *Dbg NIL
      (filter
         '(("S") (= "X" (val "S")))
         (all) ) ) )

(de can (X)
   (let *Dbg NIL
      (extract
         '(("Y")
            (and
               (= `(char "+") (char "Y"))
               (asoq X (val "Y"))
               (cons X "Y") ) )
         (all) ) ) )

# Class dependencies
(de dep ("C")
   (let *Dbg NIL
      (dep1 0 "C")
      (dep2 3 "C")
      "C" ) )

(de dep1 (N "C")
   (for "X" (type "C")
      (dep1 (+ 3 N) "X") )
   (space N)
   (println "C") )

(de dep2 (N "C")
   (for "X" (all)
      (when
         (and
            (= `(char "+") (char "X"))
            (memq "C" (type "X")) )
         (space N)
         (println "X")
         (dep2 (+ 3 N) "X") ) ) )

# Inherited methods
(de methods (Obj)
   (make
      (let Mark NIL
         (recur (Obj)
            (for X (val Obj)
               (nond
                  ((pair X) (recurse X))
                  ((memq (car X) Mark)
                     (link (cons (car X) Obj))
                     (push 'Mark (car X)) ) ) ) ) ) ) )

# Single-Stepping
(de _dbg (Lst)
   (or
      (atom (car Lst))
      (num? (caar Lst))
      (flg? (caar Lst))
      (== '! (caar Lst))
      (set Lst (cons '! (car Lst))) ) )

(de _dbg2 (Lst)
   (map
      '((L)
         (if (and (pair (car L)) (flg? (caar L)))
            (map _dbg (cdar L))
            (_dbg L) ) )
      Lst ) )

(de dbg (Lst)
   (when (pair Lst)
      (casq (++ Lst)
         ((case casq state)
            (_dbg Lst)
            (for L (cdr Lst)
               (map _dbg (cdr L)) ) )
         ((cond nond)
            (for L Lst
               (map _dbg L) ) )
         (quote
            (when (fun? Lst)
               (map _dbg (cdr Lst)) ) )
         ((job use let let? recur)
            (map _dbg (cdr Lst)) )
         (loop
            (_dbg2 Lst) )
         ((bind do)
            (_dbg Lst)
            (_dbg2 (cdr Lst)) )
         (for
            (and (pair (car Lst)) (map _dbg (cdar Lst)))
            (_dbg2 (cdr Lst)) )
         (T (map _dbg Lst)) )
      T ) )

(de d () (let *Dbg NIL (dbg ^)))

(de debug ("X" C)
   (ifn (traced? "X" C)
      (let *Dbg NIL
         (when (pair "X")
            (setq C (cdr "X")  "X" (car "X")) )
         (or
            (dbg (if C (method "X" C) (getd "X")))
            (quit "Can't debug" "X") ) )
      (untrace "X" C)
      (debug "X" C)
      (trace "X" C) ) )

(de ubg (Lst)
   (when (pair Lst)
      (map
         '((L)
            (when (pair (car L))
               (when (== '! (caar L))
                  (set L (cdar L)) )
               (ubg (car L)) ) )
         Lst )
      T ) )

(de u () (let *Dbg NIL (ubg ^)))

(de unbug ("X" C)
   (let *Dbg NIL
      (when (pair "X")
         (setq C (cdr "X")  "X" (car "X")) )
      (or
         (ubg (if C (method "X" C) (getd "X")))
         (quit "Can't unbug" "X") ) ) )

# Tracing
(de traced? ("X" C)
   (setq "X"
      (if C
         (method "X" C)
         (getd "X") ) )
   (and
      (pair "X")
      (pair (cadr "X"))
      (== '$ (caadr "X")) ) )

# Convert ((X Y) A B) --> ((X Y) ($ foo (X Y) A B))
(de trace ("X" C)
   (let *Dbg NIL
      (when (pair "X")
         (setq C (cdr "X")  "X" (car "X")) )
      (if C
         (unless (traced? "X" C)
            (or (method "X" C) (quit "Can't trace" "X"))
            (con @
               (cons
                  (conc
                     (list '$ (cons "X" C) (car @))
                     (cdr @) ) ) ) )
         (unless (traced? "X")
            (and (sym? (getd "X")) (quit "Can't trace" "X"))
            (and (num? (getd "X")) (expr "X"))
            (set "X"
               (list
                  (car (getd "X"))
                  (conc (list '$ "X") (getd "X")) ) ) ) )
      "X" ) )

# Convert ((X Y) ($ foo (X Y) A B)) --> ((X Y) A B)
(de untrace ("X" C)
   (let *Dbg NIL
      (when (pair "X")
         (setq C (cdr "X")  "X" (car "X")) )
      (if C
         (when (traced? "X" C)
            (con
               (method "X" C)
               (cdddr (cadr (method "X" C))) ) )
         (when (traced? "X")
            (let X (set "X" (cddr (cadr (getd "X"))))
               (and
                  (== '@ (++ X))
                  (= 1 (length X))
                  (= 2 (length (car X)))
                  (== 'pass (caar X))
                  (sym? (cdadr X))
                  (subr "X") ) ) ) )
      "X" ) )

(de *NoTrace
   @ @@ @@@
   pp show more
   what who can dep d e debug u unbug trace untrace )

(de traceAll (Excl)
   (let *Dbg NIL
      (for "X" (all)
         (or
            (memq "X" Excl)
            (memq "X" *NoTrace)
            (= `(char "*") (char "X"))
            (cond
               ((= `(char "+") (char "X"))
                  (mapc trace
                     (extract
                        '(("Y")
                           (and
                              (pair "Y")
                              (fun? (cdr "Y"))
                              (cons (car "Y") "X") ) )
                        (val "X") ) ) )
               ((pair (getd "X"))
                  (trace "X") ) ) ) ) ) )

# Process Listing
(de proc @
   (apply call
      (make (while (args) (link "-C" (next))))
      "ps" "-H" "-o" "pid,ppid,start,size,pcpu,cmd" ) )

# Benchmarking
(de bench Prg
   (let U (usec)
      (prog1
         (run Prg 1)
         (out 2
            (prinl
               (format (*/ (- (usec) U) 1000) 3)
               " sec" ) ) ) ) )
