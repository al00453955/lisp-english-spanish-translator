; File: translator_v08.lisp
; English-Spanish Translator
; Intelligent Systems 407701
; Assignment Semester 2, 2003
; October 29th, 2003 
; Gabriel Tellez, 
; StudentID: 0314123 @ AUT

;;; Main method for English-Spanish translation

(defun get-spanish (sentence)
  (clean_output
   (common_noun_start?
    (exchange_nouns_adj
     (get-verbs
      (get-dets
       (get-adjectives
        (get-prepositions
         (get-nouns 
          (remove_non_terminals
           (parse sentence)
           '())
          '())
         '())
        '())
       '())
      '())
     '())
    '())
   '())
  )

;;; Method for presentation purposes only

(defun steps (sentence step)

  (cond ((equal step 1)
         (parsefull sentence))

        ((equal step 2)
         (parse sentence))

        ((equal step 3)
         (remove_non_terminals
          (parse sentence)
          '()))
         
        ((equal step 4)
         (get-nouns 
          (remove_non_terminals
           (parse sentence)
           '())
          '()))

        ((equal step 5)
         (get-prepositions
          (get-nouns 
           (remove_non_terminals
            (parse sentence)
            '())
           '())
          '()))

        ((equal step 6)
         (get-adjectives
          (get-prepositions
           (get-nouns 
            (remove_non_terminals
             (parse sentence)
             '())
            '())
           '())
          '()))

        ((equal step 7)
         (get-dets
          (get-adjectives
           (get-prepositions
            (get-nouns 
             (remove_non_terminals
              (parse sentence)
              '())
             '())
            '())
           '())
          '()))

        ((equal step 8)
         (get-verbs
          (get-dets
           (get-adjectives
            (get-prepositions
             (get-nouns 
              (remove_non_terminals
               (parse sentence)
               '())
              '())
             '())
            '())
           '())
          '()))

        ((equal step 9)
         (exchange_nouns_adj
          (get-verbs
           (get-dets
            (get-adjectives
             (get-prepositions
              (get-nouns 
               (remove_non_terminals
                (parse sentence)
                '())
               '())
              '())
             '())
            '())
           '())
          '()))
        
        ((equal step 10)
         (common_noun_start?
          (exchange_nouns_adj
           (get-verbs
            (get-dets
             (get-adjectives
              (get-prepositions
               (get-nouns 
                (remove_non_terminals
                 (parse sentence)
                 '())
                '())
               '())
              '())
             '())
            '())
           '())
          '()))
 
        ((equal step 11)
         (clean_output
          (common_noun_start?
           (exchange_nouns_adj
            (get-verbs
             (get-dets
              (get-adjectives
               (get-prepositions
                (get-nouns 
                 (remove_non_terminals
                  (parse sentence)
                  '())
                 '())
                '())
               '())
              '())
             '())
            '())
           '())
          '()))))

;;; Aux functions for the translation process

; Remove all parenthesis from a given list

(defun flatten (lst)
  (if (null lst)
      '()
    (if(listp (car lst))
        (append (flatten (car lst))(flatten (cdr lst)))
      (cons (car lst)(flatten (cdr lst))))))

; Concatenate a given element at the end of a given list

(defun cons-end (x lst)
  (if (null lst)
      (list x)
    (cons (car lst)(cons-end x (cdr lst)))))  

; Clean output from unnecessary labels
  
(defun clean_output(input output)
  (cond ((null input) output)

          ((or
            (equal 'det (car input))
            (equal 'n (car input)) 
            (equal 'v (car input))
            (equal 'p (car input))
            (equal 'adj (car input)))
           (clean_output
            (cddr input)                     
            (cons-end (car(reverse (cadr input))) output)))          
        
          (t (clean_output (cdr input) (cons-end (car input) output)))))

; If the sentence starts with a common noun, 
; attach the corresponding determiner (spanish language rules)
          
(defun common_noun_start? (input output)
  (let ((sta (car input))
        (noun (cadr input)))
    
    (cond ((null input) output)
          
          ((and
            (equal sta 'n)
            (equal output '())
            (equal (caddr (reverse(cadr input))) 'common)
            )
           (common_noun_start?
            (cddr input)                     
            (cons-end noun (cons-end 'n (cons-end (get-prop-det 'the noun) (cons-end 'det output))))))
          
          (t (common_noun_start? (cdr input) (cons-end (car input) output))))))

; Exchange nouns and adjectives (spanish language rules)

(defun exchange_nouns_adj (input output)
  (let ((adjective (cadr input))
        (noun (cadddr input)))

    (cond ((null input) output)
          
          ((and
            (equal (car input) 'adj)
            (equal (caddr input) 'n)
            )
           (exchange_nouns_adj
            (cddddr input)                     
            (cons-end adjective(cons-end 'adj (cons-end noun (cons-end 'n output))))))

          (t (exchange_nouns_adj (cdr input) (cons-end (car input) output))))))
          
; Remove non-terminals from the parsed sentence

(defun remove_non_terminals (input output)
  (cond ((null input) output)
        ((or(equal (car input) 's)(equal (car input) 'np)(equal (car input) 'vp)(equal (car input) 'pp))   
         (remove_non_terminals (cdr input) output))
        (t (remove_non_terminals (cdr input) (cons-end (car input) output)))))

; Get spanish form of verbs from the parsed sentence

(defun get-verbs (input output)
  (cond ((null input) output)

        ((and (equal (car input) 'v)
              (not(equal (caddr input) 'adj))
              (not(equal (caddr input) 'det))
              (not(equal (caddr input) 'p))
              (not(equal (caddr input) 'v))
              )
         (get-verbs 
          (cddr input)
          (cons-end (get-prop-verb 
                     (cadr input)
                     (car(reverse output)) 
                     '()
                     '()
                     '()
                     '())
                    (cons-end 'v output))
          )
         )
        ((and(equal (car input) 'v)
             (equal (caddr input) 'adj)) 
         (get-verbs 
          (cddr input)
          (cons-end (get-prop-verb 
                     (cadr input)
                     (car(reverse output))
                     (cadddr input)
                     '()
                     '()
                     '())
                    (cons-end 'v output))
          )
         )

        ((and(equal (car input) 'v)
             (equal (caddr input) 'det)) 
         (get-verbs 
          (cddr input)
          (cons-end (get-prop-verb 
                     (cadr input)
                     (car(reverse output))
                     '()
                     (cadddr input)
                     '()
                     '())
                    (cons-end 'v output))
          )
         )

        ((and(equal (car input) 'v)
             (equal (caddr input) 'p)) 
         (get-verbs 
          (cddr input)
          (cons-end (get-prop-verb 
                     (cadr input)
                     (car(reverse output))
                     '()
                     '()
                     (cadddr input)
                     '())
                    (cons-end 'v output))
          )
         )

        ((and(equal (car input) 'v)
             (equal (caddr input) 'v)) 
         (get-verbs 
          (cddr input)
          (cons-end (get-prop-verb 
                     (cadr input)
                     (car(reverse output))
                     '()
                     '()
                     '()
                     (cadddr input))
                    (cons-end 'v output))
          )
         )

        (t (get-verbs 
            (cdr input) 
            (cons-end (car input) output))
           )
        )
  )

; Get verb properties 

(defun get-prop-verb (word noun adj det p v)

        ; Verb conjugation list (present and past) for the  1st person

  (cond ((and (equal (cadr(reverse noun)) '1)(not(equal word 'am))(not(equal word 'was))(null v))
         (assoc
          word
          '((bite muerdo)
            (bit mordí)
            (eat como)
            (ate comí)
            (give doy)
            (gave dí)
            (go voy)
            (went fui)
            (have tengo)
            (had tuve)
            (love amo)
            (loved amé)
            (see veo)
            (saw ví)
            (sleep duermo)
            (slept dormí)
            (visit visito)
            (visited visité)
            (walk camino)
            (walked caminé)
            )))

        ; Verb conjugation list (present and past) for the  2nd person

        ((and (equal (cadr(reverse noun)) '2)(not(equal word 'are))(not(equal word 'were))(null v))
         (assoc
          word
          '((bite muerdes)
            (bit mordiste)
            (eat comes)
            (ate comiste)
            (give das)
            (gave diste)
            (go vas)
            (went fuiste)
            (have tienes)
            (had tuviste)
            (love amas)
            (loved amaste)
            (see ves)
            (saw viste)
            (sleep duermes)
            (slept dormiste)
            (visit visitas)
            (visited visitaste)
            (walk caminas)
            (walked caminaste)
            )))

        ; Verb conjugation list (present and past) for the  3rd person

        ((and (equal (cadr(reverse noun)) '3)(not(equal word 'is))(not(equal word 'was))(null v))
         (assoc
          word
          '((bites muerde)
            (bit mordió)
            (eats come)
            (ate comió)
            (gives da)
            (gave dió)
            (go va)
            (went fue)
            (has tiene)
            (had tuvo)
            (loves ama)
            (loved amó)
            (sees ve)
            (saw vió)
            (sleeps duerme)
            (slept durmió)
            (visits visita)
            (visited visitó)
            (walks camina)
            (walked caminó)
            )))

        ; Verb conjugation list (present and past) for the  4th person

        ((and (equal (cadr(reverse noun)) '4)(not(equal word 'are))(not(equal word 'were))(null v))
         (assoc
          word
          '((bite mordemos)
            (bit mordimos)
            (eat comemos)
            (ate comimos)
            (give damos)
            (gave dimos)
            (go vamos)
            (went fuimos)
            (have tenemos)
            (had tuvimos)
            (love amamos)
            (loved amamos)
            (see vemos)
            (saw vimos)
            (sleep dormimos)
            (slept dormimos)
            (visit visitamos)
            (visited visitamos)
            (walk caminamos)
            (walked caminamos)
            )))

        ; Verb conjugation list (present and past) for the  5th person

        ((and (equal (cadr(reverse noun)) '5)(not(equal word 'are))(not(equal word 'were))(null v))
         (assoc
          word
          '((bite muerden)
            (bit mordieron)
            (eat comen)
            (ate comieron)
            (give dan)
            (gave dieron)
            (go van)
            (went fueron)
            (have tienen)
            (had tuvieron)
            (love aman)
            (loved amaron)
            (see ven)
            (saw vieron)
            (sleep duermen)
            (visit visitan)
            (walk caminan)
            )))


        ; Verb "to be" present, 1st person

        ((and (equal (cadr(reverse noun)) '1)(equal word 'am)(not(null adj)))
         (if (equal (cadr (reverse adj)) 'ser)
             (list 'am 'soy)
           (list 'am 'estoy)))

        ((and (equal (cadr(reverse noun)) '1)(equal word 'am)(not(null det)))
         (list 'am 'soy))

        ((and (equal (cadr(reverse noun)) '1)(equal word 'am)(not(null p)))
         (list 'am 'estoy))

        ((and (equal (cadr(reverse noun)) '1)(equal word 'am)(not(null v)))
         (list 'am 'estoy))

        ; Verb "to be" past, 1st person

        ((and (equal (cadr(reverse noun)) '1)(equal word 'was)(not(null adj)))
         (if (equal (cadr (reverse adj)) 'ser)
             (list 'was 'era)
           (list 'was 'estaba)))

        ((and (equal (cadr(reverse noun)) '1)(equal word 'was)(not(null det)))
         (list 'was 'era))

        ((and (equal (cadr(reverse noun)) '1)(equal word 'was)(not(null p)))
         (list 'was 'estaba))

        ((and (equal (cadr(reverse noun)) '1)(equal word 'was)(not(null v)))
         (list 'was 'estaba))

        ; Verb "to be" present, 2nd person

        ((and (equal (cadr(reverse noun)) '2)(equal word 'are)(not(null adj)))
         (if (equal (cadr (reverse adj)) 'ser)
             (list 'are 'eres)
           (list 'are 'estas)))

        ((and (equal (cadr(reverse noun)) '2)(equal word 'are)(not(null det)))
         (list 'are 'eres))

        ((and (equal (cadr(reverse noun)) '2)(equal word 'are)(not(null p)))
         (list 'are 'estas))

        ((and (equal (cadr(reverse noun)) '2)(equal word 'are)(not(null v)))
         (list 'are 'estas))

        ; Verb "to be" past, 2nd person

        ((and (equal (cadr(reverse noun)) '2)(equal word 'were)(not(null adj)))
         (if (equal (cadr (reverse adj)) 'ser)
             (list 'were 'eras)
           (list 'were 'estabas)))

        ((and (equal (cadr(reverse noun)) '2)(equal word 'were)(not(null det)))
         (list 'were 'eras))

        ((and (equal (cadr(reverse noun)) '2)(equal word 'were)(not(null p)))
         (list 'were 'estabas))

        ((and (equal (cadr(reverse noun)) '2)(equal word 'were)(not(null v)))
         (list 'were 'estabas))

        ; Verb "to be" present, 3rd person

        ((and (equal (cadr(reverse noun)) '3)(equal word 'is)(not(null adj)))
         (if (equal (cadr (reverse adj)) 'ser)
             (list 'is 'es)
           (list 'is 'está)))

        ((and (equal (cadr(reverse noun)) '3)(equal word 'is)(not(null det)))
         (list 'is 'es))

        ((and (equal (cadr(reverse noun)) '3)(equal word 'is)(not(null p)))
         (list 'is 'está))

        ((and (equal (cadr(reverse noun)) '3)(equal word 'is)(not(null v)))
         (list 'is 'está))

        ; Verb "to be" past, 3rd person

        ((and (equal (cadr(reverse noun)) '3)(equal word 'was)(not(null adj)))
         (if (equal (cadr (reverse adj)) 'ser)
             (list 'was 'era)
           (list 'was 'estaba)))

        ((and (equal (cadr(reverse noun)) '3)(equal word 'was)(not(null det)))
         (list 'was 'era))

        ((and (equal (cadr(reverse noun)) '3)(equal word 'was)(not(null p)))
         (list 'was 'estaba))

        ((and (equal (cadr(reverse noun)) '3)(equal word 'was)(not(null v)))
         (list 'was 'estaba))

        ; Verb "to be" present, 4th person

        ((and (equal (cadr(reverse noun)) '4)(equal word 'are)(not(null adj)))
         (if (equal (cadr (reverse adj)) 'ser)
             (list 'are 'somos)
           (list 'are 'estamos)))

        ((and (equal (cadr(reverse noun)) '4)(equal word 'are)(not(null det)))
         (list 'are 'somos))

        ((and (equal (cadr(reverse noun)) '4)(equal word 'are)(not(null p)))
         (list 'are 'estamos))

        ((and (equal (cadr(reverse noun)) '4)(equal word 'are)(not(null v)))
         (list 'are 'estamos))

        ; Verb "to be" past, 4th person

        ((and (equal (cadr(reverse noun)) '4)(equal word 'were)(not(null adj)))
         (if (equal (cadr (reverse adj)) 'ser)
             (list 'were 'eramos)
           (list 'were 'estabamos)))

        ((and (equal (cadr(reverse noun)) '4)(equal word 'were)(not(null det)))
         (list 'were 'eramos))

        ((and (equal (cadr(reverse noun)) '4)(equal word 'were)(not(null p)))
         (list 'were 'estabamos))

        ((and (equal (cadr(reverse noun)) '4)(equal word 'were)(not(null v)))
         (list 'were 'estabamos))

        ; Verb "to be" present, 5th person

        ((and (equal (cadr(reverse noun)) '5)(equal word 'are)(not(null adj)))
         (if (equal (cadr (reverse adj)) 'ser)
             (list 'are 'son)
           (list 'are 'están)))

        ((and (equal (cadr(reverse noun)) '5)(equal word 'are)(not(null det)))
         (list 'are 'son))

        ((and (equal (cadr(reverse noun)) '5)(equal word 'are)(not(null p)))
         (list 'are 'están))

        ((and (equal (cadr(reverse noun)) '5)(equal word 'are)(not(null v)))
         (list 'are 'están))

        ; Verb "to be" past, 5th person

        ((and (equal (cadr(reverse noun)) '5)(equal word 'were)(not(null adj)))
         (if (equal (cadr (reverse adj)) 'ser)
             (list 'were 'eran)
           (list 'were 'estaban)))

        ((and (equal (cadr(reverse noun)) '5)(equal word 'were)(not(null det)))
         (list 'were 'eran))

        ((and (equal (cadr(reverse noun)) '5)(equal word 'were)(not(null p)))
         (list 'were 'estaban))

        ((and (equal (cadr(reverse noun)) '5)(equal word 'were)(not(null v)))
         (list 'were 'estaban))


        ; Verb list for gerund form
       
        (t (assoc
            word
            '((biting mordiendo)
              (eating comiendo)
              (giving dando)
              (going yendo)
              (having teniendo)
              (loving amando)
              (seeing viendo)
              (sleeping durmiendo)
              (visiting visitando)
              (walking caminando)
              )))
        
        ))


; Get spanish form of determiners from the parsed sentence

(defun get-dets (input output)
  (cond ((null input) output)
        ((equal (car input) 'det) 
         (get-dets (cddr input)(cons-end (get-prop-det (cadr input)(cadddr input)) (cons-end 'det output))))
        (t (get-dets (cdr input) (cons-end (car input) output)))))

; Get determiner properties 

(defun get-prop-det (word noun_adj)
  (cond ((and (equal (cadr noun_adj) 'male)(equal (caddr noun_adj) 'singular))
         (assoc
          word
          '((the el)
            (a un)
            (an un)
            (my mi)
            (this este)
            )))
        ((and (equal (cadr noun_adj) 'male)(equal (caddr noun_adj) 'plural))
         (assoc
          word
          '((the los)
            (my mis)
            (those estos)
            (these estos)
            )))
        ((and (equal (cadr noun_adj) 'female)(equal (caddr noun_adj) 'singular))
         (assoc
          word
          '((the la)
            (a una)
            (an una)
            (my mi)
            (this esta)
            )))
        ((and (equal (cadr noun_adj) 'female)(equal (caddr noun_adj) 'plural))
         (assoc
          word
          '((the las)
            (my mis)
            (those estas)
            (these estas)
            )))
        ))


; Auxiliar methods for detecting sentences like "apples are red"

(defun find_noun(output)
  (cond ((equal (car output) 'n)
         (cadr output))
        (t (find_noun (cdr output)))))

(defun secondlast(lst)
  (cadr (reverse lst)))


; Get spanish form of adjectives from the parsed sentence

(defun get-adjectives (input output)
  (cond ((null input) output)
        ((and 
          (equal (car input) 'adj) 
          (equal (secondlast output) 'v) 
          (or (equal (car(last output)) 'am)
              (equal (car(last output)) 'is)
              (equal (car(last output)) 'are)
              (equal (car(last output)) 'was)
              (equal (car(last output)) 'were)
              )
          ;(equal (cadddr input) '())
          )
         (get-adjectives 
                  (cddr input)
                  (cons-end 
                   (get-prop-adj (cadr input)(find_noun output)) 
                   (cons-end 'adj output))
                  )
         )
        ((equal (car input) 'adj)
         (get-adjectives 
          (cddr input)
          (cons-end 
           (get-prop-adj (cadr input)(cadddr input)) 
           (cons-end 'adj output))
          )
         )
        (t (get-adjectives (cdr input) (cons-end (car input) output)))))

; Get adjective properties 

(defun get-prop-adj (word noun)
  (cond ((and (equal (cadr noun) 'male)(equal (caddr noun) 'singular))
         (assoc
          word
          '(
            (angry male singular estar enojado)
            (beautiful male singular ser hermoso)
            (black male singular ser negro)
            (creative male singular ser creativo)
            (dangerous male singular ser peligroso)
            (fast male singular ser rapido)
            (hard male singular estar duro)
            (intelligent male singular ser inteligente)
            (mexican male singular ser mexicano)
            (red male singular ser rojo)
            (tired male singular estar cansado)
            (white male singular ser blanco)
            (wild male singular ser salvaje)
            )))
        ((and (equal (cadr noun) 'male)(equal (caddr noun) 'plural))
         (assoc
          word
          '(
            (angry male plural estar enojados)
            (beautiful male plural ser hermosos)
            (black male plural ser negros)
            (creative male plural ser creativos)
            (dangerous male plural ser peligrosos)
            (fast male plural ser rapidos)
            (hard male plural estar duros)
            (intelligent male plural ser inteligentes)
            (mexican male plural ser mexicanos)
            (red male plural ser rojos)
            (tired male plural estar cansados)
            (white male plural ser blancos)
            (wild male plural ser salvajes)
            )))
        ((and (equal (cadr noun) 'female)(equal (caddr noun) 'singular))
         (assoc
          word
          '(
            (angry female singular estar enojada)
            (beautiful female singular ser hermosa)
            (black female singular ser negra)
            (creative female singular ser creativa)
            (dangerous female singular ser peligrosa)
            (fast female singular ser rapida)
            (hard female singular estar dura)
            (intelligent female singular ser inteligente)
            (mexican female singular ser mexicana)
            (red female singular ser roja)
            (tired female singular estar cansada)
            (white female singular ser blanca)
            (wild female singular ser salvaje)
            )))
        ((and (equal (cadr noun) 'female)(equal (caddr noun) 'plural))
         (assoc
          word
          '(
            (angry female plural estar enojadas)
            (beautiful female plural ser hermosas)
            (black female plural ser negras)
            (creative female plural ser creativas)
            (dangerous female plural ser peligrosas)
            (fast female plural ser rapidas)
            (hard female plural estar duras)
            (intelligent female plural ser inteligentes)
            (mexican female plural ser mexicanas)
            (red female plural ser rojas)
            (tired female plural estar cansadas)
            (white female plural ser blancas)
            (wild female plural ser salvajes)
            )))
        ))

; Get spanish form of nouns from the parsed sentence

(defun get-nouns (input output)
  (cond ((null input) output)
        ((equal (car input) 'n) 
         (get-nouns (cddr input)(cons-end (get-prop-n (cadr input)) (cons-end 'n output))))
        (t (get-nouns (cdr input) (cons-end (car input) output)))))

; Get noun properties 

(defun get-prop-n (word)
  (assoc
   word
   '((albert male singular proper 3 albert)
     (apple female singular common 3 manzana)
     (apples female plural common 5 manzanas)
     (car male singular common 3 coche)
     (cars male plural common 5 coches)
     (cat male singular common 3 gato)
     (cats male plural common 5 gatos)
     (chair female singular common 3 silla)
     (chairs female plural common 5 sillas)
     (dog male singular common 3 perro)
     (dogs male plural common 5 perros)
     (father male singular common 3 padre)
     (he male singular proper 3 él)
     (horse male singular common 3 caballo)
     (horses male plural common 5 caballos)
     (husband male singular common 3 esposo)
     (kermit male singular proper 3 kermit)
     (I male singular proper 1 yo)
     (kitchen female singular common 3 cocina)
     (kitchens female plural common 5 cocinas)
     (lion male singular common 3 león)
     (lions male plural common 5 leones)
     (man male singular common 3 hombre)
     (men male plural common 5 hombres)
     (mother female singular common 3 madre)
     (neighbour male singular common 3 vecino)
     (neighbours male plural common 5 vecinos)
     (night female singular common 3 noche)
     (nights female plural common 5 noches)
     (parents male plural common 5 padres)
     (park male singular common 3 parque)
     (parks male plural common 5 parques)
     (sea male singular common 3 mar)
     (she female singular proper 3 ella)
     (surveillance female singular 3 vigilancia)
     (table female singular common 3 mesa)
     (tables female plural common 5 mesas)
     (teacher male singular common 3 profesor)
     (teachers male plural common 5 profesores)
     (they male plural proper 5 ellos)
     (tree male singular common 3 árbol)
     (trees male plural common 5 árboles)
     (we male plural proper 4 nosotros)
     (wife female singular common 3 esposa)
     (woman female singular common 3 mujer)
     (women female plural common 5 mujeres)
     (world male singular common 3 mundo)
     (you male singular proper 2 tu)
     )))

; Get spanish form of prepositions from the parsed sentence

(defun get-prepositions (input output)
  (cond ((null input) output)
        ((equal (car input) 'p) 
         (get-prepositions (cddr input) (cons-end (get-prop-p (cadr input)) (cons-end 'p output))))
        (t (get-prepositions (cdr input) (cons-end (car input) output)))))

; Get preposition properties 

(defun get-prop-p (word)
  (assoc
   word
   '((at en)
     (near cerca_de)
     (in en)
     (inside dentro_de)
     (like como)
     (on sobre)
     (with con)
     (without sin)
     (by por)
     (from de)
     (to a)
     (behind detrás_de)
     (under bajo)
     )))  
  
;;; Variables for the parser

(defun start-symbol()
  's)

(defvar *sdset* '())

;;; Parse method, returns the first parsed sentence

(defun parse (input)
  (setf *sdset* '())
  (ptopdown input (list (list '() (start-symbol))))
  (flatten (car *sdset*)))

;;; Parse method, returns all the possible parsing solutions of a given sentence

(defun parsefull (input)
  (setf *sdset* '())
  (ptopdown input (list (list '() (start-symbol))))
  *sdset*)

;;; Main method for a top-down, left-right, non-deterministic parser using 
;;; a stack method (depth-first search) to implement backtracking 

(defun ptopdown (input stack)
  (cond ((and (paccept input stack)
              (pterminate (car (caar stack)))
              nil)
         nil)
    
        ((and (pair? stack)
              (pair? (cdr stack))
              (inactive? (car stack)))
         (ptopdown input (attach stack)))
    
        ((pscan input (active (car stack)))
         (ptopdown (cdr input) (shift input stack)))
    
        ((predict? input (active (car stack)))
         (pstore input stack (nprod (car (active (car stack))))))
        
        (t nil)))
        
;;; Aux functions for the translation process     
        
; If the node is not the end
        
(defun active (node)
  (cdr node))

; Is the node the end?

(defun inactive? (node)
  (null (cdr node)))

; If the ptopdown method finished to parse the sentence   
  
(defun paccept (input stack)
  (and (null input)
       (inactive? (car stack))
       (null (cdr stack))))

; Update the sdset global variable, concatenating the parsed sentence to it       
       
(defun pterminate (node)
  (setf *sdset* (cons node *sdset*)))

(defun attach (stack)
  (cons
   (cons
    (append
     (car (cadr stack))
     (list (caar stack)))
    (cddr (cadr stack)))
   (cddr stack)))
   
; Search if the expansion is terminal 
; symbol and then search for it in the lexicon   

(defun pscan (input stack)
  (and (pair? input)
       (pair? stack)
       (terminal? (car stack))
       (lexicon (car stack)(car input))))

; Put the word with its symbol in the stack
       
(defun shift (input stack)
  (cons
   (list
    (list
     (car (active (car stack)))
     (car input)))
   stack))

; Search if a non-terminal symbol   
   
(defun predict? (input stack)
  (and (pair? input)
       (pair? stack)
       (nonterminal? (car stack))))

; The method that does the recursive-backtraking       
       
(defun pstore (input stack expansions)
  (cond ((pair? expansions)
         (or (ptopdown input (cons (car expansions) stack))
             (pstore input stack (cdr expansions))))
        (t nil)))

; Expanding a node
        
(defun nprod (symbol)
  (nprod-1 (list symbol)(productions symbol)))

(defun nprod-1 (mother expansions)
  (cond ((pair? expansions)
         (cons
          (cons mother (car expansions))
          (nprod-1 mother (cdr expansions))))
        (t '())))

;;; The Grammar
        
(defun productions (sym)
  (cdr
   (assoc
    sym
    '(
      (s
       (np)
       (np vp)
       )
      (np
       (n)
       (det n)
       (n pp)
       (det n pp)
       (adj n)
       (adj det n)
       (det adj n)
       (adj n pp)
       (det adj n pp)
       )
      (pp
       (p np)
       (p vp)
       )
      (vp
       (v)
       (v adj)
       (v adj pp)
       (v np)
       (v pp)
       (v vp)
       )))))        

;;; Aux grammar functions 

(defun nonterminal? (sym)
  (member?
   sym
   '(s np vp pp)))

(defun terminal? (sym)
  (member?
   sym
   '(det n v p adj)))

(defun member? (sym set)
  (pair? (member sym set)))   

(defun pair?(a)
  (cond ((null a) nil)
        ((listp a) 'T)))
   
;;; Lexicon for the parser

(defun lexicon (category word)
  (member?
   word
   (assoc
    category
    '((det a 
           an 
           my 
           the 
           these
           this 
           those
           )
      (n I you he she it we they
         mother father parents
         albert
         apple apples 
         car cars
         cat cats
         chair chairs
         dog dogs
         horse horses
	 kermit
         kitchen kitchens
         lion lions
         man men 
         neighbour neighbours
         night nights
         park parks 
         sea
         surveillance
         table tables
         teacher teachers
         tree trees
         wife husband
         woman women 
         world
         )
      (v am are is was were
         bite bites bit biting
         eat eats ate eating
         give gives gave giving
         go goes went going
         have has had having
         love loves loved loving
         see sees saw seeing
         sleep sleeps slept sleeping
         visit visits visited visiting
         walk walks walked walking
         )
      (p at 
         behind          
         by 
         from
         in 
         inside 
         like
         near
         on 
         to 
         under
         with 
         without 
         )
      (adj angry
           beautiful
           black 
           creative 
           dangerous
           fast
           hard 
           intelligent 
	   mexican
           red  
           tired       
           white 
           wild
           )
      )
    )
   )
  )  
