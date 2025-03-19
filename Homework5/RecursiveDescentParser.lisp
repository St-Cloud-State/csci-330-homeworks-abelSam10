(defun parse-G (tokens pos)
  "Parses a G nonterminal: expects one of x, y, z, or w."
  (let ((uno (first tokens)))
    (if (member uno '(x y z w))
        (progn
          (incf pos) ; increment position after consuming token
          (values (list 'G- uno) pos (rest tokens)))
        (progn
          (error "Unexpected symbol ~a at position ~d, Expected one of 'x', 'y', 'z', 'w' instead!" uno pos)))))

(defun parse-E-prime (tokens pos)
  "Parses E': zero or more occurrences of 'o G'."
  (if (and tokens (eq (first tokens) 'o))
      (let ((tokens (rest tokens))
            (pos (1+ pos))) ; update position after consuming 'o'
        (multiple-value-bind (node pos tokens) (parse-G tokens pos)
          (multiple-value-bind (nodeE pos tokens) (parse-E-prime tokens pos)
            (values (list 'o node nodeE) pos tokens))))
      (values 'end pos tokens)))  ; return these three values

(defun parse-E (tokens pos)
  "Parses E: a G followed by E'."
  (multiple-value-bind (node pos tokens) (parse-G tokens pos)
    (multiple-value-bind (nodeE pos tokens) (parse-E-prime tokens pos)
      (values (list 'E- node nodeE) pos tokens))))

(defun parse-L (tokens pos)
  "Parses L: one or more 's' tokens."
  (let ((uno (first tokens)))
    (if (eq uno 's)
        (progn
          (incf pos) ;increment for consuming 's'
          (if (eq (first (rest tokens)) 's)
            (multiple-value-bind (nodeL pos tokens) (parse-L (rest tokens) pos)
                (values (list 'L- 's nodeL) pos tokens))
            (values (list 'L- 's) pos (rest tokens))))
        (error "Unexpected symbol ~a at position ~d, expected 's' instead" uno pos))))

(defun parse-S (tokens pos)
  "Parses S: either a simple s or a d L b structure."
  (let ((uno (first tokens)))
    (cond ((eq uno 's)
           (progn
             (incf pos) ; increment for consuming 's'
             (values (list 'S- 's) pos (rest tokens))))  ; return (AST, pos, tokens)
          ((eq uno 'd)
           (let ((tokens (rest tokens))
                 (pos (1+ pos))) ; update pos after consuming 'd'
                   (multiple-value-bind (nodeL pos tokens) (parse-L tokens pos)
                    (if (eq (first tokens) 'b)
                      (progn
                        (incf pos) ; increment for consuming 'b'
                        (values (list 'S- 'd nodeL 'b) pos (rest tokens)))
                      (error "Unexpected symbol ~a at position ~d, Expected 'b' instead" (first tokens) pos)))))
          (t (error "Unexpected symbol ~a at position ~d, Expected either 's' or 'd' instead" uno pos)))))

(defun parse-X (tokens pos)
  "Parses X: either e S or end."
  (if (and tokens (eq (first tokens) 'e))
      (let ((tokens (rest tokens))
            (pos (1+ pos))) ; update pos after consuming 'e'
        (multiple-value-bind (nodeS pos tokens) (parse-S tokens pos)
          (values (list 'X- 'e nodeS) pos tokens)))
      (values 'end pos tokens)))

(defun parse-I (tokens pos)
  "Parses I: expects an 'i', then E, then S, then optionally X."
  (unless (eq (first tokens) 'i)
    (error "Unexpected symbol ~a at position ~b, Expected 'i' instead;" (first tokens) pos))
  (let ((tokens (rest tokens))
        (pos (1+ pos))) ; update pos after consuming 'i'
    (multiple-value-bind (node pos tokens) (parse-E tokens pos)
      (multiple-value-bind (nodeS pos tokens) (parse-S tokens pos)
        (multiple-value-bind (nodeX pos tokens) (parse-X tokens pos)
          (if tokens (error "Unexpected symbol ~a at position ~d, Expected no characters!" (first tokens) pos) ())
          (decf pos)
          (format t "~&The string with length ~d, has a correct grammar! Below is its derivation:- ~%" pos)
          (values (list 'I- 'i node nodeS nodeX) tokens))))))

(defun grammar_tester (string pos)
  "Starts the parse from the I nonterminal with an initial position."
  (parse-I string pos))
