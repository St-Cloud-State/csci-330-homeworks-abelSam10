;;; Partition the original list into sorted pairs.
(defun make-sorted-pairs (lst)
  "Partition LST into a list of sorted pairs (or a singleton if odd length)."
  (cond
    ((null lst) nil)
    ((null (cdr lst)) (list lst))
    (t (let* ((first (car lst))
              (second (cadr lst))
              (pair (if (<= first second)
                        (list first second)
                        (list second first)))
              (rest (cddr lst)))
         (cons pair (make-sorted-pairs rest))))))

;;; Merge two sorted lists into one sorted list.
(defun merge-two (l1 l2)
  "Used the same logic as Question 2"
  (cond
    ((and (null l1) (null l2)) ()); if both lists are empty
    ((null l1) l2) ;check if either list is emoty to return the other
    ((null l2) l1)
    ((<= (car l1) (car l2));compares the first item of both lis to find smaller
     (cons (car l1) (merge-two (cdr l1) l2)))
    (t
     (cons (car l2) (merge-two l1 (cdr l2)))))) 


;;; Merge adjacent pairs of sorted lists.
(defun merge-adjacent (list-of-lists)
  (cond
    ((null list-of-lists) nil)
    ((null (cdr list-of-lists)) list-of-lists)
    (t (cons (merge-two (car list-of-lists) (cadr list-of-lists));call merge function for first two items
             (merge-adjacent (cddr list-of-lists)))))) ;recursively join and join with the alredy merged ones

;;; Perform successive merge passes until one sorted list remains.
(defun merge-pass (lists)
  "Recursively merge adjacent sorted lists until a single sorted list remains."
  (if (or (null lists) (null (cdr lists)))
      (if lists (car lists) nil)
      (merge-pass (merge-adjacent lists))))

;;; Bottom-Up Mergesort Function
(defun bottom-up-mergesort (lst)
  "Sort LST using a bottom-up mergesort algorithm."
  (merge-pass (make-sorted-pairs lst)))
