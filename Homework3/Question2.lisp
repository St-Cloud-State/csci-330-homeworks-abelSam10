(defun partition-list (lst)
 "Partition LST into two nearly equal halves.
Returns a list of two lists: (list first-half second-half)."
  (cond
    ((null lst) (list nil nil))  ; no elements: both halves are empty
    ((null (cdr lst)) (list lst nil)) ; one element: first half gets it, second is empty
    (t 
     (let* ((first (car lst))
            (second (cadr lst))
            (rest (cddr lst))
            (parts (partition-list rest))   ; recursively partition the rest
            (first-half (car parts))
            (second-half (cadr parts)))
       (list (cons first first-half)    ; add first to first half
             (cons second second-half))))))

(defun mergeList (l1 l2)
"Merge two sorted lists L1 and L2 into one sorted list."
  (cond
    ((and (null l1) (null l2)) ()); if both lists are empty
    ((null l1) l2) ;check if either list is emoty to return the other
    ((null l2) l1)
    ((<= (car l1) (car l2));compares the first item of both lis to find smaller
     (cons (car l1) (mergeList (cdr l1) l2)))
    (t
     (cons (car l2) (mergeList l1 (cdr l2))))))

(defun mergesort (lst)
 "Sort LST using the mergesort algorithm."
  (if (or (null lst) (null (cdr lst)))
      lst  ; A list with 0 or 1 element is already sorted.
      (let* ((parts (partition-list lst)); call partition function
             (first-half (car parts)) ;assign those lists
             (second-half (cadr parts))
             (sorted-first (mergesort first-half));recursive call
             (sorted-second (mergesort second-half)))
        (mergeList sorted-first sorted-second))));call Merge function
