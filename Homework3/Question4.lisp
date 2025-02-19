(defun insert-track (x checked remaining)
  "Insert X into the already sorted list REMAINING,
using EXAMINED to track items already checked.
This function terminates either when REMAINING is empty
or when the head of REMAINING is greater than X."
  (cond
    ;;No more items to examine—X is larger than every checked item.
    ((null remaining) (append checked (list x))) ;; Here, we simply append X at the end of checked.
    
    ;;Found an item greater than X.
    ((< x (car remaining)) (append checked (list x) remaining)) ;append x at right before car of remaining
     
     ;; Otherwise, move the first element of REMAINING to EXAMINED and continue.
    (t (insert-track x (append checked (list (car remaining))) (cdr remaining)))))

;; Main insertion function.
(defun insert (x sorted)
  "Insert element X into the sorted list SORTED in ascending order."
  (cond
    ((null sorted) (list x)); If sorted is empty, return (X)
    
    ((< x (car sorted)) (cons x sorted)) ; If X is smaller than the first element, put it in front.

    ((>= x (car (last sorted))) (append (butlast sorted) (list (car (last sorted)) x)))
     ;; If X is greater than or equal to the last element, add it to the end.
     
    ;; Otherwise, use the above helper function that tracks the checked and remaining portions. 
    (t
     (insert-track x '() sorted))))

;; The insertion sort helper: Given two lists, one sorted and one unsorted,
;; it moves one item from unsorted into sorted.
(defun insertion-sort-helper (sorted unsorted)
  "Recursively insert each element from UNSORTED into SORTED.
SORTED contains the processed items; UNSORTED holds items yet to be inserted."
  (if (null unsorted)
      sorted  ; nothing left to insert.
      (insertion-sort-helper (insert (car unsorted) sorted)
                             (cdr unsorted))))

;; The main insertion sort function.
(defun insertion-sort (lst)
  "Sort the list LST in ascending order using insertion sort in a declarative style."
  (insertion-sort-helper nil lst))
