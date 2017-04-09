(box 1)
(box (box 1))
((x y z) ((box 1) (box 2) (box3)))

(set-box! (findvar x stack) (lambda ()))

(set-box! (findvar y stack) (unbox (findvar z stack)))

((x y z) ((box (box 1)) (box 2) (box3)))

;now y can change what's inside x so that x changes values
(set-box! (findvar x stack) (findvar y stack))
