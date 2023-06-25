(defun c:get_data ()

  (Setvar "Cmdecho" 0)
  (setq oldlayer (getvar "clayer"))
  (setq oldosnap (getvar "OSMODE"))
  (command "osnap" "none")
  (setq file_name "C:\\Ent.txt")
  (setq file_n (open file_name "r"))
  (if (= file_n nil)
    (progn
      (alert "\nYou should first export Data!")
      (exit)
    )
  )

  (setq	xx '()
	yy '()
  )
  (setq sn 1)
  (setq str1 (read-line file_n))
  (while (/= str1 nil)
    (setq ini_str 0)
    (setq x1 (distof (find_str str1 ini_str)))
    (setq y1 (distof (find_str str1 ini_str)))
    (if	(< ini_str (strlen str1))
      (setq tem1 (substr str1 (1+ ini_str) (- (strlen str1) ini_str)))
      (setq tem1 "")
    )
    (setq xx (append xx (list x1)))
    (setq yy (append yy (list y1)))
    (setq sn (+ sn 1))
    (setq str1 (read-line file_n))
  )
  (close file_n)
  (if (= (tblsearch "layer" "Parcel") nil)
    (command "layer" "m" "Parcel" "c" "r" "" "")
    (progn
      (command "layer" "t" "Parcel" "u" "Area" "")
      (command "layer" "s" "Parcel" "")
      (setq lay_name "Parcel")
      (setq s_ent (ssget "X" (list (cons 8 lay_name))))
      ; (if (/= s_ent nil)
	; (Command "erase" s_ent "")
      ; )

    )
  )

  (setq nop (length xx))
  (setq n 0)
					;(setq Max_Dist 0.0)
  (while (/= n nop)
    (setq p1 (list (nth n xx) (nth n yy)))
    (if	(= n (- nop 1))
      (setq p2 (list (nth 0 xx) (nth 0 yy)))
      (setq p2 (list (nth (+ n 1) xx) (nth (+ n 1) yy)))
    )
					;(if	(> (Distance p1 p2) Max_Dist)
					;  (setq Max_dist (Distance p1 p2))
					;)
    (if	(> (Distance p1 p2) 0)
      (command "Line" p1 p2 "")
    )
    (setq n (+ n 1))
  )

;  (if (= ts nil)
;    (setq ts 2.0)
;  )
  (command "zoom" "E")
  (command "zoom" "0.8x")
					;(command "layer" "lo" "Parcel" "")
  (command "layer" "s" oldlayer "")
  (setvar "osmode" oldosnap)
  (princ)  
 (C:GL_ALL)
)					;end defun


					;find strings between ", "
(defun find_str	(str1 in_str)

  (setq in1_str in_str)
  (setq s_found "")
  (while (/= s_found ",")
    (setq in1_str (+ 1 in1_str))
    (setq s_found (substr str1 in1_str 1))
    (if	(> in1_str (strlen str1))
      (progn
	(setq s_found ",")
	(setq in1_str (1- (strlen str1)))
      )
    )
  )
  (setq str_found (substr str1 (1+ in_str) (- in1_str in_str 1)))
  (setq ini_str in1_str)
  str_found
)					;end defun


(defun c:s_ts ()


  (setq ts (* (getvar "VIEWSIZE") 0.018))
  (setq o_ts ts)
  (princ (strcat "\nSuitable TEXT Size for Current View :"
		 (rtos o_ts 2 2)
	 )
  )
  (setq ts (getreal "\nEnter the Text Size :"))
  (if (or (= ts nil) (<= ts 0.0))
    (setq ts o_ts)
  )
  (setq tset 1)
  (princ)
)


(defun c:s_mtd ()

  (initget "R B N")
  (setq	smtd
	 (getkword
	   "\nDo you want to Equivalent Area in Ropani or Bigha or None <R>/B/N:"
	 )
  )
  (if (or (= smtd nil) (= smtd "N"))
    (setq a_mtd 0)
    (if	(= smtd "R")
      (setq a_mtd 1)
      (setq a_mtd 2)
    )
  )
  (princ)
)


(defun C:GA ()

  (setvar "cmdecho" 0)
				
  (if (/= tset 1)
    (setq ts (* (getvar "VIEWSIZE") 0.025))
  )
  (if (= a_mtd nil)
    (setq a_mtd 0)
  )
  (setq oldosnap (getvar "osmode"))
  (command "osnap" "none")
  (command "style" "romans" "romans.shx" "" "1.0" "" "" "" "")
  (setq pt (getpoint "\nSelect a Internal Point"))
  (command "boundary" pt "")
  (if (/= (entlast) nil)
    (progn
      (setq tem (entget (entlast)))
      (if (/= (strcase (cdr (assoc 0 tem))) "LWPOLYLINE")
	(progn
	  (alert "The Selected Point is not inside a closed Boundary!"
	  )
	  (quit)
	)
      )
    )
    (quit)
  )

  (command "area" "e" "l")
  (setq ar1 (getvar "area"))
  (command "erase" "l" "")
  (setq oldlayer (getvar "clayer"))
  (if (= (tblsearch "layer" "Area") nil)
    (command "layer" "m" "Area" "c" "RED" "" "")
    (progn
      (command "layer" "t" "Area" "u" "Area" "")
      (command "layer" "s" "Area" "")
    )
  )

  (command "layer" "s" "Area" "")
  (command "text"
	   "j"
	   "mc"
	   pt
	   ts
	   "0.0"
	   (Strcat  (rtos ar1 2 2) )
  )

  (setq pt1 (list (nth 0 pt) (- (nth 1 pt) (* 2 ts))))
  (if (= a_mtd 1)
    (progn
      (setq atext (get_Ropani ar1))
      (command "text" "j" "mc" pt1 ts "0.0" atext)
    )

    (if	(= a_mtd 2)
      (progn
	(setq atext (get_Bigha ar1))
	(command "text" "j" "mc" pt1 ts "0.0" atext)
      )
    )
  )
  (command "layer" "s" oldlayer "")
  (setvar "osmode" oldosnap)
  (setvar "cmdecho" 1)
  (princ)
)


(defun get_Ropani (ar)

  (setq arf (float (* ar 10.76391)))
  (setq ra (float (* 74 74)))
  (setq aa (float (/ ra 16)))
  (setq pa (float (/ aa 4)))
  (setq da (float (/ pa 4)))

  (setq Rt (fix (/ arf ra)))
  (setq arf1 (- arf (* rt ra)))
  (setq At (fix (/ arf1 aa)))
  (setq arf2 (- arf1 (* at aa)))
  (setq Pt (fix (/ arf2 pa)))
  (setq arf3 (- arf2 (* pt pa)))
  (setq Dt (round (float (/ arf3 da)) 2))


  (if (>= Dt 3.99)
    (progn
      (setq Dt 0)
      (setq Pt (+ Pt 1))
    )
  )

  (if (= Pt 4)
    (progn
      (setq Pt 0)
      (setq At (+ At 1))
    )
  )

  (if (= At 16)
    (progn
      (setq At 0)
      (setq Rt (+ Rt 1))
    )
  )

  (setq	a_text (strcat "("
		       (rtos Rt 2 0)
		       "-"
		       (rtos At 2 0)
		       "-"
		       (rtos Pt 2 0)
		       "-"
		       (rtos Dt 2 0)
		       ")"
	       )
  )
  a_text
)


(defun get_Bigha (ar)

  (setq arf (float (* ar 10.76391)))
  (setq ba (float (* 270 270)))
  (setq ka (float (/ ba 20)))
  (setq da (float (/ ka 20)))
  (setq kna (float (/ da 4)))

  (setq Bt (fix (/ arf ba)))
  (setq arf1 (- arf (* bt ba)))
  (setq Kt (fix (/ arf1 ka)))
  (setq arf2 (- arf1 (* kt ka)))
  (setq Dt (fix (/ arf2 da)))
  (setq arf3 (- arf2 (* dt da)))
  (setq Knt (round (float (/ arf3 kna)) 2))


  (if (>= knt 3.99)
    (progn
      (setq Knt 0)
      (setq Dt (+ Dt 1))
    )
  )

  (if (= Dt 20)
    (progn
      (setq Dt 0)
      (setq Kt (+ Kt 1))
    )
  )

  (if (= Kt 20)
    (progn
      (setq Kt 0)
      (setq Bt (+ Bt 1))
    )
  )

  (setq	a_text (strcat "(B="
		       (rtos Bt 2 0)
		       "-"
		       (rtos Kt 2 0)
		       "-"
		       (rtos Dt 2 0)
		       "-"
		       (rtos Knt 2 2)
		       ")"
	       )
  )
  a_text
)



(defun C:GL ()
  (setvar "cmdecho" 0)

					;(if (= ts nil)    
					;  (c:s_ts)
					;)

  (if (/= tset 1)
    (setq ts (* (getvar "VIEWSIZE") 0.018))
  )
  (setvar "pdmode" 35)
  (setvar "pdsize" ts)
  (setq oldlayer (getvar "clayer"))
  (if (= (tblsearch "layer" "Length") nil)
    (command "layer" "m" "Length" "c" "BLUE" "" "")
    (progn
      (command "layer" "t" "Length" "u" "Length" "")
      (command "layer" "s" "Length" "")
    )
  )
  (command "layer" "s" "Length" "")
  (command "style" "romans" "romans.shx" "" "1.0" "" "" "" "")


  (setq oldosnap (getvar "osmode"))
  (command "osnap" "none")
  (setq ent1 (entsel "\nSelect  an edge to find it's Length:"))
  (setq ent2 (entget (car ent1)))
  (if (= (strcase (cdr (assoc 0 ent2))) "LINE")
    (progn
      (setq p1x (nth 0 (cdr (assoc 10 ent2))))
      (setq p1y (nth 1 (cdr (assoc 10 ent2))))
      (setq p2x (nth 0 (cdr (assoc 11 ent2))))
      (setq p2y (nth 1 (cdr (assoc 11 ent2))))
      (setq len1 (sqrt (+ (expt (- p1x p2x) 2) (expt (- p1y p2y) 2))))
      (setq delx (- p2x p1x))
      (setq dely (- p2y p1y))
      (setq p1 (list p1x p1y))
      (setq p2 (list p2x p2y))
      (Command "Point" p1)
      (Command "Point" p2)
      (setq ang (angle p1 p2))
      (setq ang1 (/ (* ang 180) 3.1416))
      (setq ang2 ang)
      (if (< ang1 90)
	(setq ang2 (+ ang (/ 3.1416 2)))
	(setq ang2 (- ang (/ 3.1416 2)))
      )

      (setq pt (list (- (/ (+ p1x p2x) 2) (* 1.5 ts (cos ang2)))
		     (- (/ (+ p1y p2y) 2) (* 1.5 ts (sin ang2)))
	       )
      )
    )
    (progn
      (alert
	"The Selecte entity is not a Line. You should explode, if a Polyline is selected!"
      )
      (quit)
    )
  )


  (command "text" "j" "mc" pt ts ang1 (rtos len1 2 3))
  (setq len1 0.0)
  (command "layer" "s" oldlayer "")
  (setvar "osmode" oldosnap)
  (setvar "cmdecho" 1)
  (princ)
)




(defun C:GL_All()
  (setvar "cmdecho" 0)

  (if (/= tset 1)
    (setq ts (* (getvar "VIEWSIZE") 0.018))
  )
  (setvar "pdmode" 35)
  (setvar "pdsize" ts)
  (setq oldosnap (getvar "osmode"))
  (command "osnap" "none")
  (setq oldlayer (getvar "clayer"))
  (if (= (tblsearch "layer" "Length") nil)
    (command "layer" "m" "Length" "c" "BLUE" "" "")
    (progn
      (command "layer" "t" "Length" "u" "Length" "")
      (command "layer" "s" "Length" "")
    )
  )
  (command "style" "romans" "romans.shx" "" "1.0" "" "" "" "")


  ;(princ "\nSelect all Line to Lable with it's Length:")
  (setq sset (ssget "_ALL"))
  (setq nset (sslength sset))
  (setq c 0)
  (setq d 1)
  (repeat nset
    (setq tempe (entget (ssname sset c)))
    (if	(= (strcase (cdr (assoc 0 tempe))) "LINE")
      (GLength tempe)
      (progn
	(princ (strcat "\n"
		       (rtos d 2 0)
		       " Selected Entity is not a Line.."
	       )
	)
	(setq d (+ d 1))
      )

    )
    (setq c (+ c 1))
  )

  (command "layer" "s" oldlayer "")
  (setvar "osmode" oldosnap)
  (setvar "cmdecho" 1)
  (princ)
)


(defun GLength (ent2)

  (if (= (strcase (cdr (assoc 0 ent2))) "LINE")
    (progn
      (setq p1x (nth 0 (cdr (assoc 10 ent2))))
      (setq p1y (nth 1 (cdr (assoc 10 ent2))))
      (setq p2x (nth 0 (cdr (assoc 11 ent2))))
      (setq p2y (nth 1 (cdr (assoc 11 ent2))))
      (setq len1 (sqrt (+ (expt (- p1x p2x) 2) (expt (- p1y p2y) 2))))
      (setq delx (- p2x p1x))
      (setq dely (- p2y p1y))
      (setq p1 (list p1x p1y))
      (setq p2 (list p2x p2y))
      (Command "Point" p1)
      (Command "Point" p2)
      (setq ang (angle p1 p2))
      (setq ang1 (/ (* ang 180) 3.1416))
      (setq ang2 ang)
      (if (< ang1 90)
	(setq ang2 (+ ang (/ 3.1416 2)))
	(setq ang2 (- ang (/ 3.1416 2)))
      )
      (setq pt (list (- (/ (+ p1x p2x) 2) (* 1.5 ts (cos ang2)))
		     (- (/ (+ p1y p2y) 2) (* 1.5 ts (sin ang2)))
	       )
      )

      (command "layer" "s" "Length" "")
      (command "text" "j" "mc" pt ts ang1 (rtos len1 2 3))
    )
  )
  (setq len1 0.0)
  (princ)
)




(defun C:GP ()

  (setvar "cmdecho" 0)
					;(if (= ts nil)
					;  (c:s_ts)
					;)
  (if (/= tset 1)
    (setq ts (* (getvar "VIEWSIZE") 0.018))
  )
  (setq oldosnap (getvar "osmode"))
  (command "osnap" "none")
  (setq oldlayer (getvar "clayer"))
  (if (= (tblsearch "layer" "NewLine") nil)
    (command "layer" "m" "NewLine" "c" "RED" "" "")
    (progn
      (command "layer" "t" "NewLine" "u" "NewLine" "")
      (command "layer" "s" "NewLine" "")
    )
  )
  (if (= (tblsearch "layer" "Ttext") nil)
    (command "layer" "m" "Ttext" "c" "RED" "" "")
    (progn
      (command "layer" "t" "Ttext" "u" "Ttext" "")
      (command "layer" "s" "Ttext" "")
    )
  )
  (command "style" "romans" "romans.shx" "" "1.0" "" "" "" "")
  (setvar "pdmode" 35)
  (setvar "pdsize" ts)

  (get_pt)
  (setq ptt1 pt)
  (command "layer" "s" oldlayer "")
  (ers "Ttext")


  (initget "Y N")
  (setq key1 (getkword "\nDo you want to Fix another Point <Y>/N:"))
  (if (= key1 nil)
    (setq key1 "Y")
  )
  (if (= key1 "N")
    (progn
      (prg)
      (quit)
    )
  )


  (get_pt)
  (setq ptt2 pt)
  (command "layer" "s" oldlayer "")
  (ers "Ttext")
  (prg "Ttext")

  (command "layer" "s" "NewLine" "")
  (command "line" ptt1 ptt2 "")
					;(command "line" pt pause)

					;(setq ang(angle ptt1 ptt2))
					;(setq ang1 (+ ang (/ 3.1416 2)))
					;(setq ang2 (- ang (/ 3.1416 2)))
					;(setq pt (list (/ (+ (nth 0 ptt1)  (nth 0 ptt2)) 2) (/ (+ (nth 1 ptt1)  (nth 1 ptt2)) 2)))
					;(setq pt1 (list (- (nth 0 pt) (* 0.1 (sin ang1))) (- (nth 1 pt) (* 0.1 (cos ang1)))))  	
					;(gap pt1)
					;(setq pt2 (list (- (nth 0 pt) (* 0.1 (sin ang2))) (- (nth 1 pt) (* 0.1 (cos ang2)))))
					;(gap pt2)


					;(command "point" pt1)
					;(command "point" pt2)

  (command "layer" "s" oldlayer "")
  (setq len1 0.0)
  (setq dist1 0.0)
  (setvar "osmode" oldosnap)
  (setvar "cmdecho" 1)
  (c:ga)
  (princ)
  (c:ga)
  (princ)
)


(defun C:Ers_L ()
  (Ers "Length")
  (princ)
)


(defun Ers (lay_name)

  (setq s_text (ssget "X" (list (cons 8 lay_name))))
  (princ)

  (if (/= s_text nil)
    (progn
      (Command "erase" s_text "")
    )
  )
)

(defun Prg (lay_name)

  (if (/= (tblsearch "layer" lay_name) nil)
    (if	(/= (strcase (getvar "clayer")) "TTEXT")
      (command "PURGE" "la" "Ttext" "" "Y")
    )
  )
  (princ)
)


(defun ali_lw_poly (ali)
  (setq a_x '())
  (setq a_y '())
  (setq n 1)
  (while (/= (nth n ali) nil)
    (if	(= (car (nth n ali)) 10)
      (progn (setq inpt_ali (cdr (nth n ali)))
	     (setq x1 (nth 0 inpt_ali))
	     (setq y1 (nth 1 inpt_ali))
	     (setq x0 (list x1))
	     (setq y0 (list y1))
	     (setq a_x (append a_x x0))
	     (setq a_y (append a_y y0))
      )
    )
    (setq n (1+ n))
  )
)


(defun get_pt ()

  (setq ent1 (entsel "\nSelect an edge to fix a Point:"))

  (while (= ent1 nil)
    (setq ent1 (entsel "\nSelect an edge to fix a Point:"))
  )

  (setq ent2 (entget (car ent1)))
  (if (= (strcase (cdr (assoc 0 ent2))) "LINE")
    (progn
      (setq p1x (nth 0 (cdr (assoc 10 ent2))))
      (setq p1y (nth 1 (cdr (assoc 10 ent2))))
      (setq p2x (nth 0 (cdr (assoc 11 ent2))))
      (setq p2y (nth 1 (cdr (assoc 11 ent2))))
      (setq len1 (sqrt (+ (expt (- p1x p2x) 2) (expt (- p1y p2y) 2))))
      (setq delx (- p2x p1x))
      (setq dely (- p2y p1y))
					;(setq p1 (list p1x p1y))
					;(setq p2 (list p2x p2y))
					;(setq ang(angle p1 p2))
					;(setq ang1(/ (* ang 180) 3.1416))
					;(setq pt (list (- (/ (+ p1x p2x) 2) (* 3.0 ts (sin ang))) (- (/ (+ p1y p2y) 2) (* 3.0 ts (cos ang)))))
					;(setq pt (list (/ (+ p1x p2x) 2) (/ (+ p1y p2y) 2)))
      (setq pt1 (list p1x (- p1y (* 2.0 ts))))
      (setq pt2 (list p2x (- p2y (* 2.0 ts))))
      (command "layer" "s" "Ttext" "")
      (command "text" "j" "mc" pt1 (* 2 ts) "0.0" "A")
      (command "text" "j" "mc" pt2 (* 2 ts) "0.0" "B")
      (princ
	(strcat "\nTotal Distance between A-B:" (rtos len1 2 3))
      )

      (initget "A B")
      (setq key1 (getkword "\nDistance from <A>/B:"))
      (if (= key1 nil)
	(setq key1 "A")
      )
      (if (= key1 "A")
	(progn
	  (setq dist1 (getreal "\nEnter Distance from A:"))
	  (check_dist len1 key1)
	  (setq	pt (list (+ p1x (* delx (/ dist1 len1)))
			 (+ p1y (* dely (/ dist1 len1)))
		   )
	  )
	  (command "layer" "s" "NewLine" "")
	  (command "point" pt)
	  (initget "Y N")
	  (setq
	    key2 (getkword "\nDo you want to Break the Line Y/<N>:")
	  )
	  (if (= key2 nil)
	    (setq key2 "N")
	  )
	  (if (= key2 "Y")
	    (command "break" ent1 "f" pt pt)
	  )

	)
	(progn
	  (setq dist1 (getreal "\nEnter Distance from B:"))
	  (check_dist len1 key1)
	  (setq	pt (list (- p2x (* delx (/ dist1 len1)))
			 (- p2y (* dely (/ dist1 len1)))
		   )
	  )
	  (command "layer" "s" "NewLine" "")
	  (command "point" pt)
	  (initget "Y N")
	  (setq
	    key2 (getkword "\nDo you want to Break the Line Y/<N>:")
	  )
	  (if (= key2 nil)
	    (setq key2 "N")
	  )
	  (if (= key2 "Y")
	    (command "break" ent1 "f" pt pt)
	  )

	)
      )
    )
    (progn
      (alert
	"The Selecte entity is not a Line. You should explode, if a Polyline is selected!"
      )
      (quit)
    )
  )
)


(defun check_dist (len1 side)
  (while (or (< dist1 0) (> dist1 len1))
    (princ
      (strcat "\nDistance should be inbetween 0-" (rtos len1 2 3))
    )
    (setq dist1 (getreal (strcat "\nEnter Distance from " side ":")))
  )
)

(defun C:SD1 ()
  (setq mtd 1)
  (sdm mtd)
  (princ)
)

(defun C:SD2 ()
  (setq mtd 2.0)
  (sdm mtd)
  (princ)
)

(defun c:SD3 ()
  (setq mtd 3.0)
  (sdm mtd)
  (princ)
)


(defun SDM (mtd)

					;(if (= ts nil)
					;  (c:s_ts)
					;)  
  (if (/= tset 1)
    (setq ts (* (getvar "VIEWSIZE") 0.025))
  )
  (setq oldlayer (getvar "clayer"))
  (setq oldosnap (getvar "osmode"))
  (if (= (tblsearch "layer" "NewLine") nil)
    (command "layer" "m" "NewLine" "c" "RED" "" "")
    (progn
      (command "layer" "t" "NewLine" "u" "NewLine" "")
      (command "layer" "s" "NewLine" "")
    )
  )

  (if (= (tblsearch "layer" "Ttext") nil)
    (command "layer" "m" "Ttext" "c" "RED" "" "")
    (progn
      (command "layer" "t" "Ttext" "u" "Ttext" "")
      (command "layer" "s" "Ttext" "")
    )
  )

  (if (= (tblsearch "layer" "Cut_Line") nil)
    (command "layer" "m" "Cut_Line" "c" "M" "" "")
    (progn
      (command "layer" "t" "Cut_Line" "u" "Ttext" "")
      (command "layer" "s" "Cut_Line" "")
    )
  )

  (command "layer" "s" "NewLine" "")
  (command "style" "romans" "romans.shx" "" "1.0" "" "" "" "")
  (princ
    "\nPick the 3/4 Points with first 2 points as Base Line in CW or CCW Direction.."
  )
  (setq pt1 (getpoint "\nClick for the First Point :"))
  (setq pt2 (getpoint "\nClick for the Second Point :"))
  (setq	bdist (sqrt (+ (expt (- (nth 0 pt1) (nth 0 pt2)) 2)
		       (expt (- (nth 1 pt1) (nth 1 pt2)) 2)
		    )
	      )
  )
  (If (= bdist 0.0)
    (progn
      (Alert
	"Length of Base Line cannot be 0.0 Hence cannot continue..!"
      )
      (exit_quit oldlayer oldosnap)
    )
  )

  (command "layer" "s" "NewLine" "")
  (command "line" pt1 pt2 "")
  (setq ang (angle pt1 pt2))
  (setq ang1 (/ (* ang 180) 3.1416))
  (setq ang2 ang)
  (if (< ang1 90)
    (setq ang2 (+ ang (/ 3.1416 2)))
    (setq ang2 (- ang (/ 3.1416 2)))
  )
  (setq	pt
	 (list (- (/ (+ (nth 0 pt1) (nth 0 pt2)) 2) (* 2.0 ts (cos ang2)))
	       (- (/ (+ (nth 1 pt1) (nth 1 pt2)) 2) (* 2.0 ts (sin ang2)))
	 )
  )

  (command "text" "j" "mc" pt ts ang1 "BASE LINE")
  (setq pt3 (getpoint "\nClick for the Third Point :"))
  (command "line" pt2 pt3 "")
  (setq pt4 (getpoint "\nClick for the Fourth Point :"))

  (if (/= pt4 nil)
    (progn
      (command "line" pt3 pt4 "")
      (command "line" pt4 pt1 "")
      (setq nos 4)
    )
    (progn
      (setq pt4 pt3)
      (command "line" pt3 pt1 "")
      (setq nos 3)
    )
  )
  (setq	ax '()
	ay '()
  )
  (setq ax (append ax (list (nth 0 pt1))))
  (setq ay (append ay (list (nth 1 pt1))))
  (setq ax (append ax (list (nth 0 pt2))))
  (setq ay (append ay (list (nth 1 pt2))))
  (setq ax (append ax (list (nth 0 pt3))))
  (setq ay (append ay (list (nth 1 pt3))))

  (if (= nos 4)
    (progn
      (setq ax (append ax (list (nth 0 pt4))))
      (setq ay (append ay (list (nth 1 pt4))))
      (setq p1x	(nth 0 ax)
	    p1y	(nth 0 ay)
      )
      (setq p2x	(nth 1 ax)
	    p2y	(nth 1 ay)
      )
      (setq p3x	(nth 2 ax)
	    p3y	(nth 2 ay)
      )
      (setq p4x	(nth 3 ax)
	    p4y	(nth 3 ay)
      )
      (setq dist1 (sqrt (+ (expt (- p1x p4x) 2) (expt (- p1y p4y) 2))))
      (setq dist2 (sqrt (+ (expt (- p2x p3x) 2) (expt (- p2y p3y) 2))))
      (if (or (= dist1 0.0) (= dist2 0.0))
	(progn
	  (Alert
	    "The Distance of 1st or 2nd leg cannot be 0.0 Hence cannot Subdivide with this Quardelateral!"
	  )
	  (exit_quit oldlayer oldosnap)
	)
      )
    )
    (progn

      (setq p1x	(nth 0 ax)
	    p1y	(nth 0 ay)
      )
      (setq p2x	(nth 1 ax)
	    p2y	(nth 1 ay)
      )
      (setq p3x	(nth 2 ax)
	    p3y	(nth 2 ay)
      )
      (setq dist1 (sqrt (+ (expt (- p1x p3x) 2) (expt (- p1y p3y) 2))))
      (setq dist2 (sqrt (+ (expt (- p2x p3x) 2) (expt (- p2y p3y) 2))))
      (if (or (= dist1 0.0) (= dist2 0.0))
	(progn
	  (Alert
	    "The Distance of 1st or 2nd leg cannot be 0.0 Hence cannot Subdivide with this Quardelateral!"
	  )
	  (exit_quit oldlayer oldosnap)
	)
      )
    )

  )

  (setq Ori_area (CalArea ax ay))
  (princ (strcat "\nTotal Area=" (rtos Ori_area 2 3)))

  (if (= nos 3)
    (progn
      (Divide3)
    )
    (progn
      (Divide4 mtd)
    )
  )

  (command "osnap" "none")
  (ers "NewLine")
  (command "layer" "s" "Cut_Line" "")
  (command "line" ppt1 ppt2 "")
					;(setq pt (list (/ (+ (nth 0 ax)  (nth 0 ppt2)) 2) (/ (+ (nth 1 ax)  (nth 1 ppt2)) 2)))
					;(command "layer" "s" "Area" "")
					;(command "text" "j" "mc" pt ts "0.0" (rtos (CalArea ax ay) 2 2))  
  (command "layer" "s" oldlayer "")
  (setvar "osmode" oldosnap)
  (C:ga)

					;(setq ang(angle ptt1 ptt2))
					;(setq ang1 (+ ang (/ 3.1416 2)))
					;(setq ang2 (- ang (/ 3.1416 2)))
					;(setq pt (list (/ (+ (nth 0 ptt1)  (nth 0 ptt2)) 2) (/ (+ (nth 1 ptt1)  (nth 1 ptt2)) 2)))
					;(setq pt1 (list (- (nth 0 pt) (* 0.1 (sin ang1))) (- (nth 1 pt) (* 0.1 (cos ang1)))))  	  	
					;(setq pt2 (list (- (nth 0 pt) (* 0.1 (sin ang2))) (- (nth 1 pt) (* 0.1 (cos ang2)))))

					;(command "point" pt1)
					;(command "point" pt2)

					;(command "layer" "s" oldlayer "")
					;  (setvar "cmdecho" 1)
  (princ)
					;(c:ga)
					;(princ)
)


(defun exit_quit (oldlay oldsnap)

  (if (/= oldlay nil)
    (command "layer" "s" oldlay "")
  )
  (if (/= oldsnap nil)
    (setvar "osmode" oldsnap)
  )
  (princ)
  (exit)
)


(defun Divide3 ()

  (princ
    (strcat "\nArea of Selected Triangle=" (rtos Ori_area 2 3))
  )

  (setq MArea (getreal "\nEnter the Area in Hand:"))
  (if (= MArea nil)
    (setq MArea 0.0)
  )
  (princ
    (strcat "\nMinimum Possible Area=" (rtos MArea 2 3))
  )
  (princ
    (strcat "\nMaximum Possible Area="
	    (rtos (+ MArea Ori_area) 2 3)
    )
  )
  (setq RArea (getreal "\nEnter the Required Area:"))
  (while (or (< RArea MArea) (> RArea (+ Ori_area MArea)))
    (princ
      (strcat "\nArea should be inbetween "
	      (rtos MArea 2 3)
	      "- "
	      (rtos (+ MArea Ori_area) 2 3)
      )
    )
    (setq RArea (getreal "\nEnter the Required Area:"))
  )

  (setq	p1x (nth 1 ax)
	p1y (nth 1 ay)
  )
  (setq	p2x (nth 2 ax)
	p2y (nth 2 ay)
  )
  (setq len1 (sqrt (+ (expt (- p1x p2x) 2) (expt (- p1y p2y) 2))))
  (setq cRatio (/ (+ Rarea MArea) (+ MArea Ori_Area)))

  (TCal3)
  (while (>= (abs (- (+ TArea MArea) RArea)) 0.0004)
    (setq cRatio (* CRatio (/ RArea (+ TArea MArea))))
    (TCal3)
  )

  (setq ppt1 (list (nth 0 ax) (nth 0 ay)))
  (setq ppt2 (list px py))
)


(defun TCal3 ()

  (setq px (+ p1x (* (- p2x p1x) cRatio)))
  (setq py (+ p1y (* (- p2y p1y) cRatio)))
  (setq ax (subst px (nth 2 ax) ax))
  (setq ay (subst py (nth 2 ay) ay))
  (setq TArea (CalArea ax ay))
)



(defun Divide4 (method)

  (setq	p1x (nth 0 ax)
	p1y (nth 0 ay)
  )
  (setq	p2x (nth 1 ax)
	p2y (nth 1 ay)
  )
  (setq	p3x (nth 2 ax)
	p3y (nth 2 ay)
  )
  (setq	p4x (nth 3 ax)
	p4y (nth 3 ay)
  )
  (setq dist1 (sqrt (+ (expt (- p1x p4x) 2) (expt (- p1y p4y) 2))))
  (setq dist2 (sqrt (+ (expt (- p2x p3x) 2) (expt (- p2y p3y) 2))))
  (cond
    ((= method 1)
     (princ "\nMethod 1 is selected!")
     (if (< dist1 dist2)
       (progn
	 (setq px (+ p2x (* (- p3x p2x) (/ dist1 dist2))))
	 (setq py (+ p2y (* (- p3y p2y) (/ dist1 dist2))))
	 (setq ax (subst px (nth 2 ax) ax))
	 (setq ay (subst py (nth 2 ay) ay))
       )
       (progn
	 (setq px (+ p1x (* (- p4x p1x) (/ dist2 dist1))))
	 (setq py (+ p1y (* (- p4y p1y) (/ dist2 dist1))))
	 (setq ax (subst px (nth 3 ax) ax))
	 (setq ay (subst py (nth 3 ay) ay))
       )
     )
     (setq Min_area 0.0
	   Max_area (calarea ax ay)
     )
    )

    ((= method 2)
     (princ "\nMethod 2 is selected!")
     (setq Min_area 0.0
	   Max_area Ori_area
     )
    )
    ((= method 3)
     (princ "\nMethod 3 is selected!")
     (setq a1 '()
	   b1 '()
	   a2 '()
	   b2 '()
     )
     (setq a1 (append a1 (list (nth 0 ax))))
     (setq a1 (append a1 (list (nth 0 ay))))
     (setq b1 (append b1 (list (nth 3 ax))))
     (setq b1 (append b1 (list (nth 3 ay))))
     (setq a2 (append a2 (list (nth 1 ax))))
     (setq a2 (append a2 (list (nth 1 ay))))
     (setq b2 (append b2 (list (nth 2 ax))))
     (setq b2 (append b2 (list (nth 2 ay))))

     (If (= (Is_CW ax ay) 1)

       (progn
	 (setq aa1 (GetBear_Deg b1 a1))
	 (setq aa2 (GetBear_Deg a1 a2))
	 (setq aa3 (GetBear_Deg a2 b2))
	 (if (< (GetBBear_Deg aa1) aa2)
	   (setq ang1 (- (+ (GetBBear_Deg aa1) 360) aa2))
	   (setq ang1 (- (GetBBear_Deg aa1) aa2))
	 )

	 (if (< (GetBBear_Deg aa2) aa3)
	   (setq ang2 (- (+ (GetBBear_Deg aa2) 360) aa3))
	   (setq ang2 (- (GetBBear_Deg aa2) aa3))
	 )
       )
       (progn
	 (setq aa1 (GetBear_Deg b1 a1))
	 (setq aa2 (GetBear_Deg a1 a2))
	 (setq aa3 (GetBear_Deg a2 b2))
	 (if (< aa2 (GetBBear_Deg aa1))
	   (setq ang1 (- (+ aa2 360) (GetBBear_Deg aa1)))
	   (setq ang1 (- aa2 (GetBBear_Deg aa1)))
	 )

	 (if (< aa3 (GetBBear_Deg aa2))
	   (setq ang2 (- (+ aa3 360) (GetBBear_Deg aa2)))
	   (setq ang2 (- aa3 (GetBBear_Deg aa2)))
	 )
       )
     )


     (If (or (> ang1 180) (> ang2 180))
       (progn
	 (alert
	   (strcat "The Given Area cannot be Divided with this Method."
		   " Choose a different method to Continue!"
	   )
	 )
	 (quit)
       )
       (progn
	 (If (< dist1 dist2)
	   (progn
	     (setq Dist dist1)
	     (If (< ang1 90)
	       (setq MaxPDist (* Dist (Sin (/ (* ang1 pi) 180))))
	       (setq MaxPDist (* Dist (Cos (/ (* (- ang1 90) pi) 180))))
	     )

	     (If (< ang2 90)
	       (progn
		 (setq
		   px (+ (nth 0 a2)
			 (* (- (nth 0 b2) (nth 0 a2))
			    (/ (/ MaxPDist (Sin (/ (* ang2 pi) 180)))
			       dist2
			    )
			 )
		      )
		 )
		 (setq
		   py (+ (nth 1 a2)
			 (* (- (nth 1 b2) (nth 1 a2))
			    (/ (/ MaxPDist (Sin (/ (* ang2 pi) 180)))
			       dist2
			    )
			 )
		      )
		 )
		 (setq ax (subst px (nth 2 ax) ax))
		 (setq ay (subst py (nth 2 ay) ay))
	       )
	       (progn
		 (setq px (+ (nth 0 a2)
			     (*	(- (nth 0 b2) (nth 0 a2))
				(/ (/ MaxPDist
				      (Cos (/ (* (- ang2 90) pi) 180))
				   )
				   dist2
				)
			     )
			  )
		 )
		 (setq py (+ (nth 1 a2)
			     (*	(- (nth 1 b2) (nth 1 a2))
				(/ (/ MaxPDist
				      (Cos (/ (* (- ang2 90) pi) 180))
				   )
				   dist2
				)
			     )
			  )
		 )
		 (setq ax (subst px (nth 2 ax) ax))
		 (setq ay (subst py (nth 2 ay) ay))
	       )
	     )
	   )
	   (progn
	     (setq Dist dist2)
	     (If (< ang2 90)
	       (setq MaxPDist (* Dist (Sin (/ (* ang2 pi) 180))))
	       (setq MaxPDist (* Dist (Cos (/ (* (- ang2 90) pi) 180))))
	     )

	     (If (< ang1 90)
	       (progn
		 (setq
		   px (+ (nth 0 a1)
			 (* (- (nth 0 b1) (nth 0 a1))
			    (/ (/ MaxPDist (Sin (/ (* ang1 pi) 180)))
			       dist1
			    )
			 )
		      )
		 )
		 (setq
		   py (+ (nth 1 a1)
			 (* (- (nth 1 b1) (nth 1 a1))
			    (/ (/ MaxPDist (Sin (/ (* ang1 pi) 180)))
			       dist1
			    )
			 )
		      )
		 )
		 (setq ax (subst px (nth 3 ax) ax))
		 (setq ay (subst py (nth 3 ay) ay))
	       )
	       (progn
		 (setq px (+ (nth 0 a1)
			     (*	(- (nth 0 b1) (nth 0 a1))
				(/ (/ MaxPDist
				      (Cos (/ (* (- ang1 90) pi) 180))
				   )
				   dist1
				)
			     )
			  )
		 )
		 (setq py (+ (nth 1 a1)
			     (*	(- (nth 1 b1) (nth 1 a1))
				(/ (/ MaxPDist
				      (Cos (/ (* (- ang1 90) pi) 180))
				   )
				   dist1
				)
			     )
			  )
		 )
		 (setq ax (subst px (nth 3 ax) ax))
		 (setq ay (subst py (nth 3 ay) ay))
	       )
	     )
	   )
	 )
	 (setq Min_area	0.0
	       Max_area	(calarea ax ay)
	 )
	 (If (> DistP MaxPDist)
	   (setq DistP (/ MaxPDist 2))
	 )
       )
     )
    )
    (T
     nil
    )

  )


  (princ
    (strcat "\nMaximum Area within Selected Quardlateral="
	    (rtos max_area 2 3)
    )
  )

  (setq MArea (getreal "\nEnter the Area in Hand:"))
  (if (= MArea nil)
    (setq MArea 0.0)
  )
  (princ
    (strcat "\nMinimum Possible Area=" (rtos MArea 2 3))
  )
  (princ
    (strcat "\nMaximum Possible Area="
	    (rtos (+ MArea max_area) 2 3)
    )
  )
  (setq RArea (getreal "\nEnter the Required Area:"))
  (while (or (<= RArea MArea) (>= RArea (+ max_area MArea)))
    (princ
      (strcat "\nArea should be inbetween "
	      (rtos MArea 2 3)
	      " - "
	      (rtos (+ MArea max_area) 2 3)
      )
    )
    (setq RArea (getreal "\nEnter the Required Area:"))
  )

  (Cond
    ((= method 1)
     (setq ADist (* dist1 (/ RArea (+ Max_area MArea))))
    )
    ((= method 2) (setq DistR (/ RArea (+ Max_area MArea))))
    ((= method 3)
     (setq PDist (* MaxPdist (/ RArea (+ Max_area MArea))))
    )
  )

  (TCal4 method)

  (while (>= (abs (- (+ TArea MArea) RArea)) 0.0004)
    (Cond
      ((= method 1)
       (setq ADist (* Adist (sqrt (/ RArea (+ TArea MArea)))))
      )
      ((= method 2)
       (setq DistR (* distR (sqrt (/ RArea (+ TArea MArea)))))
      )
      ((= method 3)
       (setq PDist (* Pdist (sqrt (/ RArea (+ TArea MArea)))))
      )
    )
    (TCal4 method)
  )

  (setq ppt1 (list px py))
  (setq ppt2 (list qx qy))
)



(defun TCal4 (method)

  (cond
    ((= method 1)
     (setq px (+ p1x (* (- p4x p1x) (/ ADist dist1))))
     (setq py (+ p1y (* (- p4y p1y) (/ ADist dist1))))
     (setq qx (+ p2x (* (- p3x p2x) (/ ADist dist2))))
     (setq qy (+ p2y (* (- p3y p2y) (/ ADist dist2))))
    )

    ((= method 2)
     (setq px (+ p1x (* (- p4x p1x) DistR)))
     (setq py (+ p1y (* (- p4y p1y) distR)))
     (setq qx (+ p2x (* (- p3x p2x) distR)))
     (setq qy (+ p2y (* (- p3y p2y) distR)))
    )

    ((= method 3)

     (If (< ang1 90)
       (progn
	 (setq px (+ (nth 0 a1)
		     (*	(- (nth 0 b1) (nth 0 a1))
			(/ (/ PDist (Sin (/ (* ang1 pi) 180))) dist1)
		     )
		  )
	 )
	 (setq py (+ (nth 1 a1)
		     (*	(- (nth 1 b1) (nth 1 a1))
			(/ (/ PDist (Sin (/ (* ang1 pi) 180))) dist1)
		     )
		  )
	 )
       )
       (progn
	 (setq px
		(+ (nth 0 a1)
		   (* (- (nth 0 b1) (nth 0 a1))
		      (/ (/ PDist (Cos (/ (* (- ang1 90) pi) 180))) dist1)
		   )
		)
	 )
	 (setq py
		(+ (nth 1 a1)
		   (* (- (nth 1 b1) (nth 1 a1))
		      (/ (/ PDist (Cos (/ (* (- ang1 90) pi) 180))) dist1)
		   )
		)
	 )
       )
     )

     (If (< ang2 90)
       (progn
	 (setq qx (+ (nth 0 a2)
		     (*	(- (nth 0 b2) (nth 0 a2))
			(/ (/ PDist (Sin (/ (* ang2 pi) 180))) dist2)
		     )
		  )
	 )
	 (setq qy (+ (nth 1 a2)
		     (*	(- (nth 1 b2) (nth 1 a2))
			(/ (/ PDist (Sin (/ (* ang2 pi) 180))) dist2)
		     )
		  )
	 )
       )
       (progn
	 (setq qx
		(+ (nth 0 a2)
		   (* (- (nth 0 b2) (nth 0 a2))
		      (/ (/ PDist (Cos (/ (* (- ang2 90) pi) 180))) dist2)
		   )
		)
	 )
	 (setq qy
		(+ (nth 1 a2)
		   (* (- (nth 1 b2) (nth 1 a2))
		      (/ (/ PDist (Cos (/ (* (- ang2 90) pi) 180))) dist2)
		   )
		)
	 )
       )
     )
    )
  )

  (setq ax (subst px (nth 3 ax) ax))
  (setq ay (subst py (nth 3 ay) ay))
  (setq ax (subst qx (nth 2 ax) ax))
  (setq ay (subst qy (nth 2 ay) ay))
  (setq TArea (CalArea ax ay))
)


(defun C:GData ()

  (Setvar "Cmdecho" 0)
  (setq ent1 (entsel "\nSelect a Line:"))
  (setq ent2 (entget (car ent1)))
  (if (= (strcase (cdr (assoc 0 ent2))) "LINE")
    (progn
      (setq p1x (nth 0 (cdr (assoc 10 ent2))))
      (setq p1y (nth 1 (cdr (assoc 10 ent2))))
      (setq p2x (nth 0 (cdr (assoc 11 ent2))))
      (setq p2y (nth 1 (cdr (assoc 11 ent2))))
      (setq len1 (sqrt (+ (expt (- p1x p2x) 2) (expt (- p1y p2y) 2))))
      (princ (strcat "\nStart X: "
		     (rtos p1x 2 3)
		     " Start Y: "
		     (rtos p1y 2 3)
	     )
      )
      (princ (strcat "\nEnd X: "
		     (rtos p2x 2 3)
		     " End Y: "
		     (rtos p2y 2 3)
	     )
      )
      (princ (strcat "\nTotal Length of Line : " (rtos len1 2 3)))
    )
    (princ "\nSelected Entity is not a Line!")
  )
  (Setvar "Cmdecho" 1)
  (princ)
)


(defun C:GD ()

  (Setvar "Cmdecho" 0)
  (setq file_name "C:\\Out.txt")
  (setq file_n (open file_name "w"))
  (close file_n)

  (princ "\nSelect Starting Line of Cutting Lines...")
  (setq ent1 (entsel "\nSelect a Line:"))
  (while (= ent1 nil)
    (princ "\nNo Line is selected!")
    (initget "Y N")
    (setq key3 (getkword "\nDo you want to Exit? Y/<N>:"))
    (if	(= key3 nil)
      (setq key3 "N")
    )
    (if	(= key3 "N")
      (setq ent1 (entsel "\nSelect a Line:"))
      (progn (princ "\n>>No Cutting Line is Selected!")
	     (exit)
      )
    )
  )
  (setq ent2 (entget (car ent1)))
  (while (/= (strcase (cdr (assoc 0 ent2))) "LINE")
    (princ "\nSelected Entity is not a Line!")
    (setq ent1 (entsel "\nSelect a Line:"))
    (while (= ent1 nil)
      (princ "\nNo Line is selected!")
      (initget "Y N")
      (setq key3 (getkword "\nDo you want to Exit? Y/<N>:"))
      (if (= key3 nil)
	(setq key3 "N")
      )
      (if (= key3 "N")
	(setq ent1 (entsel "\nSelect a Line:"))
	(progn (princ "\n>>Selection of Cutting Line is Complete!")
	       (exit)
	)
      )
    )
    (setq ent2 (entget (car ent1)))
  )

  (command "change" ent1 "" "p" "c" "y" "")
  (setq p1x (nth 0 (cdr (assoc 10 ent2))))
  (setq p1y (nth 1 (cdr (assoc 10 ent2))))
  (setq p2x (nth 0 (cdr (assoc 11 ent2))))
  (setq p2y (nth 1 (cdr (assoc 11 ent2))))
  (setq	p3x nil
	p3y nil
  )
  (setq	p4x nil
	p4y nil
  )
  (Setq Key2 "Y")
  (while (= key2 "Y")
    (initget "Y N")
    (setq key2 (getkword "\nDo you want select other Line <Y>/N:"))
    (if	(= key2 nil)
      (setq key2 "Y")
    )
    (if	(= key2 "Y")
      (progn
	(setq ent1 (entsel "\nSelect Other Line:"))
	(while (= ent1 nil)
	  (princ "\nNo Line is selected!")
	  (initget "Y N")
	  (setq key3 (getkword "\nDo you want to Exit? Y/<N>:"))
	  (if (= key3 nil)
	    (setq key3 "N")
	  )
	  (if (= key3 "N")
	    (setq ent1 (entsel "\nSelect a Line:"))
	    (progn (princ "\n>>Selection of Cutting Line is Complete!")
		   (exit)
	    )
	  )
	)
	(setq ent2 (entget (car ent1)))
	(while (/= (strcase (cdr (assoc 0 ent2))) "LINE")
	  (princ "\nSelected Entity is not a Line!")
	  (setq ent1 (entsel "\nSelect a Line:"))
	  (while (= ent1 nil)
	    (princ "\nNo Line is selected!")
	    (initget "Y N")
	    (setq key3 (getkword "\nDo you want to Exit? Y/<N>:"))
	    (if	(= key3 nil)
	      (setq key3 "N")
	    )
	    (if	(= key3 "N")
	      (setq ent1 (entsel "\nSelect a Line:"))
	      (progn (princ "\n>>Selection of Cutting Line is Complete!")
		     (exit)
	      )
	    )
	  )
	  (setq ent2 (entget (car ent1)))
	)

	(if (= p3x nil)
	  (progn
	    (setq p3x (nth 0 (cdr (assoc 10 ent2))))
	    (setq p3y (nth 1 (cdr (assoc 10 ent2))))
	    (setq p4x (nth 0 (cdr (assoc 11 ent2))))
	    (setq p4y (nth 1 (cdr (assoc 11 ent2))))
	    (if	(or (or	(and (= p1x p3x) (= p1y p3y))
			(and (= p1x p4x) (= p1y p4y))
		    )
		    (or	(and (= p2x p3x) (= p2y p3y))
			(and (= p2x p4x) (= p2y p4y))
		    )
		)
	      (if (or (and (and (= p1x p3x) (= p1y p3y))
			   (and (= p2x p4x) (= p2y p4y))
		      )
		      (and (and (= p1x p4x) (= p1y p4y))
			   (and (= p2x p3x) (= p2y p3y))
		      )
		  )
		(progn
		  (princ
		    "\nYou cannot select the same Line again, Please Select another Contineous Line."
		  )
		  (setq	p3x nil
			p3y nil
		  )
		  (setq	p4x nil
			p4y nil
		  )
		)
		(progn
		  (if (or (and (= p1x p3x) (= p1y p3y))
			  (and (= p1x p4x) (= p1y p4y))
		      )
		    (progn
		      (setq px p1x
			    py p1y
		      )
		      (setq p1x	p2x
			    p1y	p2y
		      )
		      (setq p2x	px
			    p2y	py
		      )

		      (if (and (= p2x p4x) (= p2y p4y))
			(progn
			  (setq	px p3x
				py p3y
			  )
			  (setq	p3x p4x
				p3y p4y
			  )
			  (setq	p4x px
				p4y py
			  )
			)
		      )
		    )

		    (if	(and (= p2x p4x) (= p2y p4y))
		      (progn
			(setq px p3x
			      py p3y
			)
			(setq p3x p4x
			      p3y p4y
			)
			(setq p4x px
			      p4y py
			)
		      )
		    )
		  )

		  (command "change" ent1 "" "p" "c" "y" "")
		  (setq file_n (open file_name "a"))
		  (princ (rtos p1x 2 12) file_n)
		  (princ "," file_n)
		  (princ (rtos p1y 2 12) file_n)
		  (princ "\n" file_n)
		  (princ (rtos p2x 2 12) file_n)
		  (princ "," file_n)
		  (princ (rtos p2y 2 12) file_n)
		  (princ "\n" file_n)
		  (princ (rtos p4x 2 12) file_n)
		  (princ "," file_n)
		  (princ (rtos p4y 2 12) file_n)
		  (close file_n)
		  (setq p1x p3x)
		  (setq p1y p3y)
		  (setq p2x p4x)
		  (setq p2y p4y)
		)
	      )
	      (progn
		(princ
		  "\nThe selcted Line is not contineous, Please Select a Contineous Line."
		)
		(setq p3x nil
		      p3y nil
		)
		(setq p4x nil
		      p4y nil
		)
	      )
	    )
	  )

	  (progn
	    (setq p3x (nth 0 (cdr (assoc 10 ent2))))
	    (setq p3y (nth 1 (cdr (assoc 10 ent2))))
	    (setq p4x (nth 0 (cdr (assoc 11 ent2))))
	    (setq p4y (nth 1 (cdr (assoc 11 ent2))))
	    (if	(or (and (and (= p1x p3x) (= p1y p3y))
			 (and (= p2x p4x) (= p2y p4y))
		    )
		    (and (and (= p1x p4x) (= p1y p4y))
			 (and (= p2x p3x) (= p2y p3y))
		    )
		)
	      (progn
		(princ
		  "\nYou cannot select the same Line again, The selcted Line is not contineous, Please Select another Contineous Line."
		)
	      )

	      (if (or (and (= p2x p3x) (= p2y p3y))
		      (and (= p2x p4x) (= p2y p4y))
		  )
		(progn
		  (if (and (= p2x p4x) (= p2y p4y))
		    (progn
		      (setq px p3x
			    py p3y
		      )
		      (setq p3x	p4x
			    p3y	p4y
		      )
		      (setq p4x	px
			    p4y	py
		      )
		    )
		  )
		  (command "change" ent1 "" "p" "c" "y" "")
		  (setq file_n (open file_name "a"))
		  (princ "\n" file_n)
		  (princ (rtos p4x 2 12) file_n)
		  (princ "," file_n)
		  (princ (rtos p4y 2 12) file_n)
		  (close file_n)
		  (setq p1x p3x)
		  (setq p1y p3y)
		  (setq p2x p4x)
		  (setq p2y p4y)
		)
		(princ
		  "\nThe selcted Line is not contineous, Please Select a Contineouse Line."
		)
	      )
	    )
	  )
	)
      )
      (progn
	(if (= p3x nil)
	  (progn
	    (setq file_n (open file_name "a"))
	    (princ (rtos p1x 2 12) file_n)
	    (princ "," file_n)
	    (princ (rtos p1y 2 12) file_n)
	    (princ "\n" file_n)
	    (princ (rtos p2x 2 12) file_n)
	    (princ "," file_n)
	    (princ (rtos p2y 2 12) file_n)
	    (close file_n)
	  )
	)
	(princ "\n>>Selection of Cutting Line is Complete!")
      )
    )
  )
  (Setvar "Cmdecho" 1)
  (princ)
)


(defun CalArea (x y / n g h i)
  (setq n (length x))
  (setq g 0.0)
  (setq i 0)
  (while (/= i n)
    (if	(/= i (- n 1))
      (setq h (* (- (nth (+ i 1) x) (nth i x))
		 (+ (nth (+ i 1) y) (nth i y))
	      )
      )
      (setq h (* (- (nth 0 x) (nth (- n 1) x))
		 (+ (nth 0 y) (nth (- n 1) y))
	      )
      )
    )

    (setq g (+ g h))
    (setq i (+ i 1))

  )
  (/ (Abs g) 2)
)

(defun GetBear_Deg (a b /)

  (setq del_x (- (nth 0 b) (nth 0 a)))
  (setq del_y (- (nth 1 b) (nth 1 a)))
  (if (= del_x 0.0)
    (setq Bear_deg 90)
    (progn
      (setq bear (Atan (/ (Abs del_y) (Abs del_x))))
      (setq Bear_Deg (/ (* 180 bear) pi))
    )
  )

  (if (and (>= del_x 0.0) (>= del_y 0.0))
    (- 90 Bear_Deg)
    (if	(and (>= del_x 0.0) (< del_y 0.0))
      (+ 90 Bear_Deg)
      (if (and (< del_x 0.0) (< del_y 0.0))
	(- 270 Bear_Deg)
	(if (and (< del_x 0.0) (>= del_y 0.0))
	  (+ 270 Bear_Deg)
	)
      )
    )
  )
)


(defun GetBBear_Deg (Bear_Deg)
  (if (> Bear_Deg 180)
    (- Bear_Deg 180)
    (+ Bear_Deg 180)
  )
)


(defun IS_CW (x y)

  (setq nop (length x))
  (setq y_min (nth 0 y))
  (setq i 0)
  (setq q 0)
  (while (/= i nop)
    (if	(> y_min (nth i y))
      (progn
	(setq y_min (nth i y))
	(setq q i)
      )
    )
    (setq i (+ i 1))
  )


  (If (= q 0)
    (setq p (- nop 1)
	  r 1
    )
    (If	(= q (- nop 1))
      (setq p (- nop 2)
	    r 0
      )
      (setq p (- q 1)
	    r (+ q 1)
      )
    )
  )

  (setq	a '()
	b '()
	c '()
  )
  (setq a (append a (list (nth p x))))
  (setq a (append a (list (nth p y))))
  (setq b (append b (list (nth q x))))
  (setq b (append b (list (nth q y))))
  (setq c (append c (list (nth r x))))
  (setq c (append c (list (nth r y))))

  (setq angle1 (GetBear_Deg b a))
  (setq angle2 (GetBear_Deg b c))

  (if (Or (and (>= (nth 0 a) (nth 0 b)) (>= (nth 0 c) (nth 0 b)))
	  (and (<= (nth 0 a) (nth 0 b)) (<= (nth 0 c) (nth 0 b)))
      )
    (If	(> angle1 angle2)
      1
      0
    )
    (If	(> angle2 angle1)
      1
      0
    )
  )
)


(defun C:GAL ()

  (Setvar "Cmdecho" 0)
  (setq oldlayer (getvar "clayer"))
  (if (= (tblsearch "layer" "T_Layer") nil)
    (command "layer" "m" "T_Layer" "c" "Y" "" "")
    (progn
      (command "layer" "t" "T_Layer" "u" "T_Layer" "")
      (command "layer" "s" "T_Layer" "")
    )
  )
  (if (= (tblsearch "layer" "Cut_Line") nil)
    (command "layer" "m" "Cut_Line" "c" "M" "" "")
    (progn
      (command "layer" "t" "Cut_Line" "u" "Ttext" "")
      (command "layer" "s" "Cut_Line" "")
    )
  )


  (Setq nLine 0)
  (setq fArea 0.0)
  (setq	ax '()
	ay '()
  )
  (setq ent1 (entsel "\nSelect Starting Line to Find Area:"))
  (while (= ent1 nil)
    (princ "\nNo Line is selected!")
    (initget "Y N")
    (setq key3 (getkword "\nDo you want to Exit? Y/<N>:"))
    (if	(= key3 nil)
      (setq key3 "N")
    )
    (if	(= key3 "N")
      (setq ent1 (entsel "\nSelect Starting Line to Find Area:"))
      (progn
	(princ "\n>>No Line is Selected!")
	(exit_quit_Gal oldlayer)
      )
    )
  )

  (setq ent2 (entget (car ent1)))
  (while (/= (strcase (cdr (assoc 0 ent2))) "LINE")
    (princ "\nSelected Entity is not a Line!")
    (setq ent1 (entsel "\nSelect a Line:"))
    (while (= ent1 nil)
      (princ "\nNo Line is selected!")
      (initget "Y N")
      (setq key3 (getkword "\nDo you want to Exit? Y/<N>:"))
      (if (= key3 nil)
	(setq key3 "N")
      )
      (if (= key3 "N")
	(setq ent1 (entsel "\nSelect a Line:"))
	(exit_quit_Gal oldlayer)
      )
    )
    (setq ent2 (entget (car ent1)))
  )

  (setq ELay_name (cdr (assoc 8 ent2)))
  (command "change" ent1 "" "p" "la" "T_layer" "")
  (setq p1x (nth 0 (cdr (assoc 10 ent2))))
  (setq p1y (nth 1 (cdr (assoc 10 ent2))))
  (setq p2x (nth 0 (cdr (assoc 11 ent2))))
  (setq p2y (nth 1 (cdr (assoc 11 ent2))))
  (setq	p3x nil
	p3y nil
  )
  (setq	p4x nil
	p4y nil
  )


  (Setq Key2 "Y")
  (while (= key2 "Y")
    (if	(>= nLine 1)
      (progn
	(initget "Y N")
	(setq key2 (getkword "\nDo you want select other Line <Y>/N:"))
	(if (= key2 nil)
	  (setq key2 "Y")
	)
      )
    )
    (if	(= key2 "Y")
      (progn
	(setq ent1 (entsel "\nSelect Other Line:"))
	(while (= ent1 nil)
	  (princ "\nNo Line is selected!")
	  (initget "Y N")
	  (setq key3 (getkword "\nDo you want to Exit? Y/<N>:"))
	  (if (= key3 nil)
	    (setq key3 "N")
	  )
	  (if (= key3 "N")
	    (setq ent1 (entsel "\nSelect a Line:"))
	    (progn
	      (if (> (length ax) 0)
		(progn
		  (setq nn (- (length ax) 1))
		  (setq pq1 (list (nth 0 ax) (nth 0 ay)))
		  (setq pq2 (list (nth nn ax) (nth nn ay)))
		  (Command "Line" pq1 pq2 "")
		)
	      )
	      (princ (strcat "\n>>Total Area of Selected Lines ="
			     (rtos farea 2 3)
		     )
	      )
	      (exit_quit_Gal oldlayer)
	    )
	  )
	)
	(setq ent2 (entget (car ent1)))
	(while (/= (strcase (cdr (assoc 0 ent2))) "LINE")
	  (princ "\nSelected Entity is not a Line!")
	  (setq ent1 (entsel "\nSelect a Line:"))
	  (while (= ent1 nil)
	    (princ "\nNo Line is selected!")
	    (initget "Y N")
	    (setq key3 (getkword "\nDo you want to Exit? Y/<N>:"))
	    (if	(= key3 nil)
	      (setq key3 "N")
	    )
	    (if	(= key3 "N")
	      (setq ent1 (entsel "\nSelect a Line:"))
	      (progn
		(if (> (length ax) 0)
		  (progn
		    (setq nn (- (length ax) 1))
		    (setq pq1 (list (nth 0 ax) (nth 0 ay)))
		    (setq pq2 (list (nth nn ax) (nth nn ay)))
		    (Command "Line" pq1 pq2 "")
		  )
		)
		(princ (strcat "\n>>Total Area of Selected Lines ="
			       (rtos fArea 2 3)
		       )
		)
		(exit_quit_Gal oldlayer)
	      )
	    )
	  )
	  (setq ent2 (entget (car ent1)))
	)

	(if (= p3x nil)
	  (progn
	    (setq p3x (nth 0 (cdr (assoc 10 ent2))))
	    (setq p3y (nth 1 (cdr (assoc 10 ent2))))
	    (setq p4x (nth 0 (cdr (assoc 11 ent2))))
	    (setq p4y (nth 1 (cdr (assoc 11 ent2))))
	    (if	(or (or	(and (= p1x p3x) (= p1y p3y))
			(and (= p1x p4x) (= p1y p4y))
		    )
		    (or	(and (= p2x p3x) (= p2y p3y))
			(and (= p2x p4x) (= p2y p4y))
		    )
		)
	      (if (or (and (and (= p1x p3x) (= p1y p3y))
			   (and (= p2x p4x) (= p2y p4y))
		      )
		      (and (and (= p1x p4x) (= p1y p4y))
			   (and (= p2x p3x) (= p2y p3y))
		      )
		  )
		(progn
		  (princ
		    "\nYou cannot select the same Line again, Please Select another Contineous Line."
		  )
		  (setq	p3x nil
			p3y nil
		  )
		  (setq	p4x nil
			p4y nil
		  )
		)
		(progn
		  (if (or (and (= p1x p3x) (= p1y p3y))
			  (and (= p1x p4x) (= p1y p4y))
		      )
		    (progn
		      (setq px p1x
			    py p1y
		      )
		      (setq p1x	p2x
			    p1y	p2y
		      )
		      (setq p2x	px
			    p2y	py
		      )

		      (if (and (= p2x p4x) (= p2y p4y))
			(progn
			  (setq	px p3x
				py p3y
			  )
			  (setq	p3x p4x
				p3y p4y
			  )
			  (setq	p4x px
				p4y py
			  )
			)
		      )
		    )

		    (if	(and (= p2x p4x) (= p2y p4y))
		      (progn
			(setq px p3x
			      py p3y
			)
			(setq p3x p4x
			      p3y p4y
			)
			(setq p4x px
			      p4y py
			)
		      )
		    )
		  )

		  (command "change" ent1 "" "p" "la" "T_layer" "")

		  (setq ax (append ax (list p1x)))
		  (setq ay (append ay (list p1y)))

		  (setq ax (append ax (list p2x)))
		  (setq ay (append ay (list p2y)))

		  (setq ax (append ax (list p4x)))
		  (setq ay (append ay (list p4y)))
		  (setq nLine (+ nline 1))
		  (setq fArea (Calarea ax ay))
		  (princ (strcat "\n>>Total Area of Selected Lines ="
				 (rtos farea 2 3)
			 )
		  )

		  (setq p1x p3x)
		  (setq p1y p3y)
		  (setq p2x p4x)
		  (setq p2y p4y)
		)
	      )
	      (progn
		(princ
		  "\nThe selcted Line is not contineous, Please Select a Contineous Line."
		)
		(setq p3x nil
		      p3y nil
		)
		(setq p4x nil
		      p4y nil
		)
	      )
	    )
	  )

	  (progn
	    (setq p3x (nth 0 (cdr (assoc 10 ent2))))
	    (setq p3y (nth 1 (cdr (assoc 10 ent2))))
	    (setq p4x (nth 0 (cdr (assoc 11 ent2))))
	    (setq p4y (nth 1 (cdr (assoc 11 ent2))))
	    (if	(or (and (and (= p1x p3x) (= p1y p3y))
			 (and (= p2x p4x) (= p2y p4y))
		    )
		    (and (and (= p1x p4x) (= p1y p4y))
			 (and (= p2x p3x) (= p2y p3y))
		    )
		)
	      (progn
		(princ
		  "\nYou cannot select the same Line again, The selcted Line is not contineous, Please Select another Contineous Line."
		)
	      )

	      (if (or (and (= p2x p3x) (= p2y p3y))
		      (and (= p2x p4x) (= p2y p4y))
		  )
		(progn
		  (if (and (= p2x p4x) (= p2y p4y))
		    (progn
		      (setq px p3x
			    py p3y
		      )
		      (setq p3x	p4x
			    p3y	p4y
		      )
		      (setq p4x	px
			    p4y	py
		      )
		    )
		  )
		  (command "change" ent1 "" "p" "la" "T_layer" "")
		  (setq ax (append ax (list p4x)))
		  (setq ay (append ay (list p4y)))
		  (setq nLine (+ nline 1))
		  (setq fArea (Calarea ax ay))
		  (princ (strcat "\n>>Total Area of Selected Lines ="
				 (rtos farea 2 3)
			 )
		  )
		  (setq p1x p3x)
		  (setq p1y p3y)
		  (setq p2x p4x)
		  (setq p2y p4y)
		)

		(princ
		  "\nThe selcted Line is not contineous, Please Select a Contineouse Line."
		)
	      )
	    )
	  )
	)
      )
      (progn
	(princ "\n>>Selection of Line for Area is Complete!")
	(if (> (length ax) 0)
	  (progn
	    (setq nn (- (length ax) 1))
	    (setq pq1 (list (nth 0 ax) (nth 0 ay)))
	    (setq pq2 (list (nth nn ax) (nth nn ay)))
	    (Command "Line" pq1 pq2 "")
	  )
	)

	(if (/= ELay_name nil)
	  (change_Layer "T_Layer" ELay_name)
	)
	(princ (strcat "\n>>Total Area of Selected Lines ="
		       (rtos farea 2 3)
	       )
	)
	(C:GA)
      )
    )
  )
  (Setvar "Cmdecho" 1)
  (princ)
)


(defun C:SDP ()

  (Setvar "Cmdecho" 0)
  (setq oldlayer (getvar "clayer"))
  (if (= (tblsearch "layer" "T_Layer") nil)
    (command "layer" "m" "T_Layer" "c" "Y" "" "")
    (progn
      (command "layer" "t" "T_Layer" "u" "T_Layer" "")
      (command "layer" "s" "T_Layer" "")
    )
  )

  (if (= (tblsearch "layer" "Cut_Line") nil)
    (command "layer" "m" "Cut_Line" "c" "M" "" "")
    (progn
      (command "layer" "t" "Cut_Line" "u" "Ttext" "")
      (command "layer" "s" "Cut_Line" "")
    )
  )

  (Setq nLine 0)
  (setq fArea 0.0)
  (setq fAreaL 0.0)
  (setq	ax '()
	ay '()
  )
  (setq ent1 (entsel "\nSelect Starting Line to Find Area:"))
  (while (= ent1 nil)
    (princ "\nNo Line is selected!")
    (initget "Y N")
    (setq key3 (getkword "\nDo you want to Exit? Y/<N>:"))
    (if	(= key3 nil)
      (setq key3 "N")
    )
    (if	(= key3 "N")
      (setq ent1 (entsel "\nSelect Starting Line to Find Area:"))
      (progn
	(princ "\n>>No Line is Selected!")
	(exit_quit_Gal oldlayer)
      )
    )
  )

  (setq ent2 (entget (car ent1)))
  (while (/= (strcase (cdr (assoc 0 ent2))) "LINE")
    (princ "\nSelected Entity is not a Line!")
    (setq ent1 (entsel "\nSelect a Line:"))
    (while (= ent1 nil)
      (princ "\nNo Line is selected!")
      (initget "Y N")
      (setq key3 (getkword "\nDo you want to Exit? Y/<N>:"))
      (if (= key3 nil)
	(setq key3 "N")
      )
      (if (= key3 "N")
	(setq ent1 (entsel "\nSelect a Line:"))
	(exit_quit_Gal oldlayer)
      )
    )
    (setq ent2 (entget (car ent1)))
  )

  (setq ELay_name (cdr (assoc 8 ent2)))
  (command "change" ent1 "" "p" "la" "T_layer" "")
  (setq p1x (nth 0 (cdr (assoc 10 ent2))))
  (setq p1y (nth 1 (cdr (assoc 10 ent2))))
  (setq p2x (nth 0 (cdr (assoc 11 ent2))))
  (setq p2y (nth 1 (cdr (assoc 11 ent2))))

  (setq	p3x nil
	p3y nil
  )
  (setq	p4x nil
	p4y nil
  )

  (Setq Key2 "Y")
  (while (= key2 "Y")
    (if	(>= nLine 1)
      (progn
	(initget "Y N")
	(setq key2 (getkword "\nDo you want select other Line <Y>/N:"))
	(if (= key2 nil)
	  (setq key2 "Y")
	)
      )
    )
    (if	(= key2 "Y")
      (progn
	(setq ent1 (entsel "\nSelect Other Line:"))
	(while (= ent1 nil)
	  (princ "\nNo Line is selected!")
	  (initget "Y N")
	  (setq key3 (getkword "\nDo you want to Exit? Y/<N>:"))
	  (if (= key3 nil)
	    (setq key3 "N")
	  )
	  (if (= key3 "N")
	    (setq ent1 (entsel "\nSelect a Line:"))
	    (progn
	      (if (> (length ax) 0)
		(progn
		  (setq nn (- (length ax) 1))
					;(setq pq1 (list (nth 0 ax) (nth 0 ay)))
					;(setq pq2 (list (nth nn ax) (nth nn ay)))
					;(Command "Line" pq1 pq2 "")
		  (princ (strcat "\n>>Minimum Area ="
				 (rtos fareaL 2 3)
				 "   Maximum Area ="
				 (rtos farea 2 3)
			 )
		  )
		  (setq FPt1 (list (nth 0 ax) (nth 0 ay)))
		  (setq FPt2 (list (nth (- nn 1) ax) (nth (- nn 1) ay)))
		  (setq FPt3 (list (nth nn ax) (nth nn ay)))
		  (Set_Area fAreal fArea Fpt1 Fpt2 Fpt3)
		)
	      )
	      (exit_quit_Gal oldlayer)
	    )
	  )
	)
	(setq ent2 (entget (car ent1)))

	(while (/= (strcase (cdr (assoc 0 ent2))) "LINE")
	  (princ "\nSelected Entity is not a Line!")
	  (setq ent1 (entsel "\nSelect a Line:"))
	  (while (= ent1 nil)
	    (princ "\nNo Line is selected!")
	    (initget "Y N")
	    (setq key3 (getkword "\nDo you want to Exit? Y/<N>:"))
	    (if	(= key3 nil)
	      (setq key3 "N")
	    )
	    (if	(= key3 "N")
	      (setq ent1 (entsel "\nSelect a Line:"))
	      (progn
		(if (> (length ax) 0)
		  (progn
		    (setq nn (- (length ax) 1))
					;(setq pq1 (list (nth 0 ax) (nth 0 ay)))
					;(setq pq2 (list (nth nn ax) (nth nn ay)))
					;(Command "Line" pq1 pq2 "")
					;(princ (strcat "\n>>Total Area of Selected Lines ="
					;	       (rtos fArea 2 3)
					;   )
					;)
		    (princ (strcat "\n>>Minimum Area ="
				   (rtos fareaL 2 3)
				   "   Maximum Area ="
				   (rtos farea 2 3)
			   )
		    )
		    (setq FPt1 (list (nth 0 ax) (nth 0 ay)))
		    (setq
		      FPt2 (list (nth (- nn 1) ax) (nth (- nn 1) ay))
		    )
		    (setq FPt3 (list (nth nn ax) (nth nn ay)))
		    (Set_Area fAreal fArea Fpt1 Fpt2 Fpt3)
		  )
		)

		(exit_quit_Gal oldlayer)
	      )
	    )
	  )
	  (setq ent2 (entget (car ent1)))
	)

	(if (= p3x nil)
	  (progn
	    (setq p3x (nth 0 (cdr (assoc 10 ent2))))
	    (setq p3y (nth 1 (cdr (assoc 10 ent2))))
	    (setq p4x (nth 0 (cdr (assoc 11 ent2))))
	    (setq p4y (nth 1 (cdr (assoc 11 ent2))))
	    (if	(or (or	(and (= p1x p3x) (= p1y p3y))
			(and (= p1x p4x) (= p1y p4y))
		    )
		    (or	(and (= p2x p3x) (= p2y p3y))
			(and (= p2x p4x) (= p2y p4y))
		    )
		)
	      (if (or (and (and (= p1x p3x) (= p1y p3y))
			   (and (= p2x p4x) (= p2y p4y))
		      )
		      (and (and (= p1x p4x) (= p1y p4y))
			   (and (= p2x p3x) (= p2y p3y))
		      )
		  )
		(progn
		  (princ
		    "\nYou cannot select the same Line again, Please Select another Contineous Line."
		  )
		  (setq	p3x nil
			p3y nil
		  )
		  (setq	p4x nil
			p4y nil
		  )
		)
		(progn
		  (if (or (and (= p1x p3x) (= p1y p3y))
			  (and (= p1x p4x) (= p1y p4y))
		      )
		    (progn
		      (setq px p1x
			    py p1y
		      )
		      (setq p1x	p2x
			    p1y	p2y
		      )
		      (setq p2x	px
			    p2y	py
		      )

		      (if (and (= p2x p4x) (= p2y p4y))
			(progn
			  (setq	px p3x
				py p3y
			  )
			  (setq	p3x p4x
				p3y p4y
			  )
			  (setq	p4x px
				p4y py
			  )
			)
		      )
		    )

		    (if	(and (= p2x p4x) (= p2y p4y))
		      (progn
			(setq px p3x
			      py p3y
			)
			(setq p3x p4x
			      p3y p4y
			)
			(setq p4x px
			      p4y py
			)
		      )
		    )
		  )

		  (command "change" ent1 "" "p" "la" "T_layer" "")

		  (setq ax (append ax (list p1x)))
		  (setq ay (append ay (list p1y)))

		  (setq ax (append ax (list p2x)))
		  (setq ay (append ay (list p2y)))

		  (setq ax (append ax (list p4x)))
		  (setq ay (append ay (list p4y)))
		  (setq fAreaL fArea)
		  (setq fArea (Calarea ax ay))
		  (Setq nLine 1)
		  (princ (strcat "\n>>Minimum Area ="
				 (rtos fareaL 2 3)
				 "   Maximum Area ="
				 (rtos farea 2 3)
			 )
		  )
					;(princ (strcat "\n>>Total Area of Selected Lines ="
					;			 (rtos farea 2 3)
					;		 )
					;		  )

		  (setq p1x p3x)
		  (setq p1y p3y)
		  (setq p2x p4x)
		  (setq p2y p4y)
		)
	      )
	      (progn
		(princ
		  "\nThe selcted Line is not contineous, Please Select a Contineous Line."
		)
		(setq p3x nil
		      p3y nil
		)
		(setq p4x nil
		      p4y nil
		)
	      )
	    )
	  )

	  (progn
	    (setq p3x (nth 0 (cdr (assoc 10 ent2))))
	    (setq p3y (nth 1 (cdr (assoc 10 ent2))))
	    (setq p4x (nth 0 (cdr (assoc 11 ent2))))
	    (setq p4y (nth 1 (cdr (assoc 11 ent2))))
	    (if	(or (and (and (= p1x p3x) (= p1y p3y))
			 (and (= p2x p4x) (= p2y p4y))
		    )
		    (and (and (= p1x p4x) (= p1y p4y))
			 (and (= p2x p3x) (= p2y p3y))
		    )
		)
	      (progn
		(princ
		  "\nYou cannot select the same Line again, The selcted Line is not contineous, Please Select another Contineous Line."
		)
	      )

	      (if (or (and (= p2x p3x) (= p2y p3y))
		      (and (= p2x p4x) (= p2y p4y))
		  )
		(progn
		  (if (and (= p2x p4x) (= p2y p4y))
		    (progn
		      (setq px p3x
			    py p3y
		      )
		      (setq p3x	p4x
			    p3y	p4y
		      )
		      (setq p4x	px
			    p4y	py
		      )
		    )
		  )
		  (command "change" ent1 "" "p" "la" "T_layer" "")
		  (setq ax (append ax (list p4x)))
		  (setq ay (append ay (list p4y)))
		  (setq nLine (+ nline 1))
		  (setq fAreaL fArea)
		  (setq fArea (Calarea ax ay))
		  (princ (strcat "\n>>Minimum Area ="
				 (rtos fareaL 2 3)
				 "   Maximum Area ="
				 (rtos farea 2 3)
			 )
		  )
					;(princ (strcat "\n>>Total Area of Selected Lines ="
					;			 (rtos farea 2 3)
					;			 )
					;)
		  (setq p1x p3x)
		  (setq p1y p3y)
		  (setq p2x p4x)
		  (setq p2y p4y)
		)

		(princ
		  "\nThe selcted Line is not contineous, Please Select a Contineouse Line."
		)
	      )
	    )
	  )
	)
      )

      (progn
	(if (> (length ax) 0)
	  (progn
	    (princ "\n>>Selection of Line is Complete!")
	    (setq nn (- (length ax) 1))
					;(setq pq1 (list (nth 0 ax) (nth 0 ay)))
					;(setq pq2 (list (nth nn ax) (nth nn ay)))
					;(Command "Line" pq1 pq2 "")
					;(princ (strcat "\n>>Total Area of Selected Lines ="
					;(rtos farea 2 3))
					;)	    
	    (setq FPt1 (list (nth 0 ax) (nth 0 ay)))
	    (setq FPt2 (list (nth (- nn 1) ax) (nth (- nn 1) ay)))
	    (setq FPt3 (list (nth nn ax) (nth nn ay)))
	    (Set_Area fAreal fArea Fpt1 Fpt2 Fpt3)
	  )
	  (princ "\n>>Not enough lines were selected!")
	)
      )
    )
  )
  (Setvar "Cmdecho" 1)
  (princ)
)


(defun Set_Area	(MinArea MaxArea pt1 pt2 pt3)

  (setq	bdist (sqrt (+ (expt (- (nth 0 pt1) (nth 0 pt2)) 2)
		       (expt (- (nth 1 pt1) (nth 1 pt2)) 2)
		    )
	      )
  )
  (If (= bdist 0.0)
    (progn
      (Alert
	"Length of Base Line cannot be 0.0 Hence cannot continue..!"
      )
      (exit)
    )
  )

  (setq	ax '()
	ay '()
  )
  (setq ax (append ax (list (nth 0 pt1))))
  (setq ay (append ay (list (nth 1 pt1))))
  (setq ax (append ax (list (nth 0 pt2))))
  (setq ay (append ay (list (nth 1 pt2))))
  (setq ax (append ax (list (nth 0 pt3))))
  (setq ay (append ay (list (nth 1 pt3))))

  (setq	p1x (nth 0 ax)
	p1y (nth 0 ay)
  )
  (setq	p2x (nth 1 ax)
	p2y (nth 1 ay)
  )
  (setq	p3x (nth 2 ax)
	p3y (nth 2 ay)
  )
  (setq dist1 (sqrt (+ (expt (- p1x p3x) 2) (expt (- p1y p3y) 2))))
  (setq dist2 (sqrt (+ (expt (- p2x p3x) 2) (expt (- p2y p3y) 2))))
  (if (or (= dist1 0.0) (= dist2 0.0))
    (progn
      (Alert
	"The Distance of 1st or 2nd leg cannot be 0.0 Hence cannot Subdivide!"
      )
      (exit)
    )
  )


  (Divide33 MinArea MaxArea)
  (if (/= ELay_name nil)
    (change_Layer "T_Layer" ELay_name)
  )
  (command "osnap" "none")
  (command "layer" "s" "Cut_Line" "")
  (command "line" ppt1 ppt2 "")
  (command "layer" "s" oldlayer "")
  (C:ga)
  (princ)
)

(defun Divide33	(MinArea MaxArea)

  (princ
    (strcat "\n>>Minimum Possible Area=" (rtos MinArea 2 3))
  )
  (princ
    (strcat "\n>>Maximum Possible Area=" (rtos MaxArea 2 3))
  )

  (setq RArea (getreal "\nEnter the Required Area:"))
  (while (or (< RArea MinArea) (> RArea MaxArea))
    (princ
      (strcat "\nArea should be inbetween "
	      (rtos MinArea 2 3)
	      " -"
	      (rtos MaxArea 2 3)
      )
    )
    (setq RArea (getreal "\nEnter the Required Area:"))
  )

  (setq	p1x (nth 1 ax)
	p1y (nth 1 ay)
  )
  (setq	p2x (nth 2 ax)
	p2y (nth 2 ay)
  )
  (setq len1 (sqrt (+ (expt (- p1x p2x) 2) (expt (- p1y p2y) 2))))
  (setq cRatio (/ (- Rarea MinArea) (- MaxArea MinArea)))

  (TCal33)
  (while (>= (abs (- TArea (- RArea MinArea))) 0.0004)
    (setq cRatio (* CRatio (/ (- RArea MinArea) TArea)))
    (TCal33)
  )

  (setq ppt1 (list (nth 0 ax) (nth 0 ay)))
  (setq ppt2 (list px py))
)


(defun TCal33 ()

  (setq px (+ p1x (* (- p2x p1x) cRatio)))
  (setq py (+ p1y (* (- p2y p1y) cRatio)))
  (setq ax (subst px (nth 2 ax) ax))
  (setq ay (subst py (nth 2 ay) ay))
  (setq TArea (CalArea ax ay))
)



(defun change_Layer (flay_name TLay_name)

  (setq s_ent (ssget "X" (list (cons 8 flay_name))))
  (princ)

  (if (/= s_ent nil)
    (progn
      (command "change" s_ent "" "p" "la" TLay_name "")
    )
  )
)

(defun exit_quit_Gal (oldlay)


  (if (/= ELay_name nil)
    (change_Layer "T_Layer" ELay_name)
  )

  (if (/= oldlay nil)
    (command "layer" "s" oldlay "")
  )
  (princ)
  (exit)
)


(defun round (num dec)

  (setq num1 (atof (rtos num 2 dec)))
  (setq mf (expt 10 dec))
  (if (> (* (- num num1) mf) 5.0)
    (setq num1 (+ num1 (/ 1 mf)))
  )
  num1
)

(defun *error* (msg)
					;(command)
					;(princ "error: ")
					;(princ msg)
					;(princ)
  (Setvar "Cmdecho" 1)
  (princ)
)

(princ "\nAutoLISP Routine to subdivide Parcel............")
(princ "\nBy SOFTWEL (P) Ltd. for DOLIA...")
(princ)
