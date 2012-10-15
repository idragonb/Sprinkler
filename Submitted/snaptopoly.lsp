; this file replaces connect.lsp




(defun c:dp() ; dump object by pick
	(setq ent (car (entsel "\nPick on entity")))
	(setq obj (vlax-ename->vla-object ent))
	(vlax-dump-object obj T)
)
(defun c:od(obj) ; dump object by var
	(vlax-dump-object obj T)
)

(defun c:css () ; routine to set multiple sprinkler blocks to polyline
	(setq obj-polyline (pick-polyline))
	(while (and (setq obj-sprinkler (pick-sprinkler)) obj-polyline)
		(record-min-data obj-polyline obj-sprinkler)			;records closest point on polyline
		(snaptopolyline obj-polyline obj-sprinkler)				;sets attributes in sprinkler
		(movetopolyline obj-sprinkler)							;move sprinkler to polyline
	)
 )
 
(defun snaptopolyline (obj-polyline obj-sprinkler)	;applies minimum data to sprinkler after recorded
	(setq pto (get-sprinkler-origin obj-sprinkler))			;sprinkler insert point
	(setq ptmin (vlax-ldata-get "minimum" "point"))			;get closest point on polyline
	(setq vertexcode (vlax-ldata-get "minimum" "type"))		;get type of connection to polyline
	(setattribute "VertexCode" obj-sprinkler vertexcode 1)	;set attribute in sprinkler
	(setq phandle (vlax-get-property obj-polyline 'Handle))	;get polyline handle
	(setattribute "PolylineHandle" obj-sprinkler phandle 0)	;save handle in block
)
(defun movetopolyline (obj-sprinkler) ; main function for moving sprinkler block to polyline
	(setq ctype (getattribute "VertexCode" obj-sprinkler 1))
	(cond
		((= ctype 0) (move-to-end obj-sprinkler))
		((= ctype 1) (move-to-vertex obj-sprinkler))
		((= ctype 2) (move-to-line obj-sprinkler))
		((= ctype 3) (move-to-arc obj-sprinkler))
		((= ctype 4) (move-to-closed obj-sprinkler))
	)
)
(defun move-to-end (obj-sprinkler) 		; move sub function - moves sprinkler to open end of polyline
	(setq phandle (getattribute "PolylineHandle" obj-sprinkler 0))
	(setq offset (getattribute "Offset" obj-sprinkler 2))
	(setq obj-polyline (vlax-ename->vla-object (handent phandle)))
	(record-min-data obj-polyline obj-sprinkler)
	(setq pt (vlax-ldata-get "minimum" "point"))	; get point on arc
	(setq nvertex (vlax-ldata-get "minimum" "index"))				; get vertex number
	(setq ptn (get-sprinkler-origin obj-sprinkler))
	(setq ang (angle pt ptn))
	(setq ptf (polar pt ang offset))
	
	(if (= nvertex 0)
		(addvertex obj-polyline 0 ptf)
		(addvertex obj-polyline (1+ nvertex) ptf)
	)
	
	(vlax-put-property obj-sprinkler 'InsertionPoint (vlax-3D-point ptf))
)
(defun move-to-vertex (obj-sprinkler) 	; move sub function - moves sprinkler to non-end vertex
	(setq phandle (getattribute "PolylineHandle" obj-sprinkler 0))
	(setq offset (getattribute "Offset" obj-sprinkler 2))
	(setq obj-polyline (vlax-ename->vla-object (handent phandle)))
	(record-min-data obj-polyline obj-sprinkler)
	(setq pt (vlax-ldata-get "minimum" "point"))
	(setq nvertex (vlax-ldata-get "minimum" "index"))
	(setq ptn (get-sprinkler-origin obj-sprinkler))
	;(setq ang (angle pt ptn))
	;(setq ptf (polar pt ang offset))
	
	(setq pt-1 (getvertexpoint obj-polyline (- nvertex 1)))
	(setq pt1 (getvertexpoint obj-polyline nvertex))
	(setq pt2 (getvertexpoint obj-polyline (1+ nvertex)))
	(setq ang1 (angle pt1 pt-1))
	(setq ang2 (angle pt1 pt2))
	(setq ang (/ (+ ang1 ang2) 2))
	(setq angd (- ang2 ang1))
	(if (< angd 0) (setq angd (+ angd (* 2 pi))))
	

	(setq ptf (polar pt ang offset))
	(setq angf (angle pt1 ptf))
	(setq angcf (- angf ang1))
	(if (< angcf 0) (setq angcf (+ angcf (* 2 pi))))
	
	
	(if (< angd pi)
		(if (< angcf pi)
			(setq ptf (polar pt (+ ang pi) offset))
		)
		(if (> angcf pi)
			(setq ptf (polar pt (+ ang pi) offset))
		)
	)
	
	
	

	(vlax-put-property obj-sprinkler 'InsertionPoint (vlax-3D-point ptf))
	
	(if (< angd pi)
		(progn
			(setdynamicproperty obj-sprinkler "ArmAngleRight" ang2)
			(setdynamicproperty obj-sprinkler "ArmAngleLeft" ang1)
		)
		(progn
			(setdynamicproperty obj-sprinkler "ArmAngleRight" ang1)
			(setdynamicproperty obj-sprinkler "ArmAngleLeft" ang2)
		)
	)

	

	(addvertex obj-polyline nvertex pt)
	(addvertex obj-polyline nvertex ptf)
	(addvertex obj-polyline nvertex pt)
	



)
(defun move-to-line (obj-sprinkler) 	; move sub function - moves sprinkler to connection perpendicular to line
	(setq phandle (getattribute "PolylineHandle" obj-sprinkler 0))
	(setq offset (getattribute "Offset" obj-sprinkler 2))
	(setq obj-polyline (vlax-ename->vla-object (handent phandle)))
	(record-min-data obj-polyline obj-sprinkler)
	(setq pt (vlax-ldata-get "minimum" "point"))
	(setq nvertex (vlax-ldata-get "minimum" "index"))
	(setq ptn (get-sprinkler-origin obj-sprinkler))
	(setq ang (angle pt ptn))
	(setq ptf (polar pt ang offset))
	
	(setq angr (- ang (/ pi 2)))
	(setq angl (+ ang (/ pi 2)))

	(addvertex obj-polyline (1+ nvertex) pt)
	(addvertex obj-polyline (1+ nvertex) ptf)
	(addvertex obj-polyline (1+ nvertex) pt)
	
	(vlax-put-property obj-sprinkler 'InsertionPoint (vlax-3D-point ptf))
	
				
	(setdynamicproperty obj-sprinkler "ArmAngleRight" angr)
	(setdynamicproperty obj-sprinkler "ArmAngleLeft" angl)
)
(defun move-to-arc (obj-sprinkler)		; move sub function - moves sprinkler to connection radial to arc

	(setq phandle (getattribute "PolylineHandle" obj-sprinkler 0))
	(setq offset (getattribute "Offset" obj-sprinkler 2))
	(setq obj-polyline (vlax-ename->vla-object (handent phandle)))
	(record-min-data obj-polyline obj-sprinkler)
	(setq pt (vlax-ldata-get "minimum" "point"))
	(setq nvertex (vlax-ldata-get "minimum" "index"))
	(setq ptn (get-sprinkler-origin obj-sprinkler))
	(setq ang (angle pt ptn))
	(setq ptf (polar pt ang offset))
	(setq bulge (vlax-invoke-method obj-polyline 'GetBulge nvertex))
	(setq pt1 (getvertexpoint obj-polyline nvertex))
	(setq pt2 (getvertexpoint obj-polyline (1+ nvertex)))
	(setq rad (getarcradius pt1 pt2 bulge))
	(setq ptc (getarccenter pt1 pt2 bulge))
	;(command "line" ptc ptn "")
	;(command "line" pt1 pt2 "")
	(setq ang1 (angle ptc pt1))
	(setq ang (angle ptc pt))
	(setq ang2 (angle ptc pt2))
	(if (> bulge 0)
		(progn
			(setq angmid1 (getbisect ang1 ang))
			(setq angmid2 (getbisect ang ang2))
		)
		(progn
			(setq angmid1 (+ pi (getbisect ang1 ang)))
			(setq angmid2 (+ pi (getbisect ang ang2)))
		)
	)
	(setq ptmid1 (polar ptc angmid1 rad))
	(setq ptmid2 (polar ptc angmid2 rad))
	;(command "line" ptc ptmid1 "")
	;(command "line" ptc ptmid2 "")
	(if (not (setq pta1 (inters ptc ptmid1 pt1 pt)))
		(progn
			(setq ptmid1 (polar ptc (+ angmid2 pi) rad))
			(setq pta1 (inters ptc ptmid1 pt1 pt))
		)
	)
	(if (not (setq pta2 (inters ptc ptmid2 pt pt2)))
		(progn
			(setq ptmid2 (polar ptc (+ angmid2 pi) rad))
			(setq pta2 (inters ptc ptmid2 pt pt2))
		)
	)
	

	(setq a1 (distance pta1 ptmid1))
	(setq a2 (distance pta2 ptmid2))
	(setq b1 (/ (distance pt1 pt) 2))
	(setq b2 (/ (distance pt pt2) 2))

    (setq sign (/ bulge (abs bulge)))
	(setq bulge1 (* (/ a1 b1) sign))
	(setq bulge2 (* (/ a2 b2) sign))
	(vlax-invoke-method obj-polyline 'SetBulge nvertex bulge1)

	
	(addvertex obj-polyline (+ nvertex 1) pt)
	(addvertex obj-polyline (+ nvertex 2) ptf)
	(addvertex obj-polyline (+ nvertex 3) pt)
	(vlax-invoke-method obj-polyline 'SetBulge (+ nvertex 3) bulge2)
	
	
	(setq ang1 (angle pt pt1))
	(setq ang2 (angle pt pt2))
	(setq angd (- ang2 ang1))
	(if (< angd 0) (setq angd (+ angd (* 2 pi))))	
	
	
	(setq disn (distance ptc ptf))
	
	(cond
		((and (< disn rad) (< bulge 0)) (setq flag nil))
		((and (< disn rad) (> bulge 0)) (setq flag T))
		((and (> disn rad) (< bulge 0)) (setq flag T))
		((and (> disn rad) (> bulge 0)) (setq flag nil))
	)
		
	(if flag
		(progn
			(setdynamicproperty obj-sprinkler "ArmAngleRight" ang2)
			(setdynamicproperty obj-sprinkler "ArmAngleLeft" ang1)
		)
		(progn
			(setdynamicproperty obj-sprinkler "ArmAngleRight" ang1)
			(setdynamicproperty obj-sprinkler "ArmAngleLeft" ang2)
		)
	)

	
	
	

	(vlax-put-property obj-sprinkler 'InsertionPoint (vlax-3D-point ptf))
)
(defun getbisect (ang1 ang2) ; returns angle that sits between 1 to 2 in a anticlockwise direction
	(if (< ang2 ang1)
		(setq ang2 (+ ang2 (* 2 pi)))
	)
	(/ (+ ang1 ang2) 2)
)
(defun move-to-closed (obj-sprinkler)	; move sub function - moves sprinkler to closed end point
	(setq phandle (getattribute "PolylineHandle" obj-sprinkler 0))
	phandle
)

(defun addvertex (objpoly which ptnew)	; inserts vertex in polyline
	(vlax-invoke-method objpoly 'AddVertex which (vlax-2D-point ptnew))
)

(defun vlax-2D-point (pt)				; creates 2D point suitable for polyline insert
	(vlax-make-variant
		(vlax-safearray-fill
			(vlax-make-safearray vlax-vbdouble (cons 0 1))
				(list
					(nth 0 pt)
					(nth 1 pt)
				)
		)
	)
)

(defun record-min-data (obj-polyline obj-sprinkler / nn pt1 pt2 pt)		; finds 4 minimum data given polyline and sprinkler block
	(init-min-data)
	(setq nn 0)																			; vertex number
	(while (setq pt1 (getvertexpoint obj-polyline nn))									; vertex
		(if (setq pt2 (getvertexpoint obj-polyline (1+ nn)))							; next vertex
			(if (issectionline obj-polyline nn)											; check if section is line or arc
				(if (setq pt (isrightangleonline (get-sprinkler-origin obj-sprinkler) pt1 pt2))	; check if perpendicular falls on line
					(save-min-data
						pt
						(distance (get-sprinkler-origin obj-sprinkler) pt)
						nn
						2
					)
					(if (iscloserpt (get-sprinkler-origin obj-sprinkler) pt1 pt2)
						(if (= nn 0)
							(save-min-data
								pt1
								(distance (get-sprinkler-origin obj-sprinkler) pt1)
								nn
								0
							)
							(save-min-data
								pt1
								(distance (get-sprinkler-origin obj-sprinkler) pt1)
								nn
								1
							)
						)
					)
				)
				(if (setq pt (isradialarmonarc obj-polyline (get-sprinkler-origin obj-sprinkler) pt1 pt2 (getbulge obj-polyline nn)))		;section is arc
					(save-min-data
						pt
						(distance (get-sprinkler-origin obj-sprinkler) pt)
						nn
						3
					)
					(if (iscloserpt (get-sprinkler-origin obj-sprinkler) pt1 pt2)
						(if (= nn 0)
							(save-min-data
								pt1
								(distance (get-sprinkler-origin obj-sprinkler) pt1)
								nn
								0
							)
							(save-min-data
								pt1
								(distance (get-sprinkler-origin obj-sprinkler) pt1)
								nn
								1
							)
						)
					)
				)
			)
		)
		(progn
			(save-min-data
				pt1
				(distance (get-sprinkler-origin obj-sprinkler) pt1)
				nn
				0
			)
		)
		(setq nn (1+ nn))
	)
)
						
						
						
(defun iscloserpt (pt pt1 pt2)			; finds if first close is closer than second pt to given point
   (if (< (distance pt pt1) (distance pt pt2))
		T
		nil
	)
)
(defun isradialarmonarc (obj-polyline pt pt1 pt2 bulge / ptc ptt dis1 dis2 dis) ; checks if radial ray intersects with arc segment

	(setq acadd (vlax-get-acad-object))
	(setq doc (vlax-get-property acadd 'ActiveDocument))
	(setq mspace (vlax-get-property doc 'ModelSpace))
	

	(setq ptc (getarccenter pt1 pt2 bulge))
	(setq rad (getarcradius pt1 pt2 bulge))
	
	(setq chkline (vlax-invoke-method mspace 'AddLine (vlax-3D-point ptc) (vlax-3D-point (polar pt (angle ptc pt) rad))))
	(setq int (vlax-variant-value (vlax-invoke-method obj-polyline 'IntersectWith chkline acExtendNone)))
	(vlax-invoke-method chkline 'Delete)
	(if (< (vlax-safearray-get-u-bound int 1) 0)
		(setq ptt (polar ptc (angle pt ptc) rad))
		(setq ptt (polar ptc (angle ptc pt) rad))
	)
	
	(setq d (distance pt ptt))
	(setq d1 (distance pt pt1))
	(setq d2 (distance pt pt2))
	
	
	(setq dis1 (distance pt1 ptt))
	(setq dis2 (distance pt2 ptt))
	(setq dis (distance pt1 pt2))
	(if (and  (< dis1 dis) (< dis2 dis) (< d d1) (< d d2)) ; 
		ptt
		nil
	)
)
	
	
(defun getarcradius (pt1 pt2 bulge / b a r) ; gets radius of arc from bulge info
	(setq b (/ (distance pt1 pt2) 2))
	(setq a (* b bulge))
	(setq r (/ (+ (* a a) (* b b)) (* 2 a)))
	(abs r)
)	
(defun getarccenter (pt1 pt2 bulge / b a r ang rang ptmid ptc) ; gets center point of arc from bulge info
	(setq b (/ (distance pt1 pt2) 2))
	(setq a (* b bulge))
	(setq r (/ (+ (* a a) (* b b)) (* 2 a)))
	(setq ang (angle pt1 pt2))
	;(if (< bulge 0)
		(setq rang (+ ang (/ pi 2)))
	;	(setq rang (- ang (/ pi 2)))
	;)
	(setq ptmid (polar pt1 ang b))
	(setq ptc (polar ptmid rang (- r a)))
)
	
	
(defun getbulge (obj index)				; gets bulge value from centerpoint and radius
	(vlax-invoke-method obj 'GetBulge index)
)	
(defun init-min-data ()					; initializes 4 minimum data
	(vlax-ldata-put "minimum" "point" nil)
	(vlax-ldata-put "minimum" "distance" nil)
	(vlax-ldata-put "minimum" "index" nil)
	(vlax-ldata-put "minimum" "type" nil)
)
(defun save-min-data (pt dis index type) ; type 0 = on end vertex, 1 = on mid vertex, 2 = on line segment, 3 = on arc segment, 4 = on closed vertex
    (if (not (setq curmin (vlax-ldata-get "minimum" "distance")))
		(progn
			(vlax-ldata-put "minimum" "point" pt)
			(vlax-ldata-put "minimum" "distance" dis)
			(vlax-ldata-put "minimum" "index" index)
			(vlax-ldata-put "minimum" "type" type)
		)	
		(if (< dis curmin)
			(progn
				(vlax-ldata-put "minimum" "point" pt)
				(vlax-ldata-put "minimum" "distance" dis)
				(vlax-ldata-put "minimum" "index" index)
				(vlax-ldata-put "minimum" "type" type)
			)
		)
	)
)


		
(defun isrightangleonline (pt pt1 pt2 / ang rang pt3 pt4) ; checks if perpendicular through point intersects line segment
	(setq ang (angle pt1 pt2))
	(setq rang (+ ang (/ pi 2)))
	(setq pt3 (polar pt rang 1000))
	(setq pt4 (polar pt (+ rang pi) 1000))
	(inters pt1 pt2 pt3 pt4 T)
)
(defun issectionline (obj-polyline nn / bulge) ; checks if section following vertex in polyline is arc or line
	(setq bulge (vlax-invoke-method obj-polyline 'GetBulge nn))
	(if (= bulge 0.0)
		T
		nil
	)
)
		
(defun getvertexpoint (obj-polyline mm / nn x y pt first final) ; returns nth vertex coordinates

	(setq coords (vlax-variant-value (vlax-get-property obj-polyline 'Coordinates)))	; polyline coordinates
	(setq first (vlax-safearray-get-l-bound coords 1))									; getstart x coord
	(setq final (vlax-safearray-get-u-bound coords 1))									; get end y coord
	(setq total (/ (- final first) 2))													; total number of coordinates
	(if (> mm total)
		(setq pt nil)
		(progn
			(setq nn (* mm 2))
			(setq x (vlax-safearray-get-element coords nn))
			(setq y (vlax-safearray-get-element coords (1+ nn)))
			(setq pt (list x y))
		)
	)
	pt
)

(defun pick-sprinkler (/ ent objsprinkler arms obj-arms) ; prompts user to indicate sprinkler block with pick
	(setq ent (car (entsel "\nPick on sprinkler")))
	(setq objsprinkler (vlax-ename->vla-object ent))
	(if (/= (vlax-get-property objsprinkler 'EffectiveName) "sprinkler")
		(progn
			(setq handle (vlax-get-property objsprinkler 'Name))
			(if (setq arms (handent handle))
				(setq obj-arms (vlax-ename->vla-object arms))
				(setq obj-arms nil)
			)
		)
		(setq obj-arms objsprinkler)
	)
	obj-arms
)
(defun pick-polyline (/ ent obj)		; prompts user to pick polyline
	(if (setq ent (car (entsel "\nPick on polyline")))
		(progn
			(setq obj (vlax-ename->vla-object ent))
			(if (= (vlax-get-property obj 'ObjectName) "AcDbPolyline")
				obj
				nil
			)
		)
	)
)
(defun get-sprinkler-origin (objsprinkler) ; returns insertion poin of polyline
   (vlax-safearray->list (vlax-variant-value (vlax-get-property objsprinkler 'InsertionPoint)))
)