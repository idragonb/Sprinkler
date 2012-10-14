; this file replaces connect.lsp




(defun c:dp() ; dump object by pick
	(setq ent (car (entsel "\nPick on entity")))
	(setq obj (vlax-ename->vla-object ent))
	(vlax-dump-object obj T)
)
(defun c:od(obj) ; dump object by var
	(vlax-dump-object obj T)
)

(defun c:css ()
	(setq obj-polyline (pick-polyline))
	(while (and (setq obj-sprinkler (pick-sprinkler)) obj-polyline)
		(record-min-data obj-polyline obj-sprinkler)
		(snaptopolyline obj-polyline obj-sprinkler)
	)
 )
 
 (defun snaptopolyline (obj-polyline obj-sprinkler)
	(setq pto (get-sprinkler-origin obj-sprinkler))
	(setq ptmin (vlax-ldata-get "minimum" "point"))
	(setq vertexcode (vlax-ldata-get "minimum" "type"))
	(setattribute "VertexCode" obj-sprinkler vertexcode 1)
	(setq phandle (vlax-get-property obj-polyline 'Handle))
	(setattribute "PolylineHandle" obj-sprinkler phandle 0)
	(movetopolyline obj-sprinkler)
)
(defun movetopolyline (obj-sprinkler)
	(setq ctype (getattribute "VertexCode" obj-sprinkler 1))
	(cond
		((= ctype 0) (move-to-end obj-sprinkler))
		((= ctype 1) (move-to-vertex obj-sprinkler))
		((= ctype 2) (move-to-line obj-sprinkler))
		((= ctype 3) (move-to-arc obj-sprinkler))
		((= ctype 4) (move-to-closed obj-sprinkler))
	)
)
(defun move-to-end (obj-sprinkler)
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
(defun move-to-vertex (obj-sprinkler)
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
	(setq ptf (polar pt (+ ang pi) offset))
	

	(addvertex obj-polyline nvertex pt)
	(addvertex obj-polyline nvertex ptf)
	(addvertex obj-polyline nvertex pt)
	
	(vlax-put-property obj-sprinkler 'InsertionPoint (vlax-3D-point ptf))
)
(defun move-to-line (obj-sprinkler)
	(setq phandle (getattribute "PolylineHandle" obj-sprinkler 0))
	(setq offset (getattribute "Offset" obj-sprinkler 2))
	(setq obj-polyline (vlax-ename->vla-object (handent phandle)))
	(record-min-data obj-polyline obj-sprinkler)
	(setq pt (vlax-ldata-get "minimum" "point"))
	(setq nvertex (vlax-ldata-get "minimum" "index"))
	(setq ptn (get-sprinkler-origin obj-sprinkler))
	(setq ang (angle pt ptn))
	(setq ptf (polar pt ang offset))

	(addvertex obj-polyline (1+ nvertex) pt)
	(addvertex obj-polyline (1+ nvertex) ptf)
	(addvertex obj-polyline (1+ nvertex) pt)
	
	(vlax-put-property obj-sprinkler 'InsertionPoint (vlax-3D-point ptf))
)
(defun move-to-arc (obj-sprinkler)
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
	(setq ang1 (angle ptc pt1))
	(setq ang (angle ptc pt))
	(setq ang2 (angle ptc pt2))
	(setq angmid1 (/ (+ ang1 ang) 2))
	(setq angmid2 (/ (+ ang ang2) 2))
	(setq ptmid1 (polar ptc angmid1 rad))
	(setq ptmid2 (polar ptc angmid2 rad))
	(setq pta1 (inters ptc ptmid1 pt1 pt))
	(setq pta2 (inters ptc ptmid2 pt pt2))
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
	
	(vlax-put-property obj-sprinkler 'InsertionPoint (vlax-3D-point ptf))
)
(defun move-to-closed (obj-sprinkler)
	(setq phandle (getattribute "PolylineHandle" obj-sprinkler 0))
	phandle
)

(defun addvertex (objpoly which ptnew)
	(vlax-invoke-method objpoly 'AddVertex which (vlax-2D-point ptnew))
)




(defun vlax-2D-point (pt)
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

(defun record-min-data (obj-polyline obj-sprinkler)	
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
						;(save-min-data  ;not really needed, will test on next vertex
						;	pt2
						;	(distance (get-sprinkler-origin obj-sprinkler) pt2)
						;	(1+ nn)
						;	1
						;)
					)
				)
				(if (setq pt (isradialarmonarc (get-sprinkler-origin obj-sprinkler) pt1 pt2 (getbulge obj-polyline nn)))		;section is arc
					(save-min-data
						pt
						(distance (get-sprinkler-origin obj-sprinkler) pt)
						nn
						3
					)
					(if (iscloserpt (get-sprinkler-origin obj-sprinkler) pt1 pt2)
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
						
						
						
(defun iscloserpt (pt pt1 pt2)
   (if (< (distance pt pt1) (distance pt pt2))
		T
		nil
	)
)
(defun isradialarmonarc (pt pt1 pt2 bulge / ptc ptt dis1 dis2 dis)
	(setq ptc (getarccenter pt1 pt2 bulge))
	(setq ptt (polar ptc (angle ptc pt) (getarcradius pt1 pt2 bulge)))
	(setq dis1 (distance pt1 ptt))
	(setq dis2 (distance pt2 ptt))
	(setq dis (distance pt1 pt2))
	(if (and (< dis1 dis) (< dis2 dis))
		ptt
		nil
	)
)
	
	
(defun getarcradius (pt1 pt2 bulge / b a r)
	(setq b (/ (distance pt1 pt2) 2))
	(setq a (* b bulge))
	(setq r (/ (+ (* a a) (* b b)) (* 2 a)))
	(abs r)
)	
(defun getarccenter (pt1 pt2 bulge / b a r ang rang ptmid ptc)
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
	
	
(defun getbulge (obj index)
	(vlax-invoke-method obj 'GetBulge index)
)	
(defun init-min-data ()
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


		
(defun isrightangleonline (pt pt1 pt2 / ang rang pt3 pt4)
	(setq ang (angle pt1 pt2))
	(setq rang (+ ang (/ pi 2)))
	(setq pt3 (polar pt rang 1000))
	(setq pt4 (polar pt (+ rang pi) 1000))
	(inters pt1 pt2 pt3 pt4 T)
)
(defun issectionline (obj-polyline nn / bulge)
	(setq bulge (vlax-invoke-method obj-polyline 'GetBulge nn))
	(if (= bulge 0.0)
		T
		nil
	)
)
		
(defun getvertexpoint (obj-polyline mm / nn x y pt first final)

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

(defun pick-sprinkler (/ ent objsprinkler arms obj-arms)
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
(defun pick-polyline (/ ent obj)
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
(defun get-sprinkler-origin (objsprinkler) 
   (vlax-safearray->list (vlax-variant-value (vlax-get-property objsprinkler 'InsertionPoint)))
)