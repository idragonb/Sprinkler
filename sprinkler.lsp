(defun getarms (notifier-object reactor-object parameter-list)
	(setq obj  notifier-object)                             ; the arms block

    (setq handlearms (vlax-get-property obj 'Handle))
	(setq blkorg (vlax-get-property obj 'InsertionPoint))
	
	; the blocks collection - we add our entities to a block name after the handle of the arms
               ;get origin
	

	(setq acad (vlax-get-acad-object))
	(setq doc (vlax-get-property acad 'ActiveDocument))
	(setq mspace (vlax-get-property doc 'ModelSpace))
	
	(setq blks (vlax-get-property doc 'Blocks)) 
	
	;if exists, erase ********
	(erase-subblock handlearms)
	;(remove-connection handlearms)

    ; create new
	(setq blk (create-subblock blkorg handlearms))


	; get geometry from dynamic block **********************************************
	(setq props (vlax-invoke-method obj 'GetDynamicBlockProperties))
	(setq proplist (vlax-safearray->list (vlax-variant-value props)))
	(setq nn 0)
	(while (< nn (length proplist))
		(setq prop (nth nn proplist))
		;    (vlax-dump-object prop T)
		(setq propname (vlax-get-property prop 'PropertyName))
		(setq propval (vlax-variant-value (vlax-get-property prop 'Value)))
		(cond
			((= propname "ArmAngleRight") (setq angright propval))
			((= propname "ArmLengthRight") (setq armlength propval))
			((= propname "ArmAngleLeft") (setq angleft propval))
			;      ((= propname "ArmLengthLeft") (setq armleft propval))
		)
		(setq nn (1+ nn))
	)


	;************************************************************************************
	; create outer arc ********
	(setq obj-outerarc (vlax-invoke-method blk 'AddArc objorigin armlength angright angleft))
	;(setq outerarc-color (getcolor-from-att "OuterLineColor" obj))
	(setq outerarc-color (getattribute "OuterLineColor" obj 1))
	(vlax-put-property color 'ColorIndex outerarc-color); change to yellow
	(vlax-put-property obj-outerarc 'TrueColor color)


	; create inner arc *****
	(setq obj-innerarc (vlax-invoke-method blk 'AddArc objorigin 5 angright angleft))
	;(setq innerarc-color (getcolor-from-att "InnerLineColor" obj))
	(setq innerarc-color (getattribute "InnerLineColor" obj 1))
	(vlax-put-property color 'ColorIndex innerarc-color) ;green
	(vlax-put-property obj-innerarc 'TrueColor color)


	; keypoints ************

	(setq ptrain0 (vlax-3D-point (polar (list 0 0 0) angright 5)))
	(setq ptraout0 (vlax-3D-point (polar (list 0 0 0) angright armlength)))
	(setq ptlain0 (vlax-3D-point (polar (list 0 0 0) angleft 5)))
	(setq ptlaout0 (vlax-3D-point (polar (list 0 0 0) angleft armlength)))

	; create temporary lines for hatch *******************
	(setq obj-ri (vlax-invoke-method blk 'AddLine objorigin ptrain0))
	(setq obj-ro (vlax-invoke-method blk 'AddLine ptrain0 ptraout0))
	(setq obj-li (vlax-invoke-method blk 'AddLine objorigin ptlain0))
	(setq obj-lo (vlax-invoke-method blk 'AddLine ptlain0 ptlaout0))

	;*****************************************************


	; remove objectsnap temporarily **********************
	(setq curosmode (getvar "osmode"))
	(setvar "osmode" 0)

                               ; create region for inner hatch **********************
	(setq objectsinner (vlax-make-safearray vlax-vbObject  '(0 . 2))) 
	(vlax-safearray-put-element objectsinner 0 obj-ri)
	(vlax-safearray-put-element objectsinner 1 obj-li)
	(vlax-safearray-put-element objectsinner 2 obj-innerarc)
	(setq regioninner (vlax-invoke-method blk 'AddRegion objectsinner))

    ;(setq innerhatchtype (gethatchtype "HatchTypeInner" obj))
	(setq innerhatchtype (getattribute "HatchTypeInner" obj 0))
	(setq hatchinner (vlax-invoke-method blk 'AddHatch 0 innerhatchtype :vlax-true))
	(vlax-invoke-method hatchinner 'AppendOuterLoop regioninner)
	;(setq innerhatch-color (getcolor-from-att "HatchColorInner" obj))
	(setq innerhatch-color (getattribute "HatchColorInner" obj 1))
	(vlax-put-property color 'ColorIndex innerhatch-color)
	(vlax-put-property hatchinner 'TrueColor color)

                              ; create region for outer hatch ************************
	(setq objectsouter (vlax-make-safearray vlax-vbObject  '(0 . 3))) 
	(vlax-safearray-put-element objectsouter 0 obj-ro)
	(vlax-safearray-put-element objectsouter 1 obj-lo)
	(vlax-safearray-put-element objectsouter 2 obj-innerarc)
	(vlax-safearray-put-element objectsouter 3 obj-outerarc)
	(setq regionouter (vlax-invoke-method blk 'AddRegion objectsouter))

	;(setq outerhatchtype (gethatchtype "HatchTypeOuter" obj))
	(setq outerhatchtype (getattribute "HatchTypeOuter" obj 0))
	(setq hatchouter (vlax-invoke-method blk 'AddHatch 0 outerhatchtype :vlax-true))
	(vlax-invoke-method hatchouter 'AppendOuterLoop regionouter)
	;(setq outerhatch-color (getcolor-from-att "HatchColorOuter" obj))
	(setq outerhatch-color (getattribute "HatchColorOuter" obj 1))
	(vlax-put-property color 'ColorIndex outerhatch-color)
	(vlax-put-property hatchouter 'TrueColor color)
	(setq outerhatchscale (getattribute "HatchScaleOuter" obj 2))
	(vlax-put-property hatchouter 'PatternScale outerhatchscale)
	; delete temporary objects ****************************
	(vlax-invoke-method obj-li 'Delete)
	(vlax-invoke-method obj-lo 'Delete)
	(vlax-invoke-method obj-ri 'Delete)
	(vlax-invoke-method obj-ro 'Delete)
	(setq objringout (nth 0 (vlax-safearray->list(vlax-variant-value regionouter))))
	(setq objringin (nth 0 (vlax-safearray->list(vlax-variant-value regioninner))))
	(vlax-invoke-method objringin 'Delete)
	(vlax-invoke-method objringout 'Delete)
		
	
	;*********************************************************

	(setvar "osmode" curosmode)

)
(defun create-subblock (blkorg handlearms)
	(setq objorigin (vlax-3D-point (list 0 0 0)))
	(setq blk (vlax-invoke-method blks 'Add objorigin handlearms))                 ; create block
	(vlax-invoke-method mspace 'InsertBlock blkorg handlearms 1 1 1 0)       ; insert it
	blk
)
(defun getattribute (chktag blk type / nn lower upper att tag val retval) ; type 0 = string, 1 = integer, 2 = real
   (setq nn 0)
   (setq atts (vlax-variant-value (vlax-invoke-method blk 'GetAttributes)))
   (setq lower (vlax-safearray-get-l-bound atts 1))
   (setq upper (vlax-safearray-get-u-bound atts 1))
   (while (<= nn upper)
      (setq att (vlax-safearray-get-element atts nn))
	  (setq tag (vlax-get-property att 'TagString))
	  (setq val (vlax-get-property att 'TextString))
	  (if (= tag chktag)
		(setq retval val)
	  )
	  (setq nn (1+ nn))
	)
	(cond
		((= type 0) (setq retval retval))			; stays string
		((= type 1) (setq retval (atoi retval)))	; integer
		((= type 2) (setq retval (atof retval)))	; real
	)
	retval
)

(defun setattribute (chktag blk content type / nn lower upper att tag) ; type 0 = string, 1 = integer, 2 = real
   (setq nn 0)
   (setq atts (vlax-variant-value (vlax-invoke-method blk 'GetAttributes)))
   (setq lower (vlax-safearray-get-l-bound atts 1))
   (setq upper (vlax-safearray-get-u-bound atts 1))
   (while (<= nn upper)
      (setq att (vlax-safearray-get-element atts nn))
	  (setq tag (vlax-get-property att 'TagString))
	  ;(setq val (vlax-get-property att 'TextString))
	  (if (= tag chktag)
		(cond
			((= type 0) (vlax-put-property att 'TextString content))
			((= type 1) (vlax-put-property att 'TextString (itoa content)))
			((= type 2) (vlax-put-property att 'TextString (rtos content)))
		)
	  )
	  (setq nn (1+ nn))
	)
	(vlax-invoke-method att 'Update)
	(vlax-invoke-method blk 'Update)
	content
)

(defun remove-connection (obj-sprinkler)
	(setq phandle (getattribute "PolylineHandle" obj-sprinkler 0))
	(setq obj-polyline (vlax-ename->vla-object (handent phandle)))
    (setq ptn (get-sprinkler-origin obj-sprinkler))
	(setq vertexcode (getattribute "VertexCode" obj-sprinkler 1))
	(cond
		((= vertexcode 0) (remove-vertex obj-polyline ptn))
		((= vertexcode 1) (remove-vertices-at-point obj-polyline ptn))
		((= vertexcode 2) (remove-vertices-on-perpendicular obj-polyline ptn))
		((= vertexcode 3) (remove-vertices-on-radial obj-polyline ptn))
		((= vertexcode 4) (remove-vertices-last-two obj-polyline))
	)
)

(defun remove-vertices-on-perpendicular (obj-polyline pt)
	(setq index (get-vertex-index obj-polyline pt))
	(remove-vertex-by-index (obj-polyline index))
	(remove-vertex-by-index (obj-polyline index))
	(remove-vertex-by-index (obj-polyline (1+ index)))
)




(defun remove-vertices-at-point (obj-polyline pt)
	(setq index (get-vertex-index obj-polyline pt))
	(remove-vertex-by-index (obj-polyline index))
	(remove-vertex-by-index (obj-polyline index))
)
	
(defun remove-vertex (obj-polyline pt)
	(setq index (get-vertex-index obj-polyline pt))
	(remove-vertex-by-index (obj-polyline index))
)
	
(defun remove-vertex-by-index (obj-polyline index)
	(setq coords (vlax-variant-value (vlax-get-property obj-polyline 'Coordinates)))
	(setq lower (vlax-safearray-get-l-bound coords 1))
	(setq upper (vlax-safearray-get-u-bound coords 1))
	(setq amt (/ (- (1+ upper) lower) 2))
	(setq nn 0)
	(setq flag nil)
	(setq newcoords '())
	(while (< nn amt)
		(setq x (vlax-safearray-get-element coords (* nn 2)))
		(setq y (vlax-safearray-get-element coords (1+ (* nn 2))))
		(if (or (= flag 1) (/= nn index))
			(setq newcoords (append newcoords (list x y)))
			(setq flag 1)
		)
		(if flag (setq retval nn))
		(setq nn (1+ nn))
	)
	(if flag (setq amt (1- amt)))
	(setq limits (cons 0 (- (* 2 amt) 1)))
	(setq coords (vlax-make-safearray vlax-vbDouble limits))
	(vlax-safearray-fill coords newcoords)
	(vlax-put-property obj-polyline 'Coordinates coords)
	nn
)
(defun get-vertex-index (obj-polyline pt)
	(setq coords (vlax-variant-value (vlax-get-property obj-polyline 'Coordinates)))
	(setq lower (vlax-safearray-get-l-bound coords 1))
	(setq upper (vlax-safearray-get-u-bound coords 1))
	(setq amt (/ (- (1+ upper) lower) 2))
	(setq nn 0)
	(setq flag nil)
	(setq xpt (nth 0 pt))
	(setq ypt (nth 1 pt))
	(while (< nn amt)
		(setq x (vlax-safearray-get-element coords (* nn 2)))
		(setq y (vlax-safearray-get-element coords (1+ (* nn 2))))
		(if (or (= flag 1) (not (and (equal x xpt 0.01) (equal y ypt 0.01))))
			(princ)
			(setq flag 1)
		)
		(if flag (setq retval nn))
		(setq nn (1+ nn))
	)
	retval
)
			
		
	
	

(defun erase-subblock(handlearms / filt ssset nn len ent old)
	(if (tblsearch "BLOCK" handlearms)
		(progn
			(setq filt (list (cons 0 "INSERT") (cons 2 handlearms)))
			(setq ssset (ssget "X" filt))
			(setq nn 0)
			(if ssset
				(setq len (sslength ssset))
				(setq len 0)
			)
			(while (< nn len)
				(setq ent (ssname ssset nn))
				(entdel ent)
				(setq nn (1+ nn))
			)         
			(setq old (vlax-invoke-method blks 'Item handlearms))
			(vlax-invoke-method old 'Delete)
		)
	)
)





;(setq ent (car (entsel)))
;(setq obj (vlax-ename->vla-object ent))
;(setq myReactor (vlr-object-reactor (list obj) "My Reactor" '((:vlr-modified . getarms))))

(defun c:is ()     ; insert sprinkler
    (setq pt (getpoint "\nPick insertion point..."))
	(setq ptvar (vlax-3D-point pt))
	(setq acad (vlax-get-acad-object))
	(setq doc (vlax-get-property acad 'ActiveDocument))
	(setq mspace (vlax-get-property doc 'ModelSpace))
   
    (setq arms (vla-InsertBlock mspace ptvar "sprinkler" 1 1 1 0))
    (setq myReactor (vlr-object-reactor (list arms) "My Reactor" '((:vlr-modified . getarms))))
	(setq copyReactor (vlr-object-reactor (list arms) "Copy Reactor" '((:vlr-copied . makecopy))))
	(vlr-pers myReactor)
	(vlr-pers copyReactor)
	(vlax-put-property arms 'InsertionPoint ptvar)
)

(defun makecopy (notifier-object reactor-object parameter-list)
   (setq copied notifier-object)
       (setq myReactor (vlr-object-reactor (list copied) "My Reactor" '((:vlr-modified . getarms))))
	(setq copyReactor (vlr-object-reactor (list copied) "Copy Reactor" '((:vlr-copied . makecopy))))
	(vlr-pers myReactor)
	(vlr-pers copyReactor)
)