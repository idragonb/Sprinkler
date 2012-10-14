(defun getarms (notifier-object reactor-object parameter-list)
	(setq obj  notifier-object)                             ; the arms block

    (setq handlearms (vlax-get-property obj 'Handle))
	(setq blkorg (vlax-get-property obj 'InsertionPoint))
	
	; the blocks collection - we add our entities to a block name after the handle of the arms
               ;get origin
	(setq objorigin (vlax-3D-point (list 0 0 0)))

	(setq acad (vlax-get-acad-object))
	(setq doc (vlax-get-property acad 'ActiveDocument))
	(setq mspace (vlax-get-property doc 'ModelSpace))
	
	(setq blks (vlax-get-property doc 'Blocks)) 
	
	;if exists, erase ********
	(erase-subblock handlearms)


	(setq blk (vlax-invoke-method blks 'Add objorigin handlearms))                 ; create block
	(vlax-invoke-method mspace 'InsertBlock blkorg handlearms 1 1 1 0)       ; insert it


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
	(setq outerarc-color (getcolor-from-att "OuterLineColor" obj))
	(vlax-put-property color 'ColorIndex outerarc-color); change to yellow
	(vlax-put-property obj-outerarc 'TrueColor color)


	; create inner arc *****
	(setq obj-innerarc (vlax-invoke-method blk 'AddArc objorigin 5 angright angleft))
	(setq innerarc-color (getcolor-from-att "InnerLineColor" obj))
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

    (setq innerhatchtype (gethatchtype "HatchTypeInner" obj))
	(setq hatchinner (vlax-invoke-method blk 'AddHatch 0 innerhatchtype :vlax-true))
	(vlax-invoke-method hatchinner 'AppendOuterLoop regioninner)
	(setq innerhatch-color (getcolor-from-att "HatchColorInner" obj))
	(vlax-put-property color 'ColorIndex innerhatch-color)
	(vlax-put-property hatchinner 'TrueColor color)

                              ; create region for outer hatch ************************
	(setq objectsouter (vlax-make-safearray vlax-vbObject  '(0 . 3))) 
	(vlax-safearray-put-element objectsouter 0 obj-ro)
	(vlax-safearray-put-element objectsouter 1 obj-lo)
	(vlax-safearray-put-element objectsouter 2 obj-innerarc)
	(vlax-safearray-put-element objectsouter 3 obj-outerarc)
	(setq regionouter (vlax-invoke-method blk 'AddRegion objectsouter))

	(setq outerhatchtype (gethatchtype "HatchTypeOuter" obj))
	(setq hatchouter (vlax-invoke-method blk 'AddHatch 0 outerhatchtype :vlax-true))
	(vlax-invoke-method hatchouter 'AppendOuterLoop regionouter)
	(setq outerhatch-color (getcolor-from-att "HatchColorOuter" obj))
	(vlax-put-property color 'ColorIndex outerhatch-color)
	(vlax-put-property hatchouter 'TrueColor color)
	(setq outerhatchscale (gethatchscale "HatchScaleOuter" obj))
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

(defun gethatchscale (chktag blk)
   (setq atts (vlax-variant-value (vlax-invoke-method obj 'GetAttributes)))
   (setq nn 0)
   (setq lower (vlax-safearray-get-l-bound atts 1))
   (setq upper (vlax-safearray-get-u-bound atts 1))
   (while (<= nn upper)
      (setq att (vlax-safearray-get-element atts nn))
	  (setq tag (vlax-get-property att 'TagString))
	  (setq val (vlax-get-property att 'TextString))
	  (if (= tag chktag)
		(setq retval (atof val))
	  )
	  (setq nn (1+ nn))
	)
	retval
)
(defun gethatchtype (chktag blk)
   (setq atts (vlax-variant-value (vlax-invoke-method obj 'GetAttributes)))
   (setq nn 0)
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
	retval
)

(defun getcolor-from-att (chktag blk)
   (setq atts (vlax-variant-value (vlax-invoke-method obj 'GetAttributes)))
   (setq nn 0)
   (setq lower (vlax-safearray-get-l-bound atts 1))
   (setq upper (vlax-safearray-get-u-bound atts 1))
   (while (<= nn upper)
      (setq att (vlax-safearray-get-element atts nn))
	  (setq tag (vlax-get-property att 'TagString))
	  (setq val (vlax-get-property att 'TextString))
	  (if (= tag chktag)
		(setq retval (atoi val))
	  )
	  (setq nn (1+ nn))
	)
	retval
)



(defun erase-subblock(handlearms)
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


(defun write-ldata (notifier-object reactor-object parameter-list)
	(setq olobj notifier-object)
	(setq truecolor-outerarc (vlax-get-property olobj 'TrueColor))
	(setq color-outerarc (vlax-get-property truecolor-outerarc 'ColorIndex))
	(vlax-ldata-put handlearms "outerarc-color" (itoa color-outerarc))
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