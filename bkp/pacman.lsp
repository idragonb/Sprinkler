(defun getarms (notifier-object reactor-object parameter-list)
   
(setq obj  notifier-object)                             ; the arms block
(setq ent (vlax-vla-object->ename obj))
(setq armsent ent)
(setq entlist (entget ent))
(setq handlearms (cdr (assoc 5 entlist)))
(setq c10 (cdr (assoc 10 entlist)))
(setq blkorg (vlax-3D-point c10))
; the blocks collection - we add our entities to a block name after the handle of the arms
               ;get origin
  (setq objorigin (vlax-3D-point (list 0 0 0)))

(setq acad (vlax-get-acad-object))
(setq doc (vlax-get-property acad 'ActiveDocument))
(setq mspace (vlax-get-property doc 'ModelSpace))

 (setq blks (vlax-get-property doc 'Blocks)) 
;if exists, erase ********
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
   (entmake
      (list
         (cons 0 "ARC")
         (cons 10 c10)
         (cons 40 armlength)
         (cons 50 angright)
         (cons 51 angleft)
         (cons 62 2)
      )
   )
   (setq ent (entlast))
   (setq outerarc ent)
   (setq entlist (entget ent))
   (setq handleouter (cdr (assoc 5 entlist)))
(princ "\n000")
; create inner arc *****
  (setq obj-innerarc (vlax-invoke-method blk 'AddArc objorigin 5 angright angleft))
   (entmake
      (list
         (cons 0 "ARC")
         (cons 10 c10)
         (cons 40 5)
         (cons 50 angright)
         (cons 51 angleft)
      )
   )
   (setq ent (entlast))
   (setq innerarc ent)
   (setq entlist (entget ent))
   (setq handleinner (cdr (assoc 5 entlist)))

   (setq angmid (/ (+ angright angleft) 2))
   (setq ptmid (polar c10  angmid armlength))



; keypoints ************
   (setq ptrain (polar c10 angright 5))
   (setq ptraout (polar c10 angright armlength))
   (setq ptlain (polar c10 angleft 5))
   (setq ptlaout (polar c10 angleft armlength))
(princ "\n111")
   (setq ptrain0 (vlax-3D-point (polar (list 0 0 0) angright 5)))
   (setq ptraout0 (vlax-3D-point (polar (list 0 0 0) angright armlength)))
   (setq ptlain0 (vlax-3D-point (polar (list 0 0 0) angleft 5)))
   (setq ptlaout0 (vlax-3D-point (polar (list 0 0 0) angleft armlength)))

; create temporary lines for hatch *******************
(setq obj-ri (vlax-invoke-method blk 'AddLine objorigin ptrain0))
   (entmake
      (list
         (cons 0 "LINE")
         (cons 10 c10)
         (cons 11 ptrain)
      )
   )
   (setq ent (entlast))
   (setq ri ent)
(setq obj-ro (vlax-invoke-method blk 'AddLine ptrain0 ptraout0))
    (entmake
      (list
         (cons 0 "LINE")
         (cons 10 ptrain)
         (cons 11 ptraout)
      )
   )  
   (setq ent (entlast))
   (setq ro ent)
(princ "\n222")
(setq obj-li (vlax-invoke-method blk 'AddLine objorigin ptlain0))
    (entmake
      (list
         (cons 0 "LINE")
         (cons 10 c10)
         (cons 11 ptlain)
      )
   )  
   (setq ent (entlast))
   (setq li ent)
(setq obj-lo (vlax-invoke-method blk 'AddLine ptlain0 ptlaout0))
    (entmake
      (list
         (cons 0 "LINE")
         (cons 10 ptlain)
         (cons 11 ptlaout)
      )
   )  
   (setq ent (entlast))
   (setq lo ent)
;*****************************************************
(princ "\n333")
; get a positive representation of the opening angle **********
   (setq test (- angleft angright))
   (if (< test 0) (setq test (+ 360 test)))

   (if (< test pi)
      (setq pt1 (polar c10 (/ (+ angright angleft) 2) 2))
      (setq pt1 (polar c10 (+ pi (/ (+ angright angleft) 2)) 2))
   )
   (if (< test pi)
      (setq pt2 (polar c10 (/ (+ angright angleft) 2) 5.5))
      (setq pt2 (polar c10 (+ (/ (+ angright angleft) 2) pi) 5.5))
   )

; remove objectsnap temporarily **********************
  (setq curosmode (getvar "osmode"))
  (setvar "osmode" 0)
 ; (command "cecolor" "green")




; objects of temporary entities ************
(setq objri (vlax-ename->vla-object ri))
(setq objro (vlax-ename->vla-object ro))
(setq objli (vlax-ename->vla-object li))
(setq objlo (vlax-ename->vla-object lo))
(setq objringinner (vlax-ename->vla-object innerarc))
(setq objringouter (vlax-ename->vla-object outerarc))

                               ; create region for inner hatch **********************
(setq objectsinner (vlax-make-safearray vlax-vbObject  '(0 . 2))) 
(vlax-safearray-put-element objectsinner 0 obj-ri)
(vlax-safearray-put-element objectsinner 1 obj-li)
(vlax-safearray-put-element objectsinner 2 obj-innerarc)
(setq regioninner (vlax-invoke-method blk 'AddRegion objectsinner))

(setq hatchinner (vlax-invoke-method blk 'AddHatch 0 "Solid" :vlax-true))
(vlax-invoke-method hatchinner 'AppendOuterLoop regioninner)

                              ; create region for outer hatch ************************
(setq objectsouter (vlax-make-safearray vlax-vbObject  '(0 . 3))) 
(vlax-safearray-put-element objectsouter 0 obj-ro)
(vlax-safearray-put-element objectsouter 1 obj-lo)
(vlax-safearray-put-element objectsouter 2 obj-innerarc)
(vlax-safearray-put-element objectsouter 3 obj-outerarc)
(setq regionouter (vlax-invoke-method blk 'AddRegion objectsouter))

(setq hatchouter (vlax-invoke-method blk 'AddHatch 0 "ANSI31" :vlax-true))
(vlax-invoke-method hatchouter 'AppendOuterLoop regionouter)

 ; delete temporary objects ****************************
(vlax-invoke-method objli 'Delete)
(vlax-invoke-method objlo 'Delete)
(vlax-invoke-method objri 'Delete)
(vlax-invoke-method objro 'Delete)
(setq objringout (nth 0 (vlax-safearray->list(vlax-variant-value regionouter))))
(setq objringin (nth 0 (vlax-safearray->list(vlax-variant-value regioninner))))
  (vlax-invoke-method objringin 'Delete)
  (vlax-invoke-method objringout 'Delete)


 
 ;(setq group (vlax-invoke-method groups 'Add handlearms))

(setq objarcinner (vlax-ename->vla-object innerarc))
(setq objarcouter (vlax-ename->vla-object outerarc))

;(setq objgroup (vlax-make-safearray vlax-vbObject  '(0 . 3))) 
;(vlax-safearray-put-element objgroup 0 objarcinner)
;(vlax-safearray-put-element objgroup 1 objarcouter)
;(vlax-safearray-put-element objgroup 2 hatchinner)
;(vlax-safearray-put-element objgroup 3 hatchinner)


; (vlax-invoke-method group 'AppendItems objgroup)

  (setvar "osmode" curosmode)

;  (vlax-ldata-put "ACT" handlearms handleblk)

)


;(setq ent (car (entsel)))
;(setq obj (vlax-ename->vla-object ent))
;(setq myReactor (vlr-object-reactor (list obj) "My Reactor" '((:vlr-modified . getarms))))


   


