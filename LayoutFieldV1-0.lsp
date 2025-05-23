;;-------------------------=={ Layout Field }==-------------------------;;
;;                                                                      ;;
;;  This program defines a set of commands which enable the user to     ;;
;;  populate a selected annotation object (Text, MText, Attribute)      ;;
;;  with a field expression referencing the name and position of the    ;;
;;  layout in which the object resides.                                 ;;
;;                                                                      ;;
;;  Contrary to the typical use of the CTAB system variable, as offered ;;
;;  by the standard AutoCAD FIELD command, the field expressions        ;;
;;  generated by this program directly reference the ActiveX Layout     ;;
;;  object within the Layouts Collection, and therefore do not depend   ;;
;;  on the layout being current in order to display the correct output. ;;
;;                                                                      ;;
;;  This is particularly advantageous when using the DATAEXTRACTION     ;;
;;  command to retrieve data from multi-layout drawings, as field       ;;
;;  expressions which reference the CTAB system variable will yield     ;;
;;  the same value for every layout when processed by this command.     ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright � 2016  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2016-12-29                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;

;;----------------------------------------------------------------------;;
;;  Layout Name                                                         ;;
;;                                                                      ;;
;;  Generates a field expression referencing the name of the layout     ;;
;;  in which the selected annotation object resides.                    ;;
;;----------------------------------------------------------------------;;

(defun c:lfname ( )
    (layoutfield
       '(lambda ( obj )
            (vla-put-textstring obj
                (strcat
                    "%<\\AcObjProp Object(%<\\_ObjId "
                    (LM:objectid (layoutfield:layout obj))
                    ">%).Name>%"
                )
            )
        )
    )
)

;;----------------------------------------------------------------------;;
;;  Layout Number                                                       ;;
;;                                                                      ;;
;;  Generates a field expression referencing the position of the layout ;;
;;  in which the selected annotation object resides.                    ;;
;;----------------------------------------------------------------------;;

(defun c:lfnumber ( )
    (layoutfield
       '(lambda ( obj )
            (vla-put-textstring obj
                (strcat
                    "Sheet "
                    "%<\\AcObjProp Object(%<\\_ObjId "
                    (LM:objectid (layoutfield:layout obj))
                    ">%).TabOrder>%"
                )
            )
        )
    )
)

;;----------------------------------------------------------------------;;
;;  Sheet Number & Total Sheets                                         ;;
;;                                                                      ;;
;;  Generates a field expression referencing the position of the layout ;;
;;  in which the selected annotation object resides, and the total      ;;
;;  number of paperspace layouts in the drawing.                        ;;
;;----------------------------------------------------------------------;;

(defun c:lfsheet ( )
    (layoutfield
       '(lambda ( obj )
            (vla-put-textstring obj
                (strcat
                    "Sheet "
                    "%<\\AcObjProp Object(%<\\_ObjId "
                    (LM:objectid (layoutfield:layout obj))
                    ">%).TabOrder>%"
                    " of "
                    "%<\\AcExpr %<\\AcObjProp Object(%<\\_ObjId "
                    (LM:objectid (vla-get-layouts (LM:acdoc)))
                    ">%).Count>%-1 >%"
                )
            )
        )
    )
)

;;----------------------------------------------------------------------;;

(defun layoutfield ( fld / *error* ent )
    
    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (while
        (progn (setvar 'errno 0) (setq ent (nentsel "\nSelect text or attribute: "))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (null ent) nil)
                (   (or (< 2 (length ent))
                        (not (wcmatch (cdr (assoc 0 (entget (setq ent (car ent))))) "TEXT,MTEXT,ATTRIB"))
                    )
                    (princ "\nInvalid object selected.")
                )
            )
        )
    )
    (if ent
        (progn
            (LM:startundo (LM:acdoc))
            (apply fld (list (vlax-ename->vla-object ent)))
            (if (= "ATTRIB" (cdr (assoc 0 (entget ent))))
                (vla-regen (LM:acdoc) acactiveviewport)
            )
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)
(defun layoutfield:layout ( obj )
    (if (and (vlax-property-available-p obj 'islayout) (= :vlax-true (vla-get-islayout obj)))
        (vla-get-layout obj)
        (layoutfield:layout (LM:owner obj))
    )
)

;; Owner -  Lee Mac
;; A wrapper for the objectidtoobject method & ownerid property to enable
;; compatibility with 32-bit & 64-bit systems
 
(defun LM:owner ( obj )
    (eval
        (list 'defun 'LM:owner '( obj )
            (if (vlax-method-applicable-p obj 'ownerid32)
                (list 'vla-objectidtoobject32 (LM:acdoc) '(vla-get-ownerid32 obj))
                (list 'vla-objectidtoobject   (LM:acdoc) '(vla-get-ownerid   obj))
            )
        )
    )
    (LM:owner obj)
)

;; ObjectID  -  Lee Mac
;; Returns a string containing the ObjectID of a supplied VLA-Object
;; Compatible with 32-bit & 64-bit systems
 
(defun LM:objectid ( obj )
    (eval
        (list 'defun 'LM:objectid '( obj )
            (if (vlax-method-applicable-p (vla-get-utility (LM:acdoc)) 'getobjectidstring)
                (list 'vla-getobjectidstring (vla-get-utility (LM:acdoc)) 'obj ':vlax-false)
               '(itoa (vla-get-objectid obj))
            )
        )
    )
    (LM:objectid obj)
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com) (princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;