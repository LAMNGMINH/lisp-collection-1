;;--------------------------=={ Copy Field }==--------------------------;;
;;                                                                      ;;
;;  This program enables the user to copy a field expression from a     ;;
;;  selected source object to multiple destination objects in a         ;;
;;  drawing.                                                            ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'copyfield' at the AutoCAD          ;;
;;  command-line, the user is prompted to select an annotation object   ;;
;;  (Text, MText, Attribute, Multileader, Dimension) containing a       ;;
;;  field expression to be copied.                                      ;;
;;                                                                      ;;
;;  Following a valid response, the user may then copy the field to     ;;
;;  multiple selected destination objects in the drawing.               ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2013-07-14                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2017-06-13                                      ;;
;;                                                                      ;;
;;  - Updated LM:fieldcode function to account for field expressions    ;;
;;    greater than 250 characters in length.                            ;;
;;----------------------------------------------------------------------;;

(defun c:copyfield ( / *error* select src )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (defun select ( msg fun / ent rtn )
        (while
            (progn (setvar 'errno 0) (setq ent (nentsel msg))
                (cond
                    (   (= 7 (getvar 'errno))
                        (princ "\nMissed, try again.")
                    )
                    (   (= 'list (type ent))
                        (cond
                            (   (progn
                                    (if (= 4 (length ent))
                                        (setq ent (last (last ent)))
                                        (setq ent (car ent))
                                    )
                                    (not (wcmatch (cdr (assoc 0 (entget ent))) "TEXT,MTEXT,ATTRIB,MULTILEADER,*DIMENSION"))
                                )
                                (princ "\nInvalid object selected.")
                            )
                            (   (not (setq rtn ((eval fun) ent))))
                        )
                    )
                )
            )
        )
        rtn
    )

    (if
        (setq src
            (select "\nSelect source field: "
                (function
                    (lambda ( ent )
                        (cond ((LM:fieldcode ent)) ((not (princ "\nSelected object does not contain a field."))))
                    )
                )
            )
        )
        (progn
            (LM:startundo (LM:acdoc))
            (select "\nSelect destination object <Exit>: "
                (function
                    (lambda ( ent / obj )
                        (cond
                            (   (null (vlax-write-enabled-p (setq obj (vlax-ename->vla-object ent))))
                                (princ "\nSelected object is on a locked layer.")
                            )
                            (   (vlax-property-available-p obj 'textoverride t)
                                (vla-put-textoverride obj src)
                                (command "_.updatefield" ent "")
                            )
                            (   (vlax-property-available-p obj 'textstring t)
                                (vla-put-textstring obj src)
                                (command "_.updatefield" ent "")
                            )
                        )
                        nil
                    )
                )
            )
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;; Field Code  -  Lee Mac
;; Returns the field expression associated with an entity

(defun LM:fieldcode ( ent / replacefield replaceobject fieldstring enx )

    (defun replacefield ( str enx / ent fld pos )
        (if (setq pos (vl-string-search "\\_FldIdx" (setq str (replaceobject str enx))))
            (progn
                (setq ent (assoc 360 enx)
                      fld (entget (cdr ent))
                )
                (strcat
                    (substr str 1 pos)
                    (replacefield (fieldstring fld) fld)
                    (replacefield (substr str (1+ (vl-string-search ">%" str pos))) (cdr (member ent enx)))
                )
            )
            str
        )
    )

    (defun replaceobject ( str enx / ent pos )
        (if (setq pos (vl-string-search "ObjIdx" str))
            (strcat
                (substr str 1 (+ pos 5)) " "
                (LM:ObjectID (vlax-ename->vla-object (cdr (setq ent (assoc 331 enx)))))
                (replaceobject (substr str (1+ (vl-string-search ">%" str pos))) (cdr (member ent enx)))
            )
            str
        )
    )

    (defun fieldstring ( enx / itm )
        (if (setq itm (assoc 3 enx))
            (strcat (cdr itm) (fieldstring (cdr (member itm enx))))
            (cond ((cdr (assoc 2 enx))) (""))
        )
    )
    
    (if (and (wcmatch  (cdr (assoc 0 (setq enx (entget ent)))) "TEXT,MTEXT,ATTRIB,MULTILEADER,*DIMENSION")
             (setq enx (cdr (assoc 360 enx)))
             (setq enx (dictsearch enx "ACAD_FIELD"))
             (setq enx (dictsearch (cdr (assoc -1 enx)) "TEXT"))
        )
        (replacefield (fieldstring enx) enx)
    )
)

;; ObjectID  -  Lee Mac
;; Returns a string containing the ObjectID of a supplied VLA-Object
;; Compatible with 32-bit & 64-bit systems

(defun LM:ObjectID ( obj )
    (eval
        (list 'defun 'LM:ObjectID '( obj )
            (if
                (and
                    (vl-string-search "64" (getenv "PROCESSOR_ARCHITECTURE"))
                    (vlax-method-applicable-p (vla-get-utility (LM:acdoc)) 'getobjectidstring)
                )
                (list 'vla-getobjectidstring (vla-get-utility (LM:acdoc)) 'obj ':vlax-false)
               '(itoa (vla-get-objectid obj))
            )
        )
    )
    (LM:ObjectID obj)
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

(vl-load-com)
(princ
    (strcat
        "\n:: CopyField.lsp | Version 1.1 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"copyfield\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;