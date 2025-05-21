;;--------------------=={ Field Formatting Code }==---------------------;;
;;                                                                      ;;
;;  This program will prompt the user to select an annotation object    ;;
;;  containing one or more field expressions with formatting applied;   ;;
;;  following a valid selection, will proceed to print the outermost    ;;
;;  formatting code associated with the last formatted field            ;;
;;  expression held by the object to the command-line.                  ;;
;;                                                                      ;;
;;  The program is compatible for use with Text, MText, Attributes,     ;;
;;  Multileaders & Dimensions containing one or more field expressions  ;;
;;  with formatting applied.                                            ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2013-11-12                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2017-06-13                                      ;;
;;                                                                      ;;
;;  - Updated LM:fieldcode function to account for field expressions    ;;
;;    greater than 250 characters in length.                            ;;
;;----------------------------------------------------------------------;;

(defun c:fieldformat ( / ent fld fmt )
    (while
        (progn (setvar 'errno 0) (setq ent (nentsel "\nSelect field: "))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (null ent) nil)
                (   (progn
                        (if (= 4 (length ent))
                            (setq ent (last (last ent)))
                            (setq ent (car ent))
                        )
                        (not (wcmatch (cdr (assoc 0 (entget ent))) "TEXT,MTEXT,ATTRIB,MULTILEADER,*DIMENSION"))
                    )
                    (princ "\nInvalid object selected.")
                )
                (   (null (setq fld (LM:fieldcode ent)))
                    (princ "\nSelected object does not contain a field.")
                )
                (   (null (setq fmt (last (LM:fieldformatting fld))))
                    (princ "\nSelected field has no formatting.")
                )
            )
        )
    )
    (if fmt
        (progn
            (princ "\nFormatting code: ")
            (prin1 fmt)
        )
    )
    (princ)
)

;; Field Formatting  -  Lee Mac
;; Returns a list of all formatting codes associated with a field expression

(defun LM:fieldformatting ( fld / pos )
    (if
        (and
            (setq pos (vl-string-search "\\f \"" fld))
            (setq fld (substr fld (+ 5 pos))
                  pos (vl-string-search "\">%" fld)
            )
        )
        (cons (substr fld 1 pos) (LM:fieldformatting (substr fld (+ 3 pos))))
    )
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
 
(defun LM:objectid ( obj )
    (eval
        (list 'defun 'LM:objectid '( obj )
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
    (LM:objectid obj)
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object
 
(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

(vl-load-com)
(princ
    (strcat
        "\n:: FieldFormat.lsp | Version 1.1 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"fieldformat\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;