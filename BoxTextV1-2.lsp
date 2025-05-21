;;----------------------------=={ Box Text }==--------------------------;;
;;                                                                      ;;
;;  This program performs in much the same way as the Express Tools'    ;;
;;  'TCircle' command: enabling the user to create a 2D polyline        ;;
;;  rectangular frame around selected Text & MText objects, with a      ;;
;;  user-defined offset.                                                ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'BT' at the AutoCAD command-line,   ;;
;;  the program first prompts the user to specify an offset factor      ;;
;;  for the text frame. This factor is multiplied by the text height    ;;
;;  for every selected text object to determine the offset of the       ;;
;;  rectangular frame from the text. At this prompt, the last used      ;;
;;  value is available as a default option.                             ;;
;;                                                                      ;;
;;  The program then prompts the user to make a selection of text       ;;
;;  and/or mtext objects. Following a valid selection, the program      ;;
;;  iterates over the selection and constructs a rectangular frame      ;;
;;  surrounding each object, offset by a distance determined by the     ;;
;;  given offset factor. The generated text box will inherit the        ;;
;;  basic properties of the enclosed text object (e.g. Layer, Linetype, ;;
;;  Lineweight etc.).                                                   ;;
;;                                                                      ;;
;;  The program will also perform successfully with Text or MText       ;;
;;  defined in any construction plane, and under all UCS & view         ;;
;;  settings.                                                           ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2010  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2015-02-22                                      ;;
;;----------------------------------------------------------------------;;

(defun c:bt ( / *error* def enx idx lst off sel )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if (or (not (setq def (getenv "LMac\\boxtext-off")))
            (not (setq def (distof def 2)))
        )
        (setenv "LMac\\boxtext-off" (rtos (setq def 0.35) 2 2))
    )
    (initget 4)
    (if (setq off (getreal (strcat "\nSpecify offset factor <" (rtos def 2 2) ">: ")))
        (setenv "LMac\\boxtext-off" (rtos off 2 2))
        (setq off def)
    )
    
    (LM:startundo (LM:acdoc))
    (if (setq sel (LM:ssget "\nSelect text or mtext <exit>: " '(((0 . "TEXT,MTEXT")))))
        (repeat (setq idx (sslength sel))
            (setq enx (entget (ssname sel (setq idx (1- idx))))
                  lst (text-box-off enx (* off (cdr (assoc 40 enx))))
            )
            (entmake
                (append
                   '(
                        (000 . "LWPOLYLINE")
                        (100 . "AcDbEntity")
                        (100 . "AcDbPolyline")
                        (090 . 4)
                        (070 . 1)
                    )
                    (LM:defaultprops enx)
                    (list (cons  038 (caddar lst)))
                    (mapcar '(lambda ( x ) (cons 10 x)) lst)
                    (list (assoc 210 enx))
                )
            )
        )
    )
    (LM:endundo (LM:acdoc))
    (princ)
)

;; ssget  -  Lee Mac
;; A wrapper for the ssget function to permit the use of a custom selection prompt
;; msg - [str] selection prompt
;; arg - [lst] list of ssget arguments

(defun LM:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;; Default Properties  -  Lee Mac
;; Returns a list of DXF properties for the supplied DXF data,
;; substituting default values for absent DXF groups
 
(defun LM:defaultprops ( enx )
    (mapcar '(lambda ( x ) (cond ((assoc (car x) enx)) ( x )))
       '(
            (006 . "BYLAYER")
            (008 . "0")
            (039 . 0.0)
            (048 . 1.0)
            (062 . 256)
            (370 . -1)
        )
    )
)

;; Text Box  -  gile / Lee Mac
;; Returns an OCS point list describing a rectangular frame surrounding
;; the supplied text or mtext entity with optional offset
;; enx - [lst] Text or MText DXF data list
;; off - [rea] offset (may be zero)

(defun text-box-off ( enx off / bpt hgt jus lst ocs org rot wid )
    (cond
        (   (= "TEXT" (cdr (assoc 00 enx)))
            (setq bpt (cdr (assoc 10 enx))
                  rot (cdr (assoc 50 enx))
                  lst (textbox enx)
                  lst
                (list
                    (list (- (caar  lst) off) (- (cadar  lst) off)) (list (+ (caadr lst) off) (- (cadar  lst) off))
                    (list (+ (caadr lst) off) (+ (cadadr lst) off)) (list (- (caar  lst) off) (+ (cadadr lst) off))
                )
            )
        )
        (   (= "MTEXT" (cdr (assoc 00 enx)))
            (setq ocs  (cdr (assoc 210 enx))
                  bpt  (trans (cdr (assoc 10 enx)) 0 ocs)
                  rot  (angle '(0.0 0.0) (trans (cdr (assoc 11 enx)) 0 ocs))
                  wid  (cdr (assoc 42 enx))
                  hgt  (cdr (assoc 43 enx))
                  jus  (cdr (assoc 71 enx))
                  org  (list (cond ((member jus '(2 5 8)) (/ wid -2.0)) ((member jus '(3 6 9)) (- wid))      (0.0))
                             (cond ((member jus '(1 2 3)) (- hgt))      ((member jus '(4 5 6)) (/ hgt -2.0)) (0.0))
                       )
                  lst
                (list
                    (list (- (car org) off)     (- (cadr org) off))     (list (+ (car org) wid off) (- (cadr org) off))
                    (list (+ (car org) wid off) (+ (cadr org) hgt off)) (list (- (car org) off)     (+ (cadr org) hgt off))
                )
            )
        )
    )
    (if lst
        (   (lambda ( m ) (mapcar '(lambda ( p ) (mapcar '+ (mxv m p) bpt)) lst))
            (list
                (list (cos rot) (sin (- rot)) 0.0)
                (list (sin rot) (cos rot)     0.0)
               '(0.0 0.0 1.0)
            )
        )
    )
)
 
;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n
 
(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
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
        "\n:: BoxText.lsp | Version 1.2 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"bt\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;