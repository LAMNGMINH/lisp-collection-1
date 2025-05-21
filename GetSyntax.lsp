;;----------------------=={ Get Syntax }==--------------------;;
;;                                                            ;;
;;  Prints the syntax of all defined commands in a selected   ;;
;;  LISP file.                                                ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:GetSyntax ( / *error* PadLeft f ) (vl-load-com)
  ;; © Lee Mac 2011

  (defun *error* ( msg )
    (if (and file (eq 'FILE (type file))) (close file))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (defun PadLeft ( string char len )
    (vl-list->string
      ( (lambda ( l ) (while (< (length l) len) (setq l (cons char l))) l)
        (vl-string->list string)
      )
    )
  )

  (and
    (setq f (getfiled "Select LISP File to Query" "" "lsp" 16))
    (princ  (PadLeft "" 45 60))
    (princ  (strcat "\n Command Syntax for " (vl-filename-base f) ".lsp: \n"))
    (princ  (PadLeft "" 45 60))
    (progn
      (if (not (mapcar '(lambda ( x ) (princ "\n   ") (princ x)) (LM:GetSyntax f)))
        (princ "\n -None-\n")
      )
      (princ (strcat "\n" (PadLeft "" 45 60)))
      (textpage)
    )
  )

  (princ)
)

;;--------------------=={ Get Syntax }==----------------------;;
;;                                                            ;;
;;  Returns a list of syntax for all defined commands in a    ;;
;;  supplied LISP file.                                       ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  file - filename of LISP file to read                      ;;
;;------------------------------------------------------------;;
;;  Returns:  List of defined commands in supplied LISP file  ;;
;;------------------------------------------------------------;;

(defun LM:GetSyntax ( file / _GetSyntax line syntax )

  (defun _GetSyntax ( p s / x )
    (if (setq x (vl-string-search p s))
      (cons
        (substr (setq s (substr s (+ x 1 (strlen p)))) 1
          (setq x
            (car
              (vl-sort
                (vl-remove 'nil
                  (mapcar
                    (function
                      (lambda ( d ) (vl-string-position d s))
                    )
                   '(32 9 40 41)
                  )
                )
                '<
              )
            )
          )
        )
        (if x (_GetSyntax p (substr s (1+ x))))
      )
    )
  ) 

  (if (setq file (open file "r"))
    (apply 'append
      (progn
        (while (setq line (read-line file))
          (setq syntax (cons (_GetSyntax "(DEFUN C:" (strcase line)) syntax))
        )
        (setq file (close file)) (reverse syntax)
      )
    )
  )
)

(princ)
(princ "\n:: GetSyntax.lsp | © Lee Mac 2011 www.lee-mac.com ::")
(princ "\n:: Type \"GetSyntax\" to query a LISP file for defined commands ::")
(princ)

;;------------------------------------------------------------;;
;;                      End of File                           ;;
;;------------------------------------------------------------;;
