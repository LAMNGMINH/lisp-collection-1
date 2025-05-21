;;----------------------=={ AutoLoader }==--------------------;;
;;                                                            ;;
;;  Prompts for a directory selection and proceeds to write   ;;
;;  AutoLoad statements for all LISP files found in the       ;;
;;  selected directory to an output text file.                ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:AutoLoader ( / *error* dir lst out file ) (vl-load-com)

  (defun *error* ( msg )
    (if (and file (eq 'FILE (type file))) (close file))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (if
    (and
      (setq dir
        (LM:DirectoryDialog
          (strcat
            "Select Directory of LISP files for which to generate AutoLoad expressions.\n"
            "\nNote: Subdirectories of the selected directory will not be processed."
          )
          nil 832
        )
      )
      (setq lst  (vl-directory-files (setq dir (vl-string-translate "\\" "/" dir)) "*.lsp" 1))
      (setq out  (getfiled "Create Output File" "" "txt" 1))
      (setq file (open out "w"))
    )
    (progn
      (mapcar
        (function
          (lambda ( lsp / syn )
            (if (setq syn (LM:GetSyntax (strcat dir "/" lsp)))
              (write-line
                (strcat "(autoload "
                  (vl-prin1-to-string (strcat dir "/" lsp)) " '" (vl-prin1-to-string syn) ")"
                )
                file
              )
            )
          )
        )
        lst
      )
      (setq file (close file)) (startapp "notepad" out)
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

;;-------------------=={ Directory Dialog }==-----------------;;
;;                                                            ;;
;;  Displays a dialog prompting the user to select a folder   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  msg  - message to display at top of dialog                ;;
;;  dir  - root directory (or nil)                            ;;
;;  flag - bit coded flag specifying dialog display settings  ;;
;;------------------------------------------------------------;;
;;  Returns:  Selected folder filepath, else nil              ;;
;;------------------------------------------------------------;;

(defun LM:DirectoryDialog ( msg dir flag / Shell HWND Fold Self Path ac )
  (vl-load-com)
  ;; © Lee Mac 2010

  (setq Shell (vla-getInterfaceObject (setq ac (vlax-get-acad-object)) "Shell.Application")
        HWND  (vl-catch-all-apply 'vla-get-HWND (list ac))
        Fold  (vlax-invoke-method Shell 'BrowseForFolder (if (vl-catch-all-error-p HWND) 0 HWND)  msg flag dir))
  (vlax-release-object Shell)
  
  (if Fold
    (progn
      (setq Self (vlax-get-property Fold 'Self) Path (vlax-get-property Self 'Path))
      (vlax-release-object Self)
      (vlax-release-object Fold)      
      
      (and (= "\\" (substr Path (strlen Path)))
           (setq Path (substr Path 1 (1- (strlen Path)))))
    )
  )
  Path
)

(princ)
(princ "\n:: AutoLoader.lsp | © Lee Mac 2011 www.lee-mac.com ::")
(princ "\n:: Type \"AutoLoader\" to generate autoload expressions ::")
(princ)

;;------------------------------------------------------------;;
;;                      End of File                           ;;
;;------------------------------------------------------------;;