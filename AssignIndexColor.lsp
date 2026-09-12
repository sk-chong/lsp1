;; ============================================================================
;; Command: ICO (Standard Index Color Dialog) & Shortcuts (c1 - c8, cbl)
;; Description: Provides a DCL dialog box interface with clickable color 
;;              buttons to quickly apply standard AutoCAD Index Colors 
;;              (1 to 7, Grey 8, and ByLayer) to selected objects. 
;;              Individual shortcuts (c1 through c8, cbl) are also provided.
;; ============================================================================

(vl-load-com)

;; =========================================================================
;; BACKGROUND PROCESSOR: Applies Standard AutoCAD Color Index
;; =========================================================================
(defun ICO_Apply (cNum / ss i ent obj)
  (princ "\nSelect objects to apply Standard Color... ")
  ;; _:L excludes locked layers automatically
  (if (setq ss (cond ((ssget "_I")) ((ssget "_:L"))))
    (progn
      (command "_.undo" "_be")
      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))
        (setq obj (vlax-ename->vla-object ent))
        (vl-catch-all-apply 'vla-put-color (list obj cNum))
        (vla-update obj)
      )
      (command "_.undo" "_end")
      (princ (strcat "\nApplied Standard Color Index (" (itoa cNum) ") to selection."))
    )
  )
  (princ)
)

;; =========================================================================
;; BACKGROUND PROCESSOR: Set Color to ByLayer
;; =========================================================================
(defun ICO_ByLayer ( / ss i ent obj)
  (princ "\nSelect objects to set Color to ByLayer... ")
  (if (setq ss (cond ((ssget "_I")) ((ssget "_:L"))))
    (progn
      (command "_.undo" "_be")
      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))
        (setq obj (vlax-ename->vla-object ent))
        (vl-catch-all-apply 'vla-put-color (list obj 256)) ; 256 = acByLayer
        (vla-update obj)
      )
      (command "_.undo" "_end")
      (princ (strcat "\nSet Color to ByLayer for " (itoa (sslength ss)) " object(s)."))
    )
  )
  (princ)
)

;; =========================================================================
;; COMMAND SHORTCUTS: c1 to c8 & cbl
;; =========================================================================
(defun c:c1 () (ICO_Apply 1))   ; Red
(defun c:c2 () (ICO_Apply 2))   ; Yellow
(defun c:c3 () (ICO_Apply 3))   ; Green
(defun c:c4 () (ICO_Apply 4))   ; Cyan
(defun c:c5 () (ICO_Apply 5))   ; Blue
(defun c:c6 () (ICO_Apply 6))   ; Magenta
(defun c:c7 () (ICO_Apply 7))   ; White
(defun c:c8 () (ICO_Apply 8))   ; Grey (ACI 8)
(defun c:cbl () (ICO_ByLayer))  ; Color ByLayer 

;; =========================================================================
;; MAIN COMMAND WITH STANDARD COLOR DCL IMAGES: "ICO"
;; =========================================================================
(defun c:ICO ( / dclFile f dclId choice)
  
  ;; 1. Automatically generate the independent DCL structure file with uniform single-row layout
  (setq dclFile (vl-filename-mktemp "ico_colors.dcl"))
  (setq f (open dclFile "w"))
  
  (write-line "ico_dialog : dialog { " f)
  (write-line "    label = \"Select Standard AutoCAD Color / ByLayer\"; " f)
  (write-line "    : boxed_column { " f)
  (write-line "        label = \"Available Palettes\";" f)
  (write-line "        : row { " f)
  (write-line "            alignment = centered; " f)
  (write-line "            : column { alignment = centered; : image_button { key = \"col1\"; width = 4; height = 3; fix_width = true; fix_height = true; } : text { label = \"Red\"; width = 6; alignment = centered; } }" f)
  (write-line "            : column { alignment = centered; : image_button { key = \"col2\"; width = 4; height = 3; fix_width = true; fix_height = true; } : text { label = \"Yellow\"; width = 6; alignment = centered; } }" f)
  (write-line "            : column { alignment = centered; : image_button { key = \"col3\"; width = 4; height = 3; fix_width = true; fix_height = true; } : text { label = \"Green\"; width = 6; alignment = centered; } }" f)
  (write-line "            : column { alignment = centered; : image_button { key = \"col4\"; width = 4; height = 3; fix_width = true; fix_height = true; } : text { label = \"Cyan\"; width = 6; alignment = centered; } }" f)
  (write-line "            : column { alignment = centered; : image_button { key = \"col5\"; width = 4; height = 3; fix_width = true; fix_height = true; } : text { label = \"Blue\"; width = 6; alignment = centered; } }" f)
  (write-line "            : column { alignment = centered; : image_button { key = \"col6\"; width = 4; height = 3; fix_width = true; fix_height = true; } : text { label = \"Magenta\"; width = 6; alignment = centered; } }" f)
  (write-line "            : column { alignment = centered; : image_button { key = \"col7\"; width = 4; height = 3; fix_width = true; fix_height = true; } : text { label = \"White\"; width = 6; alignment = centered; } }" f)
  (write-line "            : column { alignment = centered; : image_button { key = \"col8\"; width = 4; height = 3; fix_width = true; fix_height = true; } : text { label = \"Grey\"; width = 6; alignment = centered; } }" f)
  (write-line "            : column { alignment = centered; : image_button { key = \"col9\"; width = 4; height = 3; fix_width = true; fix_height = true; } : text { label = \"ByLayer\"; width = 6; alignment = centered; } }" f)
  (write-line "        } " f)
  (write-line "    } " f)
  (write-line "    : spacer { height = 1; } " f)
  (write-line "    ok_cancel; " f)
  (write-line "}" f)
  (close f)

  ;; 2. Load the newly written DCL panel layout into memory
  (setq dclId (load_dialog dclFile))
  (if (not (new_dialog "ico_dialog" dclId))
    (progn
      (princ "\nError: Unable to load Standard Color Dialog Panel.")
      (exit)
    )
  )

  ;; 3. Paint each image tile explicitly inline using standard ACI colors
  (start_image "col1") (fill_image 0 0 (dimx_tile "col1") (dimy_tile "col1") 1) (end_image) ; Red
  (start_image "col2") (fill_image 0 0 (dimx_tile "col2") (dimy_tile "col2") 2) (end_image) ; Yellow
  (start_image "col3") (fill_image 0 0 (dimx_tile "col3") (dimy_tile "col3") 3) (end_image) ; Green
  (start_image "col4") (fill_image 0 0 (dimx_tile "col4") (dimy_tile "col4") 4) (end_image) ; Cyan
  (start_image "col5") (fill_image 0 0 (dimx_tile "col5") (dimy_tile "col5") 5) (end_image) ; Blue
  (start_image "col6") (fill_image 0 0 (dimx_tile "col6") (dimy_tile "col6") 6) (end_image) ; Magenta
  (start_image "col7") (fill_image 0 0 (dimx_tile "col7") (dimy_tile "col7") 7) (end_image) ; White
  (start_image "col8") (fill_image 0 0 (dimx_tile "col8") (dimy_tile "col8") 8) (end_image) ; Grey (ACI 8)
  (start_image "col9") (fill_image 0 0 (dimx_tile "col9") (dimy_tile "col9") 7) (end_image) ; ByLayer swatch

  ;; 4. Assign clickable routing actions to each colored tile
  (action_tile "col1" "(done_dialog 1)")
  (action_tile "col2" "(done_dialog 2)")
  (action_tile "col3" "(done_dialog 3)")
  (action_tile "col4" "(done_dialog 4)")
  (action_tile "col5" "(done_dialog 5)")
  (action_tile "col6" "(done_dialog 6)")
  (action_tile "col7" "(done_dialog 7)")
  (action_tile "col8" "(done_dialog 8)")
  (action_tile "col9" "(done_dialog 9)")
  (action_tile "cancel" "(done_dialog 0)")

  ;; 5. Run dialog loop interface structure
  (setq choice (start_dialog))
  (unload_dialog dclId)
  
  ;; 6. Clean up temporary hard drive files safely
  (vl-file-delete dclFile)

  ;; 7. Execute standard color commands based on button click mapping
  (cond
    ((= choice 1) (c:c1))
    ((= choice 2) (c:c2))
    ((= choice 3) (c:c3))
    ((= choice 4) (c:c4))
    ((= choice 5) (c:c5))
    ((= choice 6) (c:c6))
    ((= choice 7) (c:c7))
    ((= choice 8) (c:c8))
    ((= choice 9) (c:cbl))
  )
  
  (princ)
)