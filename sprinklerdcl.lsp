(defun c:SprinklerData ()
   (command "OpenDCL")
   (Setq rValue (dcl_Project_Load "D:\\Work\\2043 France block\\Sprinkler" ));T
   (if (not (dcl_Form_IsActive Sprinkler_Form1))
      (progn
         (Setq rValue (dcl_Form_Show Sprinkler_Form1))
      )
   )
)