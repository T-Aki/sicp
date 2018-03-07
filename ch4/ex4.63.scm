;ex4.63.scm

(rule (son-of ?s ?f) 
      (or
        (son ?f ?s)
        (and 
          (son ?w ?s)
          (wife ?f ?w))))

(rule (grandson-of ?g ?s) 
	(and 
   (father ?s ?f)
      (father ?f ?g)))