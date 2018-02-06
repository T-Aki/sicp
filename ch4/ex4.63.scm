;ex4.63.scm

(rule (father ?s ?f) 
	(or (son ?f ?s) 
		(and (son ?w ?s) 
			(wife ?f ?w))))

(rule (grandson ?g ?s) 
	(and (father ?s ?f) 
		(father ?f ?g)))