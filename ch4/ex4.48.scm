(load "./4.3")

(define adjectives '(adjective small big fast slow)) 

(define (parse-simple-noun-phrase)       
	(amb (list 'simple-noun-phrase 
		(parse-word articles) 
		(parse-word nouns)) 
	(list 'simple-noun-phrase 
		(parse-word articles) 
		(parse-word adjectives) 
		(parse-word nouns)))) 


(parse '(the student with the small cat sleeps in the class))