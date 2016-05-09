(ns ranmel
	(:use overtone.live)
  (:use 
    leipzig.canon
    leipzig.chord
    leipzig.melody
    leipzig.temperament
    [overtone.samples.piano :only [index-buffer]]
    )
  (:require leipzig.scale)
  (:use leipzig.live)
  
)

(defn -main "Main" []

  (defn rand-notes "Return a seq of length n, filled with random note in the scale starting at root."
    [_root _scale n]
    (repeatedly n 
	  (fn ra [] (rand-nth 
	  (concat (scale _root _scale) (repeatedly 2 #(eval nil)))
	  )) ))

  (defn melody [len] "A simple melody built from durations and pitches."
	  (->> (phrase (repeatedly len #(eval 1))
		  (rand-notes (rand-nth '(:c5)) :major len)) ; pitch
	  (where :part (is :melody)) )
  )
  (defn melody-low [len] "A simple melody built from durations and pitches."
	  (->> (phrase (repeatedly len #(eval 1))
		  (rand-notes (rand-nth '(:c2)) :major len)) ; pitch
	  (where :part (is :melody)) )
  )
  (defn melody-mid [len] "A simple melody built from durations and pitches."
	  (->> (phrase (repeatedly len #(eval 1))
		  (rand-notes (rand-nth '(:c4 :c5)) :major len)) ; pitch
	  (where :part (is :melody)) )
  )
  
  
  (definst sampled-piano
    [note 60 level 0.4 rate 1 loop? 0
    attack 0 decay 1 sustain 1 release 0 curve -4 gate 1]
    (let [buf (index:kr (:id index-buffer) note)
	  env (env-gen (adsr attack decay sustain release level curve)
		      :gate gate
		      :action FREE)]
      (* env (scaled-play-buf 2 buf :level level :loop loop? :action FREE))))
      
    
  (definst sin-wave [freq 440 length 1 sustain 0.9 attack 0.01 release 0.1 vol 1] 
    (* (env-gen (lin attack sustain release) 1 1 0 length FREE)
	(sin-osc freq)
	vol))
	
	
	
  (defn play-m [melody pf speed]
    (let [beat ((metronome speed))
	  mel (melody (int (rand 16))) ]
      (doseq [note mel]
	((fn [{midi :pitch time :time duration :duration}] 
	  (at 
	    ((metronome speed) (+ time beat)) 
	    (pf midi ;(midi->hz midi) 
	     ;(float (/ ((bpm ((metronome speed) :bpm)) duration) 1000)) 
	      )))
	note ) )
	(apply-at ((metronome speed) (+ (count mel) beat)) #'play-m melody pf (+ 60 (rand 220)) [])
	))
	
  ;(play-m melody sampled-piano 999)
  (play-m melody-mid sampled-piano 999)
)