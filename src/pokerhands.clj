(ns pokerhands
  (:use [lazytest.describe])
  (:use [lazytest.expect.thrown]))

(declare make-card find-suit)

(def *card-suits*
  (zipmap [\c \d \h \s]
          [:clubs :diamonds :hearts :spades]))

(def *card-ranks*
  (zipmap ["2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"]
          (range 2 15)))

(defn find-rank
  "Determines the card rank corresponding to the string value."
  [value]
  (or (get *card-ranks* value)
    (throw (IllegalArgumentException. "Not a valid card"))))

(defn find-suit
  "translates character to suit"
  [char]
  (get *card-suits* char))

(defn classify-hand
  "returns a classification of the value of the hand"
  [hand]
  :highest-card)

(defn index-of
  "searches for sought in sequence, returning index in sequence or nil if not found"
  [sought sequence]
  (loop
      [index 0
       remainder sequence]
    (if (empty? remainder)
      nil
      (if (= sought (first remainder))
        index
        (recur (inc index) (rest remainder))))))
  

(describe classify-hand
          (it "should classify hand as :highest-card if no better match could be found"
              (= :highest-card (classify-hand '("2c" "3c" "4c" "5c" "9s")))))

(defn suit-index
  "Returns the numerical value of the card's suit from clubs to spades."
  [card]
  (index-of (:suit card) (vals *card-suits*)))

(defn highest-suit
  "Returns the card with the highest suit"
  [cards]
  (first (sort-by suit-index cards)))

(describe highest-suit
          (it "should rank diamonds over clubs"
              (= (make-card "2d") (highest-suit (map make-card '("2d" "5c")))))
          (it "should rank hearts over diamonds"
              (= (make-card "3h") (highest-suit (map make-card '("9d" "3h")))))
          (it "should rank spades over hearts"
              (= (make-card "Qs") (highest-suit (map make-card '("Qs" "Kh")))))
          (it "should rank spades over diamonds"
              (= (make-card "2s") (highest-suit (map make-card '("2s" "5d")))))
          (it "should rank spades highest"
              (= (make-card "5s") (highest-suit (map make-card '("2d" "5c" "Kc" "Ah" "5s" "2h"))))))

(defn highest-card
  "returns the highest card"
  ([cards]
     (reduce highest-card cards))
  ([card1 card2]
     (if (= (:value card1) (:value card2))
       (highest-suit (list card1 card2))
       (if (> (:value card1) (:value card2)) card1 card2))))

(describe highest-card
          (it "should return the only card when given one card"
              (= (make-card "2c") (highest-card (list (make-card "2c")))))
          (it "should return the card with the highest value when all cards are of the same suit"
              (= (make-card "3c") (highest-card (map make-card '("2c" "3c")))))
          (it "should return the card with the highest suit when all cards are of the same value"
              (= (make-card "2d") (highest-card (map make-card '("2d" "2c"))))))

(defn make-card
  "creates a card" 
  [card]
  {:suit (find-suit (last card)) :value (find-rank (apply str (butlast card)))})

(describe make-card
          (it "creates a map representation of a card from 2c"
              (= {:suit :clubs :value 2} (make-card "2c")))
          (it "creates a map representation of a card from 9s"
              (= {:suit :spades :value 9} (make-card "9s")))
          (it "creates a map representation of a card from Qs"
              (= {:suit :spades :value 12} (make-card "Qs")))
          (it "creates a map representation of a card from 11h"
              (throws? java.lang.IllegalArgumentException #(make-card "11h"))) 
          (it "creates a map representation of a card from 10h"
              (= {:suit :hearts :value 10} (make-card "10h")))) 
