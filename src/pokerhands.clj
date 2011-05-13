(ns pokerhands
  (:use [lazytest.describe])
  (:use [lazytest.expect.thrown]))

(defstruct card :rank :suit)

(def *suits* [:clubs :diamonds :hearts :spades])
(def *ranks* [:two :three :four :five :six :seven :eight :nine :ten :jack :queen :king :ace])

(def *rank-mappings* (zipmap [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A] *ranks*))
(def *suit-mappings* (zipmap [\c \d \h \s] *suits*))

(defn- find-rank
  "Determines the card rank corresponding to the string value."
  [char]
  (or (get *rank-mappings* char)
      (throw (IllegalArgumentException. "Not a valid card - rank not found"))))

(defn- find-suit
  "Determines the card suit corresponding to the input character."
  [char]
  (or (get *suit-mappings* char)
      (throw (IllegalArgumentException. "Not a valid card - suit not found"))))

(defn make-card [[rank suit]]
  (struct card (find-rank rank) (find-suit suit)))

(describe make-card
          (it "creates a map representation of a card from 2c"
              (= {:suit :clubs :rank :two} (make-card "2c")))
          (it "creates a map representation of a card from 9s"
              (= {:suit :spades :rank :nine} (make-card "9s")))
          (it "creates a map representation of a card from Qs"
              (= {:suit :spades :rank :queen} (make-card "Qs")))
          (it "creates a map representation of a card from 11h"
              (throws? java.lang.IllegalArgumentException #(make-card "11h"))) 
          (it "creates a map representation of a card from Th"
              (= {:suit :hearts :rank :ten} (make-card "Th")))) 

(defn- index-of
  "searches for sought in sequence, returning index in sequence or nil if not found"
  [sought sequence]
  (loop [index 0 remainder sequence]
    (if (empty? remainder) nil
      (if (= sought (first remainder)) index
        (recur (inc index) (rest remainder))))))

(defn- rank-value [card]
  (index-of (:rank card) *ranks*))

(defn- suit-value [card]
  (index-of (:suit card) *suits*))

(defn- highest-suit
  "Returns the card with the highest suit"
  [cards]
  (last (sort-by suit-value cards)))

(describe highest-suit
          (it "should rank diamonds over clubs"
              (= :diamonds (:suit (highest-suit (map make-card '("2d" "5c"))))))
          (it "should rank hearts over diamonds"
              (= :hearts   (:suit (highest-suit (map make-card '("9d" "3h"))))))
          (it "should rank spades over hearts" 
              (= :spades   (:suit (highest-suit (map make-card '("Qs" "Kh"))))))
          (it "should rank spades over diamonds"
              (= :spades   (:suit (highest-suit (map make-card '("2s" "5d"))))))
          (it "should rank spades highest"
              (= :spades   (:suit (highest-suit (map make-card '("2d" "5c" "Kc" "Ah" "5s" "2h")))))))

(defn highest-card
  "returns the highest card"
  ([cards]
     (reduce highest-card cards))
  ([card1 card2]
     (if (= (rank-value card1) (rank-value card2))
       (highest-suit (list card1 card2))
       (if (>  (rank-value card1) (rank-value card2)) card1 card2))))

(describe highest-card
          (it "should return the only card when given one card"
              (= (make-card "2c") (highest-card (list (make-card "2c")))))
          (it "should return the card with the highest value when all cards are of the same suit"
              (= :three (:rank (highest-card (list (make-card "2c") (make-card "3c"))))))
          (it "should return the car d with the highest suit when all cards are of the same value"
              (= :diamonds (:suit (highest-card (list (make-card "2d") (make-card "2c")))))))

(defn classify-hand
  "returns a classification of the value of the hand"
  [hand]
  :highest-card)

(describe classify-hand
          (it "should classify hand as :highest-card if no better match could be found"
              (= :highest-card (classify-hand '("2c" "3c" "4c" "5c" "9s")))))

