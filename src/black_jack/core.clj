(ns learning-clojure.core)
(require '[clojure.string :as string])

(def deck (for [suite [:Spades :Diamonds :Hearts :Clubs] rank (range 1 14)] [rank suite]))

(defn card-point-value "Calculate card values as tuplet, ace is special case where values differ."
  [card]
  (let [[rank _suite] card]
    (cond
      (<= 2 rank 10) [rank rank]
      (< rank 2) [rank 11]
      :else [10 10])))

(defn card-to-string "Return the card as a string"
  [card]
  (let [[rank suite] card]
    (cond
      (= rank 1) (str "Ace of " (name suite))
      (<= 2 rank 10) (str rank " of " (name suite))
      (= rank 11) (str "Jack of " (name suite))
      (= rank 12) (str "Queen of " (name suite))
      (= rank 13) (str "King of " (name suite)))))

(defn random-card
  "Return the position of a random card"
  [deck] (rand-int (count deck)))

(defn remove-card
  "Remove a card from the deck"
  [card deck] (remove #(= card %) deck))

(defn take-random-card
  "Take a random card from the deck and return [card, deck minus the card]"
  [deck] (let [pos (random-card deck)
               card (nth deck pos)
               remainder (remove-card card deck)]
           [card remainder]))

(defn shuffle-deck
  "Shuffle the deck."
  [deck]
  (loop [deck deck acc ()]
    (if-not (seq deck)
      acc
      (let [[card, remainder] (take-random-card deck)]
        (recur remainder (conj acc card))))))

(defn take-card "Take the first card from the deck and return [card, deck minus the card]"
  [deck] (let [card (first deck)]
           [card (rest deck)]))

(defn deal-card-to-player "Take a card from deck and add it to a hand"
  [game player]
  (let [[delt-card remainder] (take-card (:current-deck game))
        player-hand (:player-hand game)
        dealer-hand (:dealer-hand game)]
    (if (= player :player-hand)
      (assoc game :current-deck remainder :player-hand (conj player-hand delt-card))
      (assoc game :current-deck remainder :dealer-hand (conj dealer-hand delt-card)))))

(defn deal-opening-hand "Deal opening hand to player and dealer"
  [game]
  (loop [game game ctr 0]
    (if (= ctr 2)
      game
      (let [player-deal (deal-card-to-player game :player-hand)
            current-game (deal-card-to-player player-deal :dealer-hand)]
        (recur current-game (inc ctr))))))

(defn calculate-points "Calculate hand, ace is high or low. Returns a vector of 2 possible scores."
  [hand]
  (let [points (reduce (fn [[a b] [c d]] [(+ a c) (+ b d)]) [0 0]
                       (map #(card-point-value %) hand))]
    (map (fn [val] (if (< 21 val) :bust val)) points)))

(defn new-game [] {:current-deck (shuffle-deck deck)
                   :player-hand []
                   :dealer-hand []})

(defn hand-to-string "Return the hand as a string with possible points."
  [hand]
  (let [card-str (string/join ", " (map card-to-string hand))
        [l h] (calculate-points hand)]
    (cond
      (= l h) (str "cards: " card-str " -- worth: " h)
      (= h :bust) (str "cards: " card-str " --worth: " l)
      :else (str "cards: " card-str " -- worth: " h " or " l))))

(defn render-hands "Print the hands in a game"
  [game]
  (let [player-hand (:player-hand game)
        dealer-hand (:dealer-hand game)]
    (println "Your hand -> " (hand-to-string player-hand))
    (println "Dealers hand -> " (hand-to-string dealer-hand))))

(defn busted-hand "Check if a hand is busted"
  [hand]
  (let [[l h] (calculate-points hand)]
    (= l h :bust)))

(defn highest-score "Determine the highest score available among possible scores"
  [score]
  (reduce #(if (> %1 %2) %1 %2) (filter #(not= :bust %) score)))

(defn player-round "The players turn of the hand"
  [game]
  (let [cmd (read-line)]
    (if (= cmd "stick")
      game
      (cond
        (not= cmd "hit")
        (do
          (println "what?...you can only stick or hit.")
          (recur game))
        :else (let [updated-game (deal-card-to-player game :player-hand)]
                (if (busted-hand (:player-hand updated-game))
                  (do
                    (render-hands updated-game)
                    (println "Bad Luck -- you lost this hand!")
                    updated-game)
                  (do
                    (render-hands updated-game)
                    (recur updated-game))))))))

(defn dealer-round "The dealers turn of the hand"
  [game]
  (if (not (busted-hand (:dealer-hand game)))
    (let [highest-value-player (highest-score (calculate-points (:player-hand game)))
          dealer-points (calculate-points (:dealer-hand game))]
      (cond
        (> highest-value-player (highest-score dealer-points))
        (let [updated-game (deal-card-to-player game :dealer-hand)]
          (do
            (Thread/sleep 2000)
            (println "Dealer draws...")
            (render-hands updated-game)
            (recur updated-game)))
        :else
        (do
          (render-hands game)
          (println "Dealer wins the hand!")
          )))
    (do
      (println "Dealer bust! You win this hand."))))


(defn black-jack "A hand of black-jack"
  []
  (let [game (deal-opening-hand(new-game))]
    (do
      (render-hands game)
      (println "Its your turn... hit/stick")
      (let [player-turn (player-round game)]
        (if (busted-hand (:player-hand player-turn))
          (do
            (println "Dealer wins this hand!"))
          (dealer-round player-turn))))))

(defn -main [& args] ())

(-main)

