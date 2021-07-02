(ns black-jack.core)
(require '[clojure.string :as string])

;cards.png is split in to 5 rows
;clubs
;diamonds
;hearts
;spades
;jokers and card back
(def image-file-path "Image file for all cards"  "./resources/cards.png")

(def card-width  "card width in pixels" 79)

(def card-height "card height in pixels" 123)

(def deck "Deck of cards. The suite order matches that in the image file"
  (for [suite [:Clubs :Diamonds :Hearts :Spades] rank (range 1 14)] [rank suite]))

(def card-coord-map "Image coords zipped with the deck."
  (zipmap deck (for [y (range 0 (* 4 card-height) card-height)
                     x (range 0 (* 13 card-width) card-width)]
                 [x y])))

(defn card-point-value
  "Calculate card values as tuplet, ace is special case where values differ."
  [card]
  (let [[rank _suite] card]
    (cond
      (<= 2 rank 10) [rank rank]
      (< rank 2) [rank 11]
      :else [10 10])))

(defn card-to-string
  "Return the card as a string"
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

(defn take-card
  "Take the first card from the deck and return [card, deck minus the card]"
  [deck] (let [card (first deck)]
           [card (rest deck)]))

(defn deal-card-to-player
  "Take a card from deck and add it to a hand"
  [game player]
  (let [[delt-card remainder] (take-card (:current-deck game))
        player-hand (:player-hand game)
        dealer-hand (:dealer-hand game)]
    (if (= player :player-hand)
      (assoc game :current-deck remainder :player-hand (conj player-hand delt-card))
      (assoc game :current-deck remainder :dealer-hand (conj dealer-hand delt-card)))))

(defn deal-opening-hand
  "Deal opening hand to player and dealer"
  [game]
  (loop [game game ctr 0]
    (if (= ctr 2)
      game
      (let [player-deal (deal-card-to-player game :player-hand)
            current-game (deal-card-to-player player-deal :dealer-hand)]
        (recur current-game (inc ctr))))))

(defn calculate-points
  "Calculate hand, ace is high or low. Returns a vector of 2 possible scores."
  [hand]
  (let [points (reduce (fn [[a b] [c d]] [(+ a c) (+ b d)]) [0 0]
                       (map #(card-point-value %) hand))]
    (map (fn [val] (if (< 21 val) :bust val)) points)))

(defn new-game
  "Prepare the starting game representation"
  [] {:current-deck (shuffle-deck deck)
      :player-hand []
      :dealer-hand []})

(defn hand-to-string
  "Return the hand as a string with possible points."
  [hand]
  (let [card-str (string/join ", " (map card-to-string hand))
        [l h] (calculate-points hand)]
    (cond
      (= l h) (str "cards: " card-str " -- worth: " h)
      (= h :bust) (str "cards: " card-str " -- worth: " l)
      :else (str "cards: " card-str " -- worth: " h " or " l))))

(defn render-game
  "Render the current state of the game"
  [game]
  (let [player-hand (:player-hand game)
        dealer-hand (:dealer-hand game)]
    (println "Your hand -> " (hand-to-string player-hand))
    (println "Dealers hand -> " (hand-to-string dealer-hand))))

(defn busted-hand
  "Check if a hand is busted"
  [hand]
  (let [[l h] (calculate-points hand)]
    (= l h :bust)))

(defn highest-score
  "Determine the highest score available among possible scores"
  [score]
  (->> score
       (filter #(not= :bust %))
       (reduce #(if (> %1 %2) %1 %2))))

(defn game-command
  "Change game state based on player command"
  [game cmd]
  (if (= cmd :Stick)
    game
    (deal-card-to-player game :player-hand)))

(defn player-round
  "The players turn of the hand"
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
                    (render-game updated-game)
                    (println "Bad Luck -- you lost this hand!")
                    updated-game)
                  (do
                    (render-game updated-game)
                    (recur updated-game))))))))

(defn dealer-round
  "The dealers turn of the hand"
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
            (render-game updated-game)
            (recur updated-game)))
        :else
        (do
          (render-game game)
          (println "Dealer wins the hand!"))))
    (do
      (println "Dealer bust! You win this hand."))))

(defn black-jack
  "A hand of black-jack"
  []
  (let [game (deal-opening-hand (new-game))]
    (do
      (render-game game)
      (println "Its your turn... hit/stick")
      (let [player-turn (player-round game)]
        (if (busted-hand (:player-hand player-turn))
          (do
            (println "Dealer wins this hand!"))
          (dealer-round player-turn))))))


;;;;;;;;;;;GUI SECTION;;;;;;;;;;;


(defn load-sprite-sheet "load the card png"
  []
  (let [file (new java.io.File image-file-path)]
    (javax.imageio.ImageIO/read file)))

(defn hand-to-subimage-coords "translate hand to subimage x and y"
  [hand]
  (map card-coord-map hand))

(defn get-subimages "cut the subimages from the spritemap"
  [coords]
  (map (fn [[x y]] (.getSubimage (load-sprite-sheet) x y card-width card-height)) coords))

(defn image-x-pos "calculate the range of x positions to draw cards in a hand"
  [n-cards]
  (range 10 (* n-cards 20) 20))

(defn draw-hand "Render a players hand"
  [gfx player hand]
  (let [nr-cards (count hand)
        coords (hand-to-subimage-coords hand)
        subimages (get-subimages coords)
        to-draw (zipmap subimages (image-x-pos nr-cards))]
    (if (= player :player-hand)
      (doseq [[i p] to-draw]
        (.drawImage gfx i p 50 nil))
      (doseq [[i p] to-draw]
        (.drawImage gfx i p (+ 50 card-height 20) nil)))))

(defn game-panel "Render the game"
  [game]
  (proxy [javax.swing.JPanel java.awt.event.ActionListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (draw-hand g :player-hand (:player-hand @game))
      (draw-hand g :dealer-hand (:dealer-hand @game)))
    (actionPerformed [e] (swap! game deal-card-to-player :player-hand)
      (.repaint this))

))


(defn play-display "Render swing gui for blackjack"
  [game]
  (let [panel (game-panel (atom game))
        frame (javax.swing.JFrame.)
        hit-button (new javax.swing.JButton "HIT")
        stick-button (new javax.swing.JButton "STICK")]
    (doto hit-button
      (.addActionListener panel))
    (doto panel
      (.setFocusable true)
      (.setBackground (java.awt.Color/GRAY))
      (.add hit-button)
      (.add stick-button))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true)
      (.setDefaultCloseOperation javax.swing.JFrame/DISPOSE_ON_CLOSE))))

(defn -main [& args] ())

(-main)

