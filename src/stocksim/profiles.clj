




; plot the daily changes
(defn market-profile [ticker]
  (let [price-series (series ticker)
        ;; date (map first price-series)
        price (map last price-series)
        change (map - (rest price) price)
        get-stats (juxt mean
                        #(reduce max %)
                        #(reduce min %))
        stats (get-stats change)]
    ;; (view (time-series-plot date price :title ticker))
    ;; (view (time-series-plot date change :title (str "Daily change ratio " ticker)))
    ;; (view (histogram change :nbins 50 :title (str ticker" change distribution")))
    (zipmap [:mean :max :min] stats)
    ))
                                        ; Average change is 0.337% per day, max rise is 11.5% and fall is -20.5%
; S&P 500
(market-profile "^GSPC")

(defn assemble-profiles [tickers]
  (let [profiles (map market-profile tickers)]
    (map
     #(assoc %1 :ticker %2)
     profiles
     tickers)))

(def sps (assemble-profiles cannabis-stocks))
;; lowest lows of the group?
(->> sps
     (sort-by :min)
     (reverse)
     (take 5))

(defn create-sample-step
  "Chooses a random step from real data"
  [sym]
  (let [price (map last (series sym))
        changes (map / (rest price) price)
        ; use a vector so we can index any value instead of scanning
        lookup (into [] changes)]
    #(* % (rand-nth lookup))))
;; (def sp-sample-step (create-sample-step "^GSPC"))



;; TODO what we really need to do is generate a list of day-by-day stock prices
(defn possible-prices [ticker]
  (let [step (create-sample-step ticker)]
    (take (* 30 year) (iterate step 1))
    ))
(defn summarize-simulations)
(repeatedly 100 #(possible-prices (first cannabis-stocks)))
;;      for each of these stocks
;; then, a method can start with a certain capial in each





;; TODO take a list of vecs
;;      produce { :min :median :max }
;;      for each item in each vec in vecs
(defn summarize [vecs]
  (let [get-stats (juxt mean
                        #(reduce max %)
                        #(reduce min %))]
    (get-stats (last vecs))
    ))

(let [days (* 30 year)
      ticker (nth cannabis-stocks 1)
      hist (series ticker)
      simulations (simulate 10 days hist)]
  (summarize simulations))
;; (chart ticker
;; (holdings 10 simulated-prices)))




(nth (iterate #(fixed-step 0.085 %) 1)
     (* 30 year))



; Does this match an underpinning ecconomy? What is the real distribution?
(let [price (map last sp)
      period (partition (* 30 year) 1 price)
      profit-for #(- (last %) (first %))
      return-for #(/ (profit-for %) (first %))
      return (map return-for period)]
  (view (histogram return :nbins 50 :title "All 30 year period returns distribution"))
  (mean return))

(defn buy-and-hold
  "Gross return after 30 years of holding"
  [step]
  (last (take (* 30 year) (iterate step 1))))

; What are some of the relevant fixed income rates to achieve these returns?
(buy-and-hold #(fixed-step 0.069 %))
(buy-and-hold #(fixed-step 0.054 %))
(buy-and-hold #(fixed-step 0.037 %))
