(ns stocksim.core
  (:use [stocksim.data]
        [incanter core charts stats]))

(def year 252)

(defn fixed-step
  [rate price]
  (+ price (* (/ price year) rate)))
;; (sim "Fixed interest step" (partial fixed-step 0.08))

(defn momentum
  "When short term average is above long term average, continue rising"
  [history]
  (let [short-average (mean (take-last 5 history))
        long-average (mean (take-last 50 history))]
    (if (< short-average long-average)
      0.999
      1.001)))

(defn chart [title prices]
  (let [dates (trading-dates (count prices))
        chart (time-series-plot dates prices :title title)]
    (view chart)))

(defn possible-prices
  "Shows a chart of a 5 year iteration of step"
  [years step]
  (let [days (* years year)]
    (last (take days (iterate step [1])))
    ))

(defn history-step
  "Take a step by several multiplicative factors which are calculated from history"
  [factors history]
  (let [get-factors (apply juxt factors)
        ratios (get-factors history)
        new-price (apply * (last history) ratios)]
    (conj history new-price)))

(defn simulate [years bias history]
  (let [price (map last history)
        changes (map / (rest price) price)
        lookup (into [] changes)
        sample (fn [history] (rand-nth lookup))
        bias-fn (fn [history] bias)
        momentum-sample-step #(history-step [momentum sample bias-fn] %)]
     (possible-prices years momentum-sample-step)))

(defn holdings [num-shares prices]
  (map #(* num-shares %) prices))



;; get histories for each stock
(def all-histories (map series cannabis-stocks))
;; (first (first all-histories))

;; simulate each stock, with one share of each
;; map + over each holding in time to find holdings over time
;; TODO pass in tickers + allocations
(defn simulate-portfolio [years bias]
  (->> all-histories
       (pmap (partial simulate years bias))
       (pmap (partial holdings 1 ))
       (apply (partial pmap +))))

;; Generate n market biases
(defn market-bias [n]
  (sample-normal n :mean 1 :sd 0.005)
  )

(let [sim (simulate-portfolio 1 (market-bias 1))]
  (/ (last sim) (first sim)))

;; run above n times
;; summarize  { min mean max } for all simulations
(defn simulate-portfolios [n years]
  (let [med (fn [& args] (median args))
        summarize (juxt max med min)
        biases (market-bias n)]
    (->> (pmap (partial simulate-portfolio years) biases)
         (apply (partial pmap summarize))
         (pmap #(zipmap [:max :median :min] %))
         )
    ))

;; TODO error will be correlated among all stocks
;; TODO the real optimization is "lowest minimum *given capital*"
(time (last (simulate-portfolios 5 1)))








;;; TODO
;;; search thru a mix of asset allocations that maximizes the minimum















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




;; TODO make a portfolio of each stock
;;      simulate that portfolio n times






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


(def cannabis-stocks ["GWPH" "TWMJF" "MJNA" "TRTC"
                      "CBIS" "CBDS" "SPRWF"
                      "CNBX" "MCIG" "CANN" "ZDPY"
                      "REFG" "GRNH" "AERO" "GBLX"
                      ])
                      ;;"GRWC" "EAPH" "MNTR" "PHOT"
                      ;;"FITX" "GLDFF" "ERBB" "SING"
                      ;;"TAUG" "HEMP" "DEWM" "PLPL"
                      ;;"EDXC" "VAPE" "VPOR" "HMPQ"
                      ;;"ICBU" "ENRT" "CBGI" "XTRM"
                      ;;"ENDO" "ATTBF" "LATF" "NGMC"
                      ;;"BAYP" "NTRR" "FSPM" "MINE"
                      ;;"DIRV" "PUGE" "EXMT" "RFMK"
                      ;;"REVI" "EFFI" "ANAS"])

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
