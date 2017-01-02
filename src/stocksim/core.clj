(ns stocksim.core
  (:use [stocksim.data]
        ;; [cortex.optimise]
        ;; [clojure.core.matrix]
        [incanter core charts stats]))

(def year 252)

(def cannabis-stocks ["GWPH" "TWMJF" "MJNA" "TRTC"
                      "CBIS" "CBDS" "SPRWF" "CNBX"
                      "MCIG" "CANN" "ZDPY" "REFG"
                      "GRNH" "AERO" "GBLX" "GRWC"
                      "EAPH" "MNTR" "FITX" "ERBB"
                      "SING" "TAUG" "HEMP" "DEWM"
                      "PLPL" "EDXC" "VAPE" "VPOR"
                      "HMPQ" "ICBU" "ENRT" "CBGI"
                      "XTRM" "ATTBF" "LATF" "NGMC"
                      "BAYP" "NTRR" "FSPM" "MINE"
                      "DIRV" "PUGE" "EXMT" "RFMK"
                      "EFFI" "ANAS"
                      ;;"REVI";; "PHOT";; "GLDFF" ;;"ENDO"
                      ])

;; get histories for each stock
(def all-histories (pmap series cannabis-stocks))
(last (first all-histories))


(defn holdings
  [num-shares prices]
  (map #(* num-shares %) prices))

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


(defn chart
  [title prices]
  (let [dates (trading-dates (count prices))
        chart (time-series-plot dates prices :title title)]
    (view chart)))

(defn possible-prices
  "Shows a chart of a 5 year iteration of step"
  [days step]
  (last (take days (iterate step [1]))))

(defn history-step
  "Take a step by several multiplicative factors which are calculated from history"
  [factors history]
  (let [get-factors (apply juxt factors)
        ratios (get-factors history)
        new-price (apply * (last history) ratios)]
    (conj history new-price)))

(defn simulate [days history]
  (let [price (map last history)
        changes (map / (rest price) price)
        lookup (into [] changes)
        sample (fn [history] (rand-nth lookup))
        sample-step #(history-step [momentum sample] %)]
     (possible-prices days sample-step)))

;; simulate stock prices for each stock
;; returns an end-of-year yield
(defn simulate-portfolio [days _]
  (let [starting-prices (map #(last (last %)) all-histories)
        ending-prices (->> all-histories
                           (pmap (partial simulate days))
                           (map last)
                           )
        compare #(let [change (/ %1 %2)]
                   (if (> %1 %2) change (* -1 change)))]
    (map compare ending-prices starting-prices)
    ))


(def simulations
  (let [results
        (pmap #(simulate-portfolio year %) (range 1000))
        summarize
        (juxt #(apply min %) median #(apply max %))
        ]
    (->> (map summarize results)
         (zipmap cannabis-stocks))
    )
  )


(->> simulations
     ;; filter for median profitable
     (filter (fn [[k v]] (< 0 (second v)))) 
     ;; sort for lowest inimum
     (sort (fn [[_ v1] [_ v2]] (> 0 (first v1) (first v2))))
     (keys)
     )


