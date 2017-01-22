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



(defn compound3 [amount yearly-contribs yields]
  (let [history (if (vector? amount) amount [ amount ])
        this-yield (first yields)
        this-years-returns (* (last history) this-yield)
        next-years-balance (+ this-years-returns
                              (first yearly-contribs)
                              (last history))
        ]
    (if (= 1 (count yields)) amount
        (recur (conj history next-years-balance)
               (rest yearly-contribs)
               (rest yields)
               )
        )
    ))


(defn contribs [n caps increase]
  (if (zero? n)
    caps
    (recur
     (dec n)
     (conj caps
           (+ (last caps)
              (* increase (last caps))
              ))
     increase

      )
  )
  )

(contribs 10 [31200] 0.05)

(compound3 80000
           (contribs 10 [31200] 0.05)
           (repeat 10 0.05)
           )

(defn random-normal
  "Simulate a normal distribution by adding random numbers together"
  []
  (reduce + -50 (repeatedly 100 rand)))

(random-normal)

(defn normal-step
  "Add a (biased) random normal number relative to current price"
  [price]
  (let [biased (+ price (random-normal))
        scale 0.004]
    (+ price (* biased scale))))



(defn generate-yields [n start]
  (take n (iterate normal-step start))
  )

(let [simulations
      (repeatedly 1000
                  #(last
                    (compound3 80000
                               (contribs 10 [31200] 0.07)
                               (generate-yields 10 0.05))))
      ]

  (view (histogram simulations
                   :title "Simulated outcomes after ten years"))

   (median simulations)

  )
