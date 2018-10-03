(ns example.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [kitchen-async.promise :as p]
            [cljs.core.async :refer [chan <! >! close!]]
            [cljs-http.client :as hc]
            [cljs-time.core :as cc]
            [cljs-time.format :as cf]
            [cljs-time.predicates :as cp]))

;; to avoid "Octal literals are not allowed in strict mode."
;; https://github.com/anmonteiro/lumo/issues/28
(.setFlagsFromString (js/require "v8") "--no-use_strict")

;; With node, xhr2 have to be used as xhr doesn't exist
(set! js/XMLHttpRequest (js/require "xhr2"))

(def puppeteer (js/require "puppeteer"))

(defn get-json [url]
  (let [out-ch (chan)]
    (go (let [response (<! (hc/get url))]
          (>! out-ch (:body response))))
    out-ch))

(defn cb
  [items]
  (.map items (fn [itm]
                #js {:url (.. itm -firstElementChild -href)
                     :text (.-innerText itm)})))

(defn get-page-items
  [url selector]
  (let [out-ch (chan)]
    (p/let [browser (.launch puppeteer)
            page (.newPage browser)
            _ (.goto page url #js {:waitUntil "domcontentloaded"})
            elements (.$$eval page selector cb)]
      (go
        (>! out-ch (js->clj elements)))
      (.close browser))
    out-ch))

(defn get-holidays [year month]
  (let [date (cf/parse (cf/formatters :date) (goog.string.format "%s-%s-1" year month))
        next-day (cc/plus date (cc/months 1))
        base-url "https://www.googleapis.com/calendar/v3/calendars/"
        calendar-id "en.japanese%23holiday@group.v.calendar.google.com"
        gcal-api-key process.env.GCAL_API_KEY
        start-date (goog.string.format "%s-%s-01T00:00:00Z" (cc/year date) (cc/month date))
        end-date (goog.string.format "%s-%s-01T00:00:00Z" (cc/year next-day) (cc/month next-day))
        params (goog.string.format
                "/events?key=%s&timeMin=%s&timeMax=%s&maxResults=50&orderBy=startTime&singleEvents=true"
                gcal-api-key start-date end-date)
        url (str base-url calendar-id params)
        in-ch (get-json url)
        out-ch (chan)]
    (go
      (>! out-ch
          (map (fn [itm] (get-in itm [:start :date])) (:items (<! in-ch))))
      (close! in-ch))
    out-ch))

(defn holiday? [& [date]] ; "yyyy-mm-dd"
  (let [_date (or (and date (cf/parse (cf/formatters :date) date))
                  (cc/now))
        in-ch (get-holidays (cc/year _date) (cc/month _date))
        out-ch (chan)]
    (go
      (>! out-ch
          (or
           (cp/weekend? _date)
           (some #{(cf/unparse (cf/formatters :date) _date)}
                 (<! in-ch))
           false))
      (close! in-ch))
    out-ch))

(defn get-target-info
  [url]
  (let [selector (str "div.ymuiContainerNopad.ymuiDotLine.clearFix "
                      "div.marB15.clearFix "
                      "ul "
                      "li.yjMt.ymuiArrow1L")
        in-ch (get-page-items url selector)
        out-ch (chan)]
    (go
      (>! out-ch (let [items (<! in-ch)]
                   (keep #(when (re-matches #".*【.】.+" (get % "text"))
                            %)
                         items)))
      (close! in-ch))
    out-ch))

(defn get-target-link [& [opts]]
  (let [{:keys [date]} opts
        _date (if date
                date
                (cf/unparse (cf/formatters :basic-date) (cc/now)))
        url "https://news.finance.yahoo.co.jp/cp/stkms"
        out-ch (chan)]
    (go
      (loop [page 1]
        (let [in-ch (get-target-info (str url "?p=" page "&date=" _date))
              item (clj->js (<! in-ch))]
          (close! in-ch)
          (cond
            (not (empty? item)) (>! out-ch (first item))
            (< 5 page) "not found" ;; TODO
            :else (recur (inc page))))))
    out-ch))


(defn main [& cli-args]
  #_(go 
    (console.log
     (->
      (<! (get-target-info "https://news.finance.yahoo.co.jp/cp/stkms"
                           "div.ymuiContainerNopad.ymuiDotLine.clearFix div.marB15.clearFix ul li.yjMt.ymuiArrow1L"))
      first
      (get "url")
      clj->js
      )))
  #_(go (console.log
       (clj->js
        (<! (get-target-link "https://news.finance.yahoo.co.jp/cp/stkms?p=2&date=20180928")))))
  
  (go
    (let [date (first cli-args) ;; yyyy-mm-dd
          holiday?-ch (holiday? date)] 
      (when (not (<! holiday?-ch))
        (let [in-chan (get-target-link {:date (and
                                               date
                                               (clojure.string/replace date #"-" ""))})
              info (<! in-chan)
              url (get (js->clj info) "url")]
          (close! in-chan)
          (p/let [browser (.launch puppeteer)
                  page (.newPage browser)
                  selector "div#richToolTipArea div.ymuiContainerNopad.clearFix.s170"
                  item (.$ page selector)]
            (.goto page url #js {:waitUntil "domcontentloaded"})
            (do
              (p/then (p/resolve (.$eval page selector (fn [itm] (.-innerText itm)))) console.log))
            (.close browser))))
      (close! holiday?-ch))))
