(ns dan-bot.nlp
  (:import java.util.Properties
           (edu.stanford.nlp.pipeline StanfordCoreNLP
                                      CoreDocument)
           (java.nio.file Files
                          Paths
                          StandardOpenOption
                          OpenOption))
  (:gen-class))

(def pipeline
  (let [props (when-let [ps (Properties.)]
                (.setProperty ps
                              "annotators"
                              (clojure.string/join "," ["tokenize"
                                                        "ssplit"
                                                        "pos"
                                                        "parse"
                                                        "sentiment"]))
                ps)]
    (StanfordCoreNLP. props)))

(defn sentiment [text]
  (let [document (CoreDocument. text)]
    (.annotate pipeline document)
    (->> (.sentences document)
         (map (fn [s]
                [(.sentiment s)
                 (.text s)])))))

(defn append-sentiment [file sentiments]
  (when (not (empty? sentiments))
    (let [text (->> sentiments
                    (map (fn [[sentiment sentence]]
                           (str sentiment " " sentence)))
                    (clojure.string/join "\n"))
          full-text (str text "\n")]
      (Files/write (Paths/get file (make-array String 0))
                   (.getBytes full-text)
                   (into-array OpenOption [StandardOpenOption/APPEND])))))

(comment
  (str "test" "ing")

  (->> [["POS" "testing"]]
       (map (fn [[sentiment sentence]]
              (str sentiment " " sentence)))
       (clojure.string/join "\n")
       (.getBytes))

  (append-sentiment "./sentiments.log" [["POS" "testing"]])
  (make-array String 0)
  (append-sentiment "blah" [["NEG" "testing"]
                            ["POS" "more text"]])
  (sentiment "this is a test")
  (sentiment "another test")
  (sentiment "I hate all this testing"))
