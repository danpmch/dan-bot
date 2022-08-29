(ns dan-bot.nlp
  (:import java.util.Properties
           (edu.stanford.nlp.pipeline StanfordCoreNLP
                                      CoreDocument))
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

(comment
  (sentiment "this is a test")
  (sentiment "another test")
  (sentiment "I hate all this testing"))
