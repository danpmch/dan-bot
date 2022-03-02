(ns dan-bot.core
  (:import net.dv8tion.jda.api.JDABuilder
           net.dv8tion.jda.api.hooks.ListenerAdapter
           net.dv8tion.jda.api.interactions.commands.build.Commands
           net.dv8tion.jda.api.interactions.commands.OptionType
           net.dv8tion.jda.api.requests.GatewayIntent)
  (:gen-class))

(def token
  (-> (slurp "./config.edn")
      clojure.edn/read-string
      :token))

;(map #(.getName %) (.getMethods ListenerAdapter))
;(map #(.getName %) (.getMethods SlashCommandEvent))

(defn roll-n [sides]
  (let [r (rand-int sides)]
    (if (= sides 100)
      r
      (inc r))))

(defn roll-dice [dice-description]
  (let [[_ n-str sides-str] (re-matches #"([0-9]*)d([0-9]+)" dice-description)
        total-dice (if (empty? n-str)
                     1
                     (Integer/parseInt n-str))
        sides (Integer/parseInt sides-str)]
    {:sides sides
     :values (mapv roll-n (repeat total-dice sides))}))

(defn render-rolls [rolls]
  (letfn [(render [result]
            (str "d" (:sides result) ": " (:values result)
                 (if (< (count (:values result)) 2)
                   ""
                   (str "   total: " (apply + (:values result))))))]
    (let [rolls-text (->> rolls
                          (map render)
                          (clojure.string/join "\n"))
          grand-total (apply + (mapcat :values rolls))]
      (if (< (count rolls) 2)
        rolls-text
        (str rolls-text "\n"
             "grand total: " grand-total)))))

(defn slash-command-data [command-name description options]
  (let [data (Commands/slash command-name description)]
    (when (not (empty? options))
      (reduce (fn [d [opt-name values]]
                (if-let [required (:required values)]
                  (.addOption d (:type values) (name opt-name) (:description values) required)
                  (.addOption d (:type values) (name opt-name) (:description values))))
              data
              options))))

(def jda (.. (JDABuilder/createLight token [GatewayIntent/GUILD_MESSAGES
                                            GatewayIntent/DIRECT_MESSAGES])
             build))

(defn global-command-defined? [name]
  (let [commands (.. jda
                     retrieveCommands
                     complete)
        command-names (set (mapv #(.getName %) commands))]
    (command-names name)))

(defn log-event [event]
  (println "got an event:"
           (.getCommandString event)))

(defn listener-adapter [fn-name args & body]
  `(proxy [ListenerAdapter] []
     (~fn-name ~args ~@body)))

(defmacro slash-command-listener [args & body]
  `(proxy [ListenerAdapter] []
     (onSlashCommandInteraction ~args
       ~@body)))

(defmacro defslash [name description options args & body]
  (let [[event] args
        handler-name (symbol (str (str name) "-handler"))
        name-key (gensym name)
        data-sym (gensym "data")]
    `(do (defn ~handler-name ~args ~@body)
         (defonce ~name
           (let [~name-key (slash-command-listener
                            ~args
                            (when (= ~(str name) (.getName ~event))
                              (log-event ~event)
                              (~handler-name ~event)))]
             (.addEventListener jda (object-array [~name-key]))
             ~name-key))
         (when-not (global-command-defined? ~(str name))
           (let [~data-sym (slash-command-data ~(str name) ~description ~options)]
             (.. jda
                 (upsertCommand ~data-sym)
                 queue))))))

(defslash test
  "test slash command for dan-bot"
  {}
  [event]
  (.. event
      (reply "I'm aliiiiive!")
      (setEphemeral true)
      queue))

(defslash roll
  "roll some dice"
  {:dice {:type OptionType/STRING
          :description "The dice to roll, e.g. d6, 2d10"
          :required true}}
  [event]
  (let [dice-str (.. event
                     getOptions
                     (get 0)
                     getAsString)
        results (mapv roll-dice
                      (clojure.string/split dice-str #" "))]
    (.. event
        (reply (render-rolls results))
        (setEphemeral false)
        queue)))

(defmacro message-listener [args & body]
  (apply listener-adapter 'onMessageReceived args body))

(defmacro defmessage [name args & body]
  (let [[event] args
        handler-name (symbol (str (str name) "-handler"))
        name-key (gensym name)]
    `(do (defn ~handler-name ~args ~@body)
         (defonce ~name
           (let [~name-key (message-listener ~args
                                             (when-not (= (.getSelfUser jda) (.getAuthor ~event))
                                               (~handler-name ~event)))]
             (.addEventListener jda (object-array [~name-key]))
             ~name-key)))))

(defmessage friends-night [event]
  (let [message (.. event
                    getMessage)
        text (.getContentDisplay message)]
    (when (re-find #"(?i)who.*friend.*night\?" text)
      (.. message
          getChannel
          (sendMessage "I'm always ready for friend's night, it's the best!!!")
          queue))))

(defmessage praise-the-orb [event]
  (let [message (.getMessage event)
        text (.getContentDisplay message)
        praises ["Praise the orb!"
                 "All hail the orb!"
                 "All shall kneel before the orb!"
                 "Sing praises to the orb! I\u00e4! Shub-Niggurath!"
                 "The shining orb circumscribes our existence!"
                 "There is no truth but the orb!"
                 "The orb's beauty passes understanding!"
                 "Those who reject the orb must perish!"
                 "The faithful will find rest within the the orb's spherical embrace!"
                 "The orb laughs at those who would oppose it!"
                 "The orb shines down upon the faithful!"
                 "The faithful bask in the light of the orb!"
                 "Those who would defile the orb shall be mist!"]]
    (when (re-find #"(?i) orb([ !.,?;]|$)" text)
      (.. message
          getChannel
          (sendMessage (rand-nth praises))
          queue))))

(comment
  (.. jda
      (addEventListener (object-array [|test])))

  (def global-commands (.. jda
                           retrieveCommands
                           complete))

  (.. jda
      getRegisteredListeners)

  (->> global-commands
       (filter #(= "test2" (.getName %)))
       first
       .getId
       (.deleteCommandById jda)
       .queue)

  (.. jda
      (upsertCommand |roll-data)
      queue))

