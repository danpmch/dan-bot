(ns dan-bot.core
  (:import (java.time ZonedDateTime
                      ZoneId)
           java.time.format.DateTimeFormatter
           java.time.temporal.ChronoUnit
           net.dv8tion.jda.api.JDABuilder
           net.dv8tion.jda.api.hooks.ListenerAdapter
           net.dv8tion.jda.api.interactions.commands.build.Commands
           net.dv8tion.jda.api.interactions.commands.OptionType
           net.dv8tion.jda.api.requests.GatewayIntent
           net.dv8tion.jda.api.entities.Activity
           net.dv8tion.jda.api.entities.Activity$ActivityType)
  (:require [next.jdbc :as jdbc]
            [dan-bot.nlp :as nlp])
  (:gen-class))

(def db {:dbtype "sqlite" :dbname "bot"})
(def ds (jdbc/get-datasource db))

(def config
  (-> (slurp "./config.edn")
      clojure.edn/read-string))

(def token
  (:token config))

(def friend-ids
  (:friends config))

(defn friend-set [id-list]
  (set (mapv #(% friend-ids) id-list)))

(defn roll-insultable? [friend-id]
  (let [friend-ids (:die-rolls-to-insult config)]
    ((friend-set friend-ids) friend-id)))

(defn mention-friend [friend-id]
  (str "<@!" friend-id ">"))

(defn roll-n [sides]
  (let [r (rand-int sides)]
    (if (= sides 100)
      r
      (inc r))))

(defn explode-n [roll]
  (fn [n]
    (let [result (roll n)]
      (if (= result n)
        (+ result (roll n))
        result))))

(defn roll-dice [dice-description]
  (let [[_ n-str sides-str mod-str] (re-matches #"([0-9]*)d([0-9]+)([+-][0-9]+)?" dice-description)
        total-dice (if (empty? n-str)
                     1
                     (Integer/parseInt n-str))
        sides (Integer/parseInt sides-str)
        mod (if mod-str
              (Integer/parseInt mod-str)
              0)
        values (mapv roll-n (repeat total-dice sides))]
    {:sides sides
     :dice dice-description
     :values values
     :total (apply + mod values)}))

(defn roll-dice-shadowrun [dice-description]
  (let [[_ n-str sides-str mod-str thresh-str] (re-matches #"([0-9]*)d([0-9]+)([+-][0-9]+)?(>[0-9]+)?" dice-description)
        total-dice (if (empty? n-str)
                     1
                     (Integer/parseInt n-str))
        sides (Integer/parseInt sides-str)
        mod (if mod-str
              (Integer/parseInt mod-str)
              0)
        thresh (if thresh-str
                 (->> thresh-str rest (apply str) (Integer/parseInt))
                 4)
        values (->> (mapv (explode-n roll-n) (repeat total-dice sides))
                    (mapv #(+ mod %)))]
    {:sides sides
     :dice dice-description
     :thresh thresh
     :values values
     :successes (->> values
                     (filter #(< thresh %))
                     count)}))

(defn render-rolls [author rolls]
  (letfn [(render [result]
            (clojure.string/join "\n" [(str (:dice result) ": " (:values result)
                                            (str "   total: " (:total result)))
                                       (if-let [friend (and (= (:sides result) 100)
                                                            (some #(<= 90 %) (:values result))
                                                            (roll-insultable? (.getId author)))]
                                         (str "Goddammit " (mention-friend friend) ".")
                                         nil)]))]
    (let [rolls-text (->> rolls
                          (map render)
                          (clojure.string/join "\n"))
          grand-total (apply + (map :total rolls))]
      (if (< (count rolls) 2)
        rolls-text
        (str rolls-text "\n"
             "grand total: " grand-total)))))

(defn slash-command-data [command-name description options]
  (let [data (Commands/slash command-name description)]
    (if-not (empty? options)
      (reduce (fn [d [opt-name values]]
                (let [required (:required values)
                      autocomplete (:autocomplete values)]
                  (.addOption d
                              (:type values)
                              (name opt-name)
                              (:description values)
                              (or required false)
                              (if autocomplete true false))))
              data
              options)
      data)))

(def jda (.. (JDABuilder/createLight token [GatewayIntent/GUILD_MESSAGES
                                            GatewayIntent/DIRECT_MESSAGES])
             (setActivity (Activity/listening "the sweet song of the orb"))
             build))

#_(.addEventListener jda (object-array
                        [(proxy [ListenerAdapter] []
                           (onSlashCommandInteraction [event]
                             (println
                              "got slash command event:"
                              (.getName event))))]))

(def alert-channel (.. jda
                       (retrieveUserById (:me friend-ids))
                       complete
                       openPrivateChannel
                       complete))

(defn alert [& msg]
  (.. alert-channel
      (sendMessage (clojure.string/join " " msg))
      queue))

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

(defn complete [text completions]
  (->> completions
       (filter #(.contains % text))
       (sort-by #(clojure.string/index-of % text))))

(defn completion-listener [command-name option-name query]
  (proxy [ListenerAdapter] []
    (onAutoCompleteInteraction [event]
      (println "Running completion for " (.getName event) (.. event getFocusedOption getName))
      (when (and (= command-name (.getName event))
                 (= option-name (.. event
                                    getFocusedOption
                                    getName)))
        (let [search-text (.. event
                              getFocusedOption
                              getValue)
              completions (query)]
          (complete search-text completions))))))

(defmacro defslash [name description options args & body]
  (let [[event] args
        handler-name (symbol (str (str name) "-handler"))
        name-key (gensym name)
        data-sym (gensym "data")]
    `(let [options# ~options]
       (defn ~handler-name ~args ~@body)
       (defonce ~name
         (let [~name-key (slash-command-listener
                          ~args
                          (when (= ~(str name) (.getName ~event))
                            (log-event ~event)
                            (~handler-name ~event)))
               completions# (->> options#
                                 (filter (fn [[unused# settings#]]
                                           (:autocomplete settings#)))
                                 (map (fn [[n# settings#]]
                                        (completion-listener ~(str name) n# (:autocomplete settings#)))))]
           (.addEventListener jda (object-array (cons ~name-key
                                                      completions#)))
           ~name-key))
       (when-not (global-command-defined? ~(str name))
         (let [~data-sym (slash-command-data ~(str name) ~description options#)]
           (.. jda
               (upsertCommand ~data-sym)
               queue))))))

(defn get-option-as [event option-key type-enum]
  (let [option (name option-key)
        type-map {OptionType/STRING #(.getAsString %)
                  OptionType/INTEGER #(.getAsLong %)}
        type (or (get type-map type-enum)
                 (throw (Exception. "Unknown enum value")))]
    (when-let [v (->> (.getOptions event)
                      (filter #(= option (.getName %)))
                      first)]
      (type v))))

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
  (let [dice-str (get-option-as event :dice OptionType/STRING)
        results (mapv roll-dice
                      (clojure.string/split dice-str #" "))]
    (.. event
        (reply (render-rolls (.getUser event) results))
        (setEphemeral false)
        queue)))

(defslash poll-by-reactions
  "Create a simple poll using reactions"
  {:title {:type OptionType/STRING
           :description "Describe what you're polling"
           :required true}
   :choices {:type OptionType/STRING
             :description "Semicolon separated list of choices"
             :required true}}
  [event]
  (let [title (get-option-as event :title OptionType/STRING)
        choices-str (get-option-as event :choices OptionType/STRING)
        emojis ["1️⃣" "2️⃣" "3️⃣" "4️⃣" "5⃣" "6️⃣" "7️⃣" "8️⃣" "9️⃣" "🔟"]
        choices (clojure.string/split choices-str #";")
        display-choices (mapv (fn [emoji choice]
                                (str emoji ": " choice))
                              emojis
                              choices)
        text (clojure.string/join "\n" (flatten [(str "**" title "**")
                                                 display-choices
                                                 "Vote by reacting to this message:"]))
        message (.. event
                    (reply text)
                    #_(setEphemeral true)
                    complete
                    retrieveOriginal
                    complete)]
    (->> (mapv #(.addReaction message %)
               (take (count choices) emojis))
         (mapv #(.queue %)))))

(defn get-guild-ids [guilds]
  (let [all-guilds (:guilds config)]
    (set (mapv #(% all-guilds) guilds))))

(defn in-guilds? [guilds event]
  (let [guild-ids (get-guild-ids guilds)
        guild-id (.. event
                     getGuild
                     getId)]
    (contains? guild-ids guild-id)))

(defslash list-incident-types
  "List the currently defined incident types"
  {}
  [event]
  (if (in-guilds? (:incident-guilds config) event)
    (let [incident-types (jdbc/execute! ds ["select name, description from incident_types order by name"])
          display-rows (mapv (fn [r]
                               (let [n (:incident_types/name r)
                                     d (:incident_types/description r)]
                                 (format "**%s**: %s" n d)))
                             incident-types)]
      (.. event
          (reply (clojure.string/join "\n" display-rows))
          (setEphemeral true)
          queue))
    (.. event
        (reply "This slash command is not supported for your guild.")
        (setEphemeral true)
        queue)))

(defslash define-incident-type
  "Define a new type of incident"
  {:type {:type OptionType/STRING
          :description "The name that identifies this type of incident"
          :required true}
   :description {:type OptionType/STRING
                 :description "An explanation of what this incident type represents"
                 :required true}
   :template {:type OptionType/STRING
              :description "A printf style template for printing the most recent incident"
              :required true}}
  [event]
  (if (in-guilds? (:incident-guilds config) event)
    (let [n (get-option-as event :type OptionType/STRING)
          d (get-option-as event :description OptionType/STRING)
          t (get-option-as event :template OptionType/STRING)]
      (jdbc/execute! ds [(str "insert into incident_types (name, description, status_template) values ("
                              (->> (mapv #(str "'" % "'") [n d t])
                                   (clojure.string/join ", "))
                              ")")])
      (.. event
          (reply "New incident type stored.")
          (setEphemeral true)
          queue))
    (.. event
        (reply "This slash command is not supported for your guild.")
        (setEphemeral true)
        queue)))

(defslash update-incident-type
  "Update an existing type of incident"
  {:type {:type OptionType/STRING
          :description "The name that identifies this type of incident"
          :required true}
   :description {:type OptionType/STRING
                 :description "An explanation of what this incident type represents"}
   :template {:type OptionType/STRING
              :description "A printf style template for printing the most recent incident"}}
  [event]
  (if (in-guilds? (:incident-guilds config) event)
    (let [n (get-option-as event :type OptionType/STRING)
          d (get-option-as event :description OptionType/STRING)
          t (get-option-as event :template OptionType/STRING)
          updates (clojure.string/join ", " (->> [["description" d]
                                                  ["status_template" t]]
                                                 (filter (comp not nil? second))
                                                 (map (fn [[col v]]
                                                        (format "%s = '%s'" col v)))))]
      (jdbc/execute! ds [(format "update incident_types set %s where name = '%s'" updates n)])
      (.. event
          (reply (format "Incident type %s updated." n))
          (setEphemeral true)
          queue))
    (.. event
        (reply "This slash command is not supported for your guild.")
        (setEphemeral true)
        queue)))

(defn to-java-datetime [sql-datetime]
  (ZonedDateTime/parse sql-datetime
                       (.. (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")
                           (withZone (ZoneId/of "UTC")))))

(defn latest-incident [incident-name]
  (str "
select status_template, t
 from incident_types join incidents on incident_types.name = incidents.name
 where incident_types.name = '" incident-name "'
 order by t desc
 limit 1
"))

(defn get-time-since-last-incident-report [incident-type]
  (when-let [lookup-result (->> (jdbc/execute! ds [(latest-incident incident-type)])
                                first)]
    (let [last-incident (->> lookup-result
                             :incidents/t
                             to-java-datetime)
          template (:incident_types/status_template lookup-result)
          elapsed-days (.between ChronoUnit/DAYS last-incident (ZonedDateTime/now))]
      (format template elapsed-days))))

(defslash record-incident
  "Record the date of the current incident"
  {:type {:type OptionType/STRING
          :description "The type of incident (use the same type name each time)"
          :required true}}
  [event]
  (if (in-guilds? (:incident-guilds config) event)
    (let [n (get-option-as event :type OptionType/STRING)
          time-since-last (get-time-since-last-incident-report n)]
      (jdbc/execute! ds [(str "insert into incidents (name) values ('" n "')")])
      (.. event
          (reply (clojure.string/join "\n"
                                      [(or time-since-last "")
                                       (format "New %s incident recorded." n)]))
          queue))
    (.. event
        (reply "This slash command is not supported for your guild.")
        (setEphemeral true)
        queue)))

(defslash test-record-incident
  "Test autocompletion"
  {:type {:type OptionType/STRING
          :description "The type of incident (use the same type name each time)"
          :required true
          :autocomplete (fn []
                          (println "Fetching completion options")
                          (->> (jdbc/execute! ds [(format "select distinct name from incident_types")])
                               (map :incident_types/name)))}}
  [event]
  (if (in-guilds? (:incident-guilds config) event)
    (let [n (get-option-as event :type OptionType/STRING)
          time-since-last nil]
      (.. event
          (reply (clojure.string/join "\n"
                                      [(or time-since-last "")
                                       (format "New %s incident recorded." n)]))
          (setEphemeral true)
          queue))
    (.. event
        (reply "This slash command is not supported for your guild.")
        (setEphemeral true)
        queue)))

(defslash incident-history
  "List the last n incidents for a given type"
  {:type {:type OptionType/STRING
          :description "The type of incident (use the same type name each time)"
          :required true}
   :n {:type OptionType/INTEGER
       :description "The maximum number of events to list"}}
  [event]
  (if (in-guilds? (:incident-guilds config) event)
    (let [type (get-option-as event :type OptionType/STRING)
          n (or (get-option-as event :n OptionType/INTEGER)
                10)
          history (->> (jdbc/execute! ds [(format "select t from incidents where name = '%s' order by t limit %d" type n)])
                       (map :incidents/t))
          days (->> history
                    (map to-java-datetime)
                    (map #(.truncatedTo % ChronoUnit/DAYS))
                    distinct)
          diffs (map #(.between ChronoUnit/DAYS %1 %2)
                     days
                     (rest days))
          average-days (if (empty? diffs)
                         ##Inf
                         (float (/ (apply + diffs)
                                   (count diffs))))]
      (.. event
          (reply (clojure.string/join "\n" (concat [(format "**%s** history:" type)
                                                    ""]
                                                   history
                                                   [""
                                                    (format "Average days between incidents: %.1f" average-days)])))
          queue))
    (.. event
        (reply "This slash command is not supported for your guild.")
        (setEphemeral true)
        queue)))

(defslash time-since-last-incident
  "Report the time elapsed since the last incident"
  {:type {:type OptionType/STRING
          :description "The type of incident (use the same type name each time)"
          :required false}}
  [event]
  (if (in-guilds? (:incident-guilds config) event)
    (let [incident-type (or (get-option-as event :type OptionType/STRING)
                            (:default-incident config))]
      (.. event
          (reply (get-time-since-last-incident-report incident-type))
          queue))
    (.. event
        (reply "This slash command is not supported for your guild.")
        (setEphemeral true)
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

(defmessage react-ukraine-flag [_]
  #_(let [message (.getMessage event)
          text (.getContentDisplay message)]
      (when (re-find #"(?i)ukraine" text)
        (.. message
            (addReaction "🇺🇦")
            queue))))

; <a:ultrafastparrot:658317840868442113>
(defmessage calm-down [event]
  (let [message (.getMessage event)
        text (.getContentRaw message)
        user-id (.. (.getAuthor event)
                    getId)
        friend-to-calm (:friend-to-calm-down config)
        friend-to-calm-id (get friend-ids friend-to-calm)]
    (when (and (re-find #"<a:ultrafastparrot:658317840868442113>" text)
               #_(= user-id friend-to-calm-id))
      (alert "Detected uncalmness:" text))))

(defmessage sentiment-alert [event]
  (let [message (.getMessage event)
        text (.getContentRaw message)
        sentiments (nlp/sentiment text)]
    (nlp/append-sentiment "./sentiments.log" sentiments)))

(defmacro def-random-response-listener [name regex responses]
  (let [event (gensym "event")
        message (gensym "message")
        text (gensym "text")]
    `(defmessage ~name [~event]
       (let [~message (.getMessage ~event)
             ~text (.getContentDisplay ~message)]
         (when (re-find ~regex ~text)
           (.. ~message
               getChannel
               (sendMessage (rand-nth ~responses))
               queue))))))

(def-random-response-listener friends-night
  #"(?i)who.*friend.*night.*\?"
  (concat ["I'm always ready for friend's night, it's the best!!!"
           "Only losers skip friend's night!"
           "I've been waiting for friend's night all week!"
           "Friends don't let friends skip friend's night!"
           "Be there or be square!"]
          (mapv #(str (mention-friend %) " is a butt-face!")
                (friend-set (:friends-to-insult config)))))

(def-random-response-listener praise-the-orb
  #"(?i) orb([ !.,?;]|$)"
  ["Praise the orb!"
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
   "Those who would defile the orb shall be mist!"])

(comment
  (.. jda
      (addEventListener (object-array [|test])))

  (def global-commands (.. jda
                           retrieveCommands
                           complete))

  (->> global-commands
       (mapv #(.getName %)))

  (count (.. jda
             getRegisteredListeners))

  (->> global-commands
       (filter #(= "time-since-last-incident" (.getName %)))
       first
       .getId
       (.deleteCommandById jda)
       .queue)

  (.. jda
      (upsertCommand |roll-data)
      queue)

  (jdbc/execute! ds ["
create table incident_types (
  name varchar primary key,
  description varchar,
  status_template varchar
)
"])
  (jdbc/execute! ds ["
create table incidents (
  name varchar,
  t datetime default current_timestamp,
  primary key (name, t),
  foreign key (name) references incident_types (name)
    on delete cascade
)"])

  (.. jda
      getPresence
      (setActivity (Activity/listening "the sweet song of the orb"))))

