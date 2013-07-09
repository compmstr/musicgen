(ns musicgen.midi
  (import [javax.sound.midi MidiEvent MidiMessage MidiSystem Sequence ShortMessage Track]
          [java.io File]))

(def note-names
  [:C :C# :D :D# :E :F :F# :G :G# :A :A# :B])

(let [num->cmd {0x90 :note-on
                0x80 :note-off}
      cmd->num (reduce merge (map #(apply hash-map %)
                                  (map reverse num->cmd)))]
  (defn midi-cmd
    [cmd]
    (if (keyword? cmd)
      (cmd->num cmd)
      (num->cmd cmd))))

(defn midi-filename
  [name]
  (if-let [url (clojure.java.io/resource name)]
    (.getFile url)
    name))

(defn midi-file-sequence
  [filename]
  (MidiSystem/getSequence (File. (midi-filename filename))))

(defn note-desc->note-info
  [desc]
  (let [[note octave] (rest (re-matches #"([a-gA-G]#?)([0-9]+)" desc))
        octave (try (Integer/valueOf octave)
                    (catch NumberFormatException e
                      (println "Invalid note description")
                      nil))]
    (if (or (nil? note) (nil? octave))
      nil
      (let [note (.toUpperCase note)]
        {:note-name (keyword note)
         :octave octave}))))

(defn note-info->key
  [{:keys [note octave note-name] :as note-info}]
  (let [note (or note
                 (loop [i 0]
                   (if (< i (count note-names))
                     (if (= (.toUpperCase
                             (if (keyword? note-name)
                               (name note-name)
                               (str note-name)))
                            (name (nth note-names i)))
                       i
                       (recur (inc i)))
                     nil)))]
    (if (or (nil? note)
            (nil? octave))
      nil
      (+ (* (inc octave) 12) note))))

(defn key->note-info
  [key]
  (let [note (mod key 12)
        octave (dec (quot key 12))
        note-name (note-names note)]
    {:note note :octave octave :note-name note-name}))

(defn note-desc->key
  [desc]
  (note-info->key (note-desc->note-info desc)))

(defn- event-process-key
  "Add readable note/octave/note-name to note event"
  [event]
  (merge event (key->note-info (:key event))))

(defn parse-track-event
  [event]
  (let [tick (.getTick event)
        msg (.getMessage event)]
    (if (instance? ShortMessage msg)
      (case (midi-cmd (.getCommand msg))
        (:note-on :note-off)
        (let [msg-key (.getData1 msg)]
          (event-process-key
            {:cmd (midi-cmd (.getCommand msg))
             :channel (.getChannel msg)
             :key msg-key
             :tick tick
             :vel (.getData2 msg)}))
        {:cmd (.getCommand msg)})
      :non-sm)))
(defn parse-track
  [idx track]
  (println "Parsing Track" idx)
  {:index idx
   :ticks (.ticks track)
   :events (doall 
            (for [i (range (.size track))]
             (parse-track-event (.get track i))))})

(defn parse-midi
  [filename]
  (let [midi-seq (midi-file-sequence filename)]
     {:tracks
      (doall
       (map-indexed parse-track (seq (.getTracks midi-seq))))}))

(defn track-note-events
  [track]
  (filter #(let [cmd (:cmd %)] (or (= cmd :note-on) (= cmd :note-off)))
          (filter :cmd (:events track))))

(defn print-note-event
  [evt]
  (println (format "@%d Channel: %d %s, %s octave=%d key=%s vel: %d"
                   (:tick evt)
                   (:channel evt)
                   (:cmd evt)
                   (:note-name evt)
                   (:octave evt)
                   (:key evt)
                   (:vel evt))))

(defn print-track
  [track]
  (println "Track ticks:" (:ticks track))
  (doseq [evt (track-note-events track)]
    (print-note-event evt)))

(defn- remove-event
  "Remove an event from a list/vector
   returns [event-removed updated-list]"
  [events channel key]
  (loop [events (apply list events)
         checked '()]
    (if (empty? events)
      [nil (reverse checked)]
      (let [cur-evt (first events)]
        (if (and (= key (:key cur-evt))
                 (= channel (:channel cur-evt)))
          [cur-evt (concat (reverse checked) (rest events))]
          (recur (rest events)
                 (conj checked cur-evt)))))))
(defn- add-event
  [events on off]
  (let [on-tick (:tick on)
        off-tick (:tick off)
        duration (- off-tick on-tick)
        event {:tick on-tick
               :duration duration
               :channel (:channel on)
               :key (:key on)
               :vel (:vel on)}]
    (conj events event)))
(defn track->events
  "Generate a list of {:tick :channel :key :vel :duration} entries for each channel on this track"
  [track]
  (loop [raw-events (track-note-events track)
         events []
         started []]
    (if (empty? raw-events)
      events
      (let [cur-event (first raw-events)
            cur-key (:key cur-event)
            cur-channel (:channel cur-event)]
        (case (:cmd cur-event)
          :note-on
          (recur (rest raw-events)
                 events
                 (conj started cur-event))
          :note-off
          (let [[on-event new-started]
                (remove-event started (:channel cur-event) (:key cur-event))]
            (recur (rest raw-events)
                         (add-event events on-event cur-event)
                         new-started))
          (do
            (println "Invalid cmd")
            (recur (rest raw-events)
                   events
                   started)))))))

;;----------PLAYBACK------------
  
(let [stop-sequencer (fn stop-sequencer
                       [sequencer]
                       (doto sequencer
                         (.stop)
                         (.close)))]
  (defn play-midi-file
    ([filename]
       (play-midi-file filename 5000))
    ([filename time]
       (let [sequence (midi-file-sequence filename)
             sequencer (MidiSystem/getSequencer)]
         (doto sequencer
           (.setSequence sequence)
           (.open)
           (.start))
         (.start
          (Thread. 
           (fn []
             (Thread/sleep time)
             (stop-sequencer sequencer))))))))

(defn short-message
  [cmd channel note vel]
  (ShortMessage. cmd channel note vel))

(defn player-events
  [{:keys [event-queue recv] :as player}]
   (let [cur-time (System/currentTimeMillis)
         {cur-events true other-events false} (group-by #(< (:at %) cur-time) event-queue)]
     (doseq [{:keys [action data]} cur-events]
       (action player data))
     (assoc player :event-queue other-events)))

(defn new-player
  []
  (let [synth (MidiSystem/getSynthesizer)
        recv (.getReceiver synth)]
    {:synth synth
     :recv recv
     :event-queue []}))

(let [player-event-thread-fn
      (fn player-event-thread-fn
        [player]
        (fn []
          (try
            (while true
              (swap! player player-events)
              (Thread/sleep 1))
            (catch InterruptedException e
              (println "Ending player event thread")))))]

  (defn add-player-event-thread!
    "Take in a player atom, and start an event thread for it"
    [player-atom]
    (let [event-thread (Thread. (player-event-thread-fn player-atom))]
      (.start event-thread)
      (swap! player-atom assoc :event-thread event-thread))))
(defn open-player
  [{synth :synth :as player}]
  (.open synth)
    (-> player
        (assoc :start (System/currentTimeMillis))
        (assoc :event-queue [])))
(defn close-player
  [{:keys [synth event-thread] :as player}]
  (when event-thread
    (.interrupt event-thread))
  (when (and synth (.isOpen synth))
    (.close synth))
  nil) ;;clear out player

(defn- note-on-msg
  [{:keys [channel key vel]}]
  (short-message ShortMessage/NOTE_ON channel key vel))
(defn- note-off-msg
  [{:keys [channel key vel]}]
  (short-message ShortMessage/NOTE_OFF channel key vel))

(defn start-note
  [{:keys [recv] :as player} note-info]
  (.send recv (note-on-msg note-info) -1))

(defn stop-note
  [{:keys [recv] :as player} note-info]
  (.send recv (note-off-msg note-info) -1))

(defn- note-off-event
  [note-info after]
  {:action stop-note
   :data note-info
   :at (+ after (System/currentTimeMillis))})

(defn- remove-note-events
  "Remove any events associated with the provided note-info"
  [event-queue {:keys [channel key vel]}]
  (remove (fn [{{cur-channel :channel
                 cur-key :key
                 cur-vel :vel} :data}]
            (and (= cur-channel channel)
                 (= cur-key key)
                 (= cur-vel vel)))
          event-queue))

(declare play-note)
(defn play-note-later-event
  [{:keys [at] :as note-info}]
  {:at at :action play-note :data note-info})
(defn play-note-later
  "Sets up a note-info with an :at key to play when :at is reached"
  [{:keys [recv event-queue] :as player} note-info]
  (update-in  player [:event-queue] conj (play-note-later-event note-info)))

(defn play-note
  [{:keys [recv event-queue] :as player} {:keys [duration] :as note-info}]
  (stop-note player note-info)
  (start-note player note-info)
  (-> player
      ;;Remove any existing stop events for this note
      (update-in [:event-queue] remove-note-events note-info)
      ;;Add the new stop event
      (update-in [:event-queue] conj (note-off-event note-info duration))))

(defn play-note-old
  [note vel duration]
  (let [sm (short-message ShortMessage/NOTE_ON 0 note vel)
        em (short-message ShortMessage/NOTE_OFF 0 note vel)
        synth (MidiSystem/getSynthesizer)
        recv (.getReceiver synth)]
    (.open synth)
    (.send recv sm -1) ;;-1 means no time stamp
    (Thread/sleep duration)
    (.send recv em -1)
    (.close synth)))

(defn gen-note-info
  [channel key vel duration]
  {:channel channel
   :key key
   :vel vel
   :duration duration})
(def test-note
  (gen-note-info 0 60 90 1000))
(def test-notes
  (map #(gen-note-info 0 % 90 500)
       (range 55 65 2)))
(def test-notes2
  (map #(gen-note-info 0 % 90 250)
       (range 54 65 2)))

(defn convert-ticks-to-ms
  "Converts :tick in events into :at time references"
  ([events tick-ms]
     (convert-ticks-to-ms events tick-ms (System/currentTimeMillis)))
  ([events tick-ms start-time]
     (map #(assoc % :at
                  (+ start-time (* tick-ms (:tick %))))
          events)))

(defn bpm->ticks-per-ms
  [bpm]
  (int (/ 60000 (* 96 bpm)))) ;;96 ticks per beat
  
(defn play-track-events-in-bg
  "Plays a set of events (uses track->events)"
  [player track {:keys [bpm]}]
  (let [events (map play-note-later-event
                    (convert-ticks-to-ms (track->events track) (bpm->ticks-per-ms bpm)))]
    (update-in player [:event-queue] concat events)))

(defn play-parsed-midi
  [player parsed bpm]
  (loop [player player
         tracks (:tracks parsed)]
    (if (empty? tracks)
      player
      (recur (play-track-events-in-bg player (first tracks) {:bpm bpm})
             (rest tracks)))))

(defn play-notes-in-bg
  "Play notes in the bg, either specifying the delay between notes or
   playing one after the next"
  ([player notes delay]
     (.start
      (Thread.
       (fn []
         (doseq [n notes]
           (swap! player play-note n)
           (Thread/sleep delay))))))
  ([player notes]
     (.start
      (Thread.
       (fn []
         (doseq [{:keys [duration] :as n} notes]
           (swap! player play-note n)
           (Thread/sleep duration)))))))

(defn play-single-note
  [note-info]
  (let [player (atom (new-player))]
    (swap! player open-player)
    (swap! player play-note note-info)
    (while (not (empty? (:event-queue @player)))
      (Thread/sleep 1)
      (swap! player player-events))
    (swap! player close-player)))

(defn song-test
  "Play lion sleeps tonight"
  [player]
  (play-notes-in-bg 
   player 
   (map (comp 
         #(gen-note-info 0 % 90 250)
         note-desc->key) 
        ["c4" "d4" "e4" "d4" "e4" "f4" "e4"
         "d4" "c4" "d4" "e4" "d4" "c4" "e4" "d4"])
   250))

(defn restart-a-player
  [player]
  (swap! player close-player)
  (reset! player (new-player))
  (swap! player open-player)
  (add-player-event-thread! player))

(comment

  (def tmp (parse-midi "midi/fur_elise.mid"))
  
  (def player (atom (new-player)))
  (swap! player open-player)
  (add-player-event-thread! player)
  (swap! player play-parsed-midi tmp 200)

  (swap! player play-note {:key 60 :duration 1000 :channel 0 :vel 90})
  (swap! player play-note (gen-note-info 0 (note-desc->key "c4") 90 1000))
  (doseq [n test-notes] (swap! player play-note n) (Thread/sleep 250))
  (swap! player close-player)


)
