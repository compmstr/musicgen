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


(defn- event-process-key
  "Add readable note/octave/note-name to note event"
  [event]
  (let [key (:key event)
        note (mod key 12)
        octave (dec (quot key 12))
        note-name (note-names note)]
    (assoc event
      :note note
      :octave octave
      :note-name note-name)))

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
             :velocity (.getData2 msg)}))
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
                   (:velocity evt))))

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
  ;;TODO
  events)
(defn track->events
  "Generate a list of {:tick :channel :key :velocity :duration} entries for each channel on this track"
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
  
(defn- stop-sequencer
  [sequencer]
  (doto sequencer
    (.stop)
    (.close)))
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
           (stop-sequencer sequencer)))))))

(defn short-message
  [cmd channel note vel]
  (ShortMessage. cmd channel note vel))

(defn new-player
  []
  (let [synth (MidiSystem/getSynthesizer)
        recv (.getReceiver synth)]
    {:synth synth
     :recv recv
     :event-queue []}))
(defn open-player
  [{synth :synth :as player}]
  (.open synth)
  (-> player
      (assoc :start (System/currentTimeMillis))
      (assoc :event-queue [])))
(defn close-player
  [{synth :synth :as player}]
  (.close synth)
  nil) ;;clear out player

(defn- note-on-msg
  [{:keys [channel note vel]}]
  (short-message ShortMessage/NOTE_ON channel note vel))
(defn- note-off-msg
  [{:keys [channel note vel]}]
  (short-message ShortMessage/NOTE_OFF channel note vel))

(defn stop-note
  [{:keys [recv] :as player} note-info]
  (.send recv (note-off-msg note-info) -1))

(defn- note-off-event
  [note-info after]
  {:action stop-note
   :data note-info
   :at (+ after (System/currentTimeMillis))})

(defn play-note
  [{:keys [recv event-queue] :as player} {:keys [duration] :as note-info}]
  (let [on-msg (note-on-msg note-info)]
    (.send recv on-msg -1)
    (update-in player [:event-queue] conj (note-off-event note-info duration))))

(defn player-events
  [{:keys [event-queue recv] :as player}]
   (let [cur-time (System/currentTimeMillis)
         {cur-events true other-events false} (group-by #(< (:at %) cur-time) event-queue)]
     (doseq [{:keys [action data]} cur-events]
       (action player data))
     (assoc player :event-queue other-events)))

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
  [channel note vel duration]
  {:channel channel
   :note note
   :vel vel
   :duration duration})
(def test-note
  (gen-note-info 0 60 90 1000))
(defn play-single-note
  [note-info]
  (let [player (atom (new-player))]
    (swap! player open-player)
    (swap! player play-note note-info)
    (while (not (empty? (:event-queue @player)))
      (Thread/sleep 1)
      (swap! player player-events))
    (swap! player close-player)))

(comment

  (def tmp (parse-midi "midi/fur_elise.mid"))
  
  (def player (atom (new-player)))
  (swap! player open-player)
  (swap! player play-note {:note 60 :duration 1000 :channel 0 :vel 90})
  (while (not (empty? (:event-queue @player)))
    (swap! player player-events))
  (swap! player close-player)

)
