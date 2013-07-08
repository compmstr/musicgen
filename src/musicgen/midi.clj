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
  (let [midi-seq (MidiSystem/getSequence (File. (midi-filename filename)))]
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
  

(comment

  (def tmp (parse-midi "midi/fur_elise.mid"))

)
