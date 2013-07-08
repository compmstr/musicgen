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

(defn parse-track-event
  [event]
  (let [tick (.getTick event)
        msg (.getMessage event)]
    (if (instance? ShortMessage msg)
      (case (midi-cmd (.getCommand msg))
        (:note-on :note-off)
        (let [msg-key (.getData1 msg)
              note (mod msg-key 12)]
          {:cmd (midi-cmd (.getCommand msg))
           :channel (.getChannel msg)
           :key msg-key
           :octave (dec (quot msg-key 12))
           :note note
           :tick tick
           :note-name (note-names note)
           :velocity (.getData2 msg)})
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
