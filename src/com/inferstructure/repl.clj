(ns com.inferstructure.repl)

(derive clojure.lang.PersistentArrayMap ::Map)
(derive clojure.lang.PersistentVector ::Vector)
(derive clojure.lang.PersistentHashSet ::Set)
(derive clojure.lang.Iterate ::Seq)
(derive clojure.lang.IPersistentList ::Seq)

(defmulti explore
          "Safely explore possibly large map/vector/set/seq structures in the repl.
           threshold - if count is under threshold, recursively explore
           peekn - if over threshold, recursivley explore peekn entries
                   and summarize the rest
           mvsors - map, vector, set, or seq"
          {:arglists '([threshold peekn mvsors])}
          (fn [_ _ c] (class c)))

(defmulti big
          "Used by explore to summaraize a structure with more than threshold
          elements by building a new structure with peekn + 1 elements.
          Generally, peekn elements (e) come from mvsors and are the result of
          calling (explore t p e).
          The other element is either \"n more entries\", where n is (- (count mvsors) p),
          or \"more ...\" if (count mvsors) cannot be known.

          threshold - mvsors is assumed to be > than threshold, passed to calls of explore
          peekn - recursivley explore peekn entries and summarize the rest
          mvsors - map, vector, set, or seq"
          {:arglists '([threshold peekn mvsors])}
          (fn [_ _ c] (class c)))

(defmethod explore ::Map
  [t p m]
  (if (< (count m) t)
    (into {} (map (fn [[key val]] [key (explore t p val)]) m))
    (big t p m)))

(defmethod explore ::Vector
  [t p v]
  (if (< (count v) t)
    (into [] (map (fn [e] (explore t p e)) v))
    (big t p v)))

(defmethod explore ::Set
  [t p s]
  (if (< (count s) t)
    (into #{} (map (fn [e] (explore t p e)) s))
    (big t p s)))

(defmethod explore ::Seq
  [t p s]
  (let [front (take t s)
        more (drop t s)]
    (if (and (< (count front) t)
             (empty? more))
      (map (fn [e] (explore t p e)) s)
      (big t p s))))

(defmethod explore :default
  [_ _ v]
  v)

(defmethod big ::Map
  [t p m]
  (let [peeked (take p m)
        rest (drop p m)
        more (str (count rest) " more entries")]
    (assoc (into {}
                 (map (fn [[key val]] [key (explore t p val)]) peeked))
      :more more)))

(defmethod big ::Vector
  [t p v]
  (let [peeked (take p v)
        rest (drop p v)
        more (str (count rest) " more entries")]
    (conj (mapv (fn [e] (explore t p e)) peeked)
          more)))

(defmethod big ::Set
  [t p s]
  (let [peeked (take p s)
        rest (drop p s)
        more (str (count rest) " more entries")]
    (conj (into #{} (map (fn [e] (explore t p e)) peeked))
          more)))

(defmethod big ::Seq
  [t p s]
  (let [peeked (take p s)
        more "more ..."]
    (concat (map (fn [e] (explore t p e)) peeked)
            (list more))))
