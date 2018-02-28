#! /usr/bin/env lumo

"This script converts Elite Command maps into Zetawar maps.

Input:
- JSON file with one Elite Command map
- Zetawar data.cljs file

Output:
- New Zetawar data.cljs file

Elite Command    | Zetawar
--------------------------
                 | --MAP--
name             | id
name             | description
description      | notes
tiles            | terrains[q]
tiles            | terrains[r]
tiles            | terrains[terrain-type]
                 |
                 | --SCENARIO--
name             | id
name             | description
starting_credits | factions[credits]
bases[player]    | factions[color]
bases[x]         | factions[bases][q]
bases[y]         | factions[bases][r]
bases[base_type] | factions[bases][base-type]
units[player]    | factions[color]
units[x]         | factions[units][q]
units[y]         | factions[units][r]
units[unit_type] | factions[units][unit-type]
                 | factions[ai]
                 | ruleset-id
                 | max-count-per-count
                 | credits-per-base
                 |
id               |
created_at       |
free             |
img_full         |
img_medium       |
official         |
status           |
updated_at       |
"

(require '[clojure.string :as str])

(def node-fs (js/require "fs"))
(def readfile (.-readFileSync node-fs))
(def writefile (.-writeFileSync node-fs))

(def terrain-types
  {0 :plains
   1 :deep-water
   2 :mountains
   3 :woods
   4 :desert
   5 :tundra
   6 :swamp
   7 :shallow-water
   8 :ford})

(def faction-colors
  {1 :red
   2 :blue
   3 :yellow
   4 :pink
   5 :green
   6 :orange})

;;;;;;;;;;;;;;;; Conversion ;;;;;;;;;;;;;;;;

(defn map-ids
  "Convert an Elite Command map name to Zetawar map and scenario ids and descriptions."
  [ec-name]
  (let [descr-str (.replace ec-name "+" "")
        id-str (-> descr-str
                   .toLowerCase
                   (.replace " " "-")
                   (.replace "+" "")
                   (.replace "'" ""))]
    {:map-id (keyword id-str)
     :scenario-id (keyword (str id-str "-multiplayer"))
     :map-description descr-str
     :scenario-description (str descr-str " Multiplayer")}))

(defn most-common-neighbor-terrain
  "Find the most common terrain-type among a tile's neighbors.
  Check all neighboring tiles (up to six). If two terrain-types
  are tied for most frequent, which ever is determined to be the
  'max-key' is selected."
  [tiles q r]
  (let [neighbors (filter #(or (and (= (- q 1) (:q %)) (= (- r 1) (:r %)))
                               (and (= q (:q %)) (= (- r 1) (:r %)))
                               (and (= (- q 1) (:q %)) (= r (:r %)))
                               (and (= (+ q 1) (:q %)) (= r (:r %)))
                               (and (= (- q 1) (:q %)) (= (+ r 1) (:r %)))
                               (and (= q (:q %)) (= (+ r 1) (:r %))))
                          tiles)]
    (->> neighbors
         (map :terrain-type)
         frequencies
         (apply max-key val)
         key)))

(defn generate-terrains-for-bases
  "Generate a terrain-type for a tile with a base on it.
  Leave all other tiles unchanged."
  [tiles]
  (map #(if (= :base-filler (:terrain-type %))
          (assoc % :terrain-type (most-common-neighbor-terrain tiles (:q %) (:r %)))
          %)
       tiles))

(defn terrains
  "Convert terrains to 'terrains' data structure.
  Elite Command represents a map's bases in both its 'terrains' data structure
  and its 'bases' data structure, whereas Zetawar puts an appropiate non-base
  terrain where a base would go in its 'terrains' data structure.
  `generate-terrains-for-bases` fills in those missing terrains."
  [ec-tiles]
  (->> (for [[y row] (map-indexed vector ec-tiles)]
         (for [[x terrain-int] (map-indexed vector row)]
           (cond
             (and (>= terrain-int 0) (<= terrain-int 8))
             {:q x
              :r y
              :terrain-type (terrain-types terrain-int)}

             (and (>= terrain-int 10) (<= terrain-int 12))
             {:q x
              :r y
              :terrain-type :base-filler})))
       flatten
       (remove nil?)
       generate-terrains-for-bases
       (sort-by (juxt :r :q))
       (into [])))

(defn all-bases
  "Convert all bases to 'bases' data structure.
  Elite Command represents a map's bases in a single data structure, whereas
  Zetawar splits it up into a general 'bases' data structure (for location and base-type)   
  and, if a base is initially owned by a faction, that faction's corresponding
  'bases' data structure (for location again).
  This function creates the general 'bases' data structure."
  [ec-bases]
  (->> (for [{:keys [x y base_type]} ec-bases]
         {:q x
          :r y
          :base-type (-> base_type
                         .toLowerCase
                         keyword)})
       (sort-by (juxt :q :r))
       (into [])))

(defn faction-bases 
  "Convert owned bases to corresponding faction's 'bases' data structure.
  Elite Command represents a map's bases in a single data structure, whereas
  Zetawar splits it up into a general 'bases' data structure (location and base-type)   
  and, if a base is initially owned by a faction, that faction's corresponding
  'bases' data structure (location again).
  This function creates a data structure with each faction's units."
  [ec-bases]
  (let [owned-bases (->> ec-bases
                         (filter #(< 0 (:player %)))
                         (group-by :player)
                         sort)]
    (into {}
          (for [[faction-int base-list] owned-bases]
            {(faction-colors faction-int)
             {:bases (->> (for [{:keys [x y]} base-list]
                            {:q x
                             :r y})
                          (sort-by (juxt :q :r))
                          (into []))}}))))

(defn faction-units
  "Convert units to corresponding faction's 'units' data structure.
  Elite Command represents a map's initial units in a single data structure, whereas
  Zetawar splits them up into their faction's corresponding 'units' data structure."
  [ec-units]
  (let [sorted-units (->> ec-units
                          (group-by :player)
                          sort)]
    (into {}
          (for [[faction-int unit-list] sorted-units]
            {(faction-colors faction-int)
             {:units (->> (for [{:keys [x y unit_type]} unit-list]
                            {:q x
                             :r y
                             :unit-type (-> unit_type
                                            .toLowerCase
                                            keyword)})
                          (sort-by (juxt :q :r))
                          (into []))}}))))

(defn factions
  "Create finalized version of 'factions' data structure.
  Merge results of `faction-bases` and `faction-units` with remaining required info
  for each faction: color, credits, and where or not it is an AI. All factions but
  the first are set as AI."
  [ec-bases ec-units credits]
  (let [faction-bases' (faction-bases ec-bases)
        faction-units' (faction-units ec-units)
        units-and-bases (merge-with into faction-bases' faction-units')]
    (into []
          (for [[i [color info]] (map-indexed vector units-and-bases)]
            (assoc info
                   :color color
                   :credits credits
                   :ai (if (> i 0)
                         true
                         false))))))

;;;;;;;;;;;;;;;; Formatting ;;;;;;;;;;;;;;;;

"These functions format the data into the format expected in a Zetawar data.cljs file."

(defn format-terrains-or-bases [entities]
  (str/join "\n" (for [[i t] (map-indexed vector entities)]
                   (let [first-part (= i 0)
                         last-part (= i (- (count entities) 1))]
                     (cond
                       first-part
                       (str "    [" t)
                       last-part
                       (str "     " t "]")
                       :else
                       (str "     " t))))))

(defn format-faction-properties [entities property-name]
  (str/join "\n" (for [[j x] (map-indexed vector entities)]
                   (let [first-part (= j 0)
                         last-part (= j (- (count entities) 1))]
                     (cond
                       (and first-part last-part)
                       (str "      " property-name " [" x "]")
                       first-part
                       (str "      " property-name " [" x)
                       last-part
                       (str "              " x "]")
                       :else
                       (str "              " x))))))

(defn format-factions [factions]
  (str/join "\n" (for [[i t] (map-indexed vector factions)]
                   (let [first-part (= i 0)
                         last-part (= i (- (count factions) 1))]
                     (str (if first-part
                            (str "    [{:color " (:color t) "\n")
                            (str "     {:color " (:color t) "\n"))
                          "      :credits " (:credits t) "\n"
                          "      :ai " (:ai t) "\n"
                          (format-faction-properties (:bases t) :bases)
                          (if (:units t)
                            (str "\n" (format-faction-properties (:units t) :units)))
                          "}"
                          (if last-part "]"))))))

(defn format-map-and-scenario [ec-data]
  (let [{:keys [name tiles bases units description starting_credits]} ec-data
        {:keys [map-id scenario-id map-description scenario-description]} (map-ids name)
        creator "Chris Vincent"
        ruleset-id :zetawar
        max-count-per-count 10
        credits-per-base 100]
    [(str "(def maps\n"
          "  {" map-id "\n"
          "   {:id " map-id "\n"
          "    :description \"" map-description "\"\n"
          "    :created-by \"" creator "\"\n"
          "    :notes \"" description "\"\n"
          "    :terrains\n"
          (format-terrains-or-bases (terrains tiles)) "}\n\n   ")
     (str "(def scenarios\n"
          "  {" scenario-id "\n"
          "   {:id " scenario-id "\n"
          "    :description \"" scenario-description "\"\n"
          "    :created-by \"" creator "\"\n"
          "    :notes \"" description "\"\n"
          "    :ruleset-id " ruleset-id "\n"
          "    :map-id " map-id "\n"
          "    :max-count-per-unit " max-count-per-count "\n"
          "    :credits-per-base " credits-per-base "\n"
          "    :bases\n"
          (format-terrains-or-bases (all-bases bases)) "\n"
          "    :factions\n"
          (format-factions (factions bases units starting_credits)) "}\n\n   ")]))

;;;;;;;;;;;;;;;; Main ;;;;;;;;;;;;;;;;

(defn main [inputfile outputfile]
  (let [input-contents (readfile inputfile "utf8")
        output-contents (readfile outputfile "utf8")
        ec-data (js->clj (js/JSON.parse input-contents) :keywordize-keys true)
        [formatted-map formatted-scenario] (format-map-and-scenario ec-data)
        map-and-scenario-inserted (-> output-contents
                                      (.replace "(def maps\n  {" formatted-map)
                                      (.replace "(def scenarios\n  {" formatted-scenario))]
    (writefile outputfile  map-and-scenario-inserted)))

(if (not= (count *command-line-args*) 2)
  (println (str "Please try again with two arguments:\n"
                "1. Elite Command JSON file\n"
                "2. Zetawar data file"))
  (main (first *command-line-args*) (second *command-line-args*)))
