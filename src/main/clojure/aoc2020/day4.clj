(ns aoc2020.day4
  (:require
    [clojure.string :as str]
    [schema.core :as s]
    [schema.coerce :as coerce]
    [schema.spec.core :as spec]
    [utils :as util]))

(defn string->int [schema]
  (Integer/parseInt schema))

(defn between? [low high num]
  (and (>= num low)
       (<= num high)))

(defn simple-coercion
  "Coerces strings into Int/Keyword"
  [schema]
  (spec/run-checker
    (fn [s params]
      (let [walk (spec/checker (s/spec s) params)]
        (fn [x]
          (condp = s
            s/Keyword (walk (keyword x))
            s/Int (walk (string->int x))
            (walk x)))))
    true
    schema))

(defn is-valid-height [s]
  (let [[_ ht unit] (re-matches #"([0-9]+)(cm|in)" s)
        ht (string->int ht)]
    (condp = unit
      "cm" (between? 150 193 ht)
      "in" (between? 59 76 ht)
      false)))

(def BirthYear (s/constrained s/Int #(between? 1920 2002 %)))
(def IssueYear (s/constrained s/Int #(between? 2010 2020 %)))
(def ExpYear (s/constrained s/Int #(and (>= % 2020) (<= % 2030)) ))
(def HairColor (s/pred #(re-matches #"#[a-f0-9]{6}" %)))
(def EyeColor (s/enum "amb" "blu" "brn" "gry" "grn" "hzl" "oth"))
(def PassId (s/pred #(re-matches #"[0-9]{9}" %)))
(def Height (s/pred is-valid-height))

(def Passport
  {
   (s/required-key "byr") BirthYear
   (s/required-key "iyr") IssueYear
   (s/required-key "eyr") ExpYear
   (s/required-key "hgt") Height
   (s/required-key "hcl") HairColor
   (s/required-key "ecl") EyeColor
   (s/required-key "pid") PassId
   (s/optional-key "cid") s/Any
   }
  )

(defn raw->passport [s]
  (->> (str/split s #"[\n ]")
       (map #(str/split % #":"))
       (into {})
       ((simple-coercion Passport) )))

(defn read-passports []
  (let [raw-data (util/read-file "day4.txt" "\n\n")
        passports (map raw->passport raw-data)
        valid-passports (filter #(not (s/check Passport %)) passports)]
    (comment doseq [s passports]
      (println s)
      (println (s/check Passport s))
      (println "-----")
    )
    (println "Number of valid passports: " (count valid-passports))))

(read-passports)