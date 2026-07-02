(ns vrm.glb-test
  "Restoration-fidelity tests — one per original `kami-vrm/src/glb.rs`
  `#[cfg(test)] mod tests` (kotoba-lang/kami-engine, deleted PR #82)."
  (:require [clojure.test :refer [deftest is]]
            [vrm.glb :as glb]))

;; mirrors `roundtrip`
(deftest roundtrip
  (let [json (glb/string->byte-seq "{\"asset\":{\"version\":\"2.0\"}}")
        bin [1 2 3 4 5 6 7]
        out (glb/write-glb json bin)]
    (is (= glb/glb-magic (reduce (fn [acc [i b]] (bit-or acc (bit-shift-left b (* 8 i))))
                                  0 (map-indexed vector (subvec (vec out) 0 4)))))
    (let [version (reduce (fn [acc [i b]] (bit-or acc (bit-shift-left b (* 8 i))))
                           0 (map-indexed vector (subvec (vec out) 4 8)))]
      (is (= glb/glb-version version)))
    (let [total (reduce (fn [acc [i b]] (bit-or acc (bit-shift-left b (* 8 i))))
                         0 (map-indexed vector (subvec (vec out) 8 12)))]
      (is (= total (count out))))
    (let [chunks (glb/parse-glb out)
          parsed-json (clojure.string/trim (glb/byte-seq->string (:json chunks)))]
      (is (= parsed-json "{\"asset\":{\"version\":\"2.0\"}}"))
      (is (= bin (subvec (vec (:bin chunks)) 0 (count bin)))))))

;; mirrors `invalid_magic`
(deftest invalid-magic
  (is (thrown? #?(:clj Exception :cljs js/Error) (glb/parse-glb (vec (repeat 20 0))))))

;; mirrors `too_short`
(deftest too-short
  (is (thrown? #?(:clj Exception :cljs js/Error) (glb/parse-glb [])))
  (is (thrown? #?(:clj Exception :cljs js/Error) (glb/parse-glb (vec (repeat 8 0))))))
