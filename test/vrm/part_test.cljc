(ns vrm.part-test
  "Restoration-fidelity tests — one per original `kami-vrm/src/part.rs`
  `#[cfg(test)] mod tests` (kotoba-lang/kami-engine, deleted PR #82)."
  (:require [clojure.test :refer [deftest is]]
            [vrm.part :as part]))

;; mirrors `classify_hair`
(deftest classify-hair
  (is (= :hair (part/classify-mesh "Hair_001" [] "")))
  (is (= :hair (part/classify-mesh "Bangs" [] "hair_node"))))

;; mirrors `classify_body`
(deftest classify-body
  (is (= :body (part/classify-mesh "Body" [] "")))
  (is (= :body (part/classify-mesh "mesh" ["skin_material"] ""))))

;; mirrors `classify_outfit`
(deftest classify-outfit
  (is (= :outfit (part/classify-mesh "Clothing_Top" [] "")))
  (is (= :outfit (part/classify-mesh "" ["shirt_red"] ""))))

;; mirrors `classify_face`
(deftest classify-face
  (is (= :face (part/classify-mesh "Face" [] "")))
  (is (= :face (part/classify-mesh "FaceEyeline" [] ""))))

;; mirrors `classify_other`
(deftest classify-other
  (is (= :other (part/classify-mesh "Unknown_Part" [] ""))))
