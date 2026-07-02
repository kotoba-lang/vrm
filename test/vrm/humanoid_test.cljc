(ns vrm.humanoid-test
  "Restoration-fidelity tests — mirrors `kami-vrm/src/humanoid.rs`
  `#[cfg(test)] mod tests` (kotoba-lang/kami-engine, deleted PR #82)."
  (:require [clojure.test :refer [deftest is]]
            [vrm.vrm-types :as vt]))

;; mirrors `bone_name_roundtrip`
(deftest bone-name-roundtrip
  (doseq [bone vt/human-bone-names]
    (let [name (vt/human-bone-name->str bone)
          parsed (vt/str->human-bone-name name)]
      (is (= bone parsed)))))
