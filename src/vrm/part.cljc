(ns vrm.part
  "VRM part decomposition: VrmDocument -> `[VrmPart ...]`. Restored from
  `kami-vrm/src/part.rs` (kotoba-lang/kami-engine, deleted PR #82) as part of
  the clj-wgsl migration (ADR-2607010930, com-junkawasaki/root).")

;; Category tag for avatar parts.
(def part-categories #{:body :hair :face :outfit :accessory :other})

(defn classify-mesh
  "Classify a mesh node into a category keyword by name heuristics (VRoid
  Studio naming: \"Body\", \"Hair\", \"Face\", \"FaceEyeline\", ...; generic:
  material name keywords)."
  [mesh-name material-names node-name]
  (let [combined (clojure.string/lower-case
                  (str mesh-name " " node-name " " (clojure.string/join " " material-names)))
        has? (fn [s] (clojure.string/includes? combined s))]
    (cond
      (or (has? "hair") (has? "bangs")) :hair
      (or (has? "face") (has? "eye") (has? "mouth") (has? "brow")) :face
      (or (has? "body") (has? "skin")) :body
      (or (has? "cloth") (has? "outfit") (has? "shirt") (has? "pants")
          (has? "dress") (has? "shoe") (has? "tops") (has? "bottoms")) :outfit
      (or (has? "accessory") (has? "hat") (has? "glass") (has? "ribbon")
          (has? "earring") (has? "necklace")) :accessory
      :else :other)))

(defn- collect-material-textures
  "Collect texture indices referenced by a glTF material map into `textures`
  (a vector), appending new ones, returning the updated vector."
  [mat textures]
  (let [pbr (:pbrMetallicRoughness mat)
        add (fn [textures idx] (if (some #{idx} textures) textures (conj textures idx)))]
    (cond-> textures
      (get-in pbr [:baseColorTexture :index]) (add (get-in pbr [:baseColorTexture :index]))
      (get-in pbr [:metallicRoughnessTexture :index]) (add (get-in pbr [:metallicRoughnessTexture :index])))))

(defn decompose
  "Decompose a VrmDocument into swappable parts.

  Strategy: 1) walk the node tree, identify mesh-bearing nodes; 2) classify
  each by name heuristics; 3) for each part, collect transitive closure of
  materials/textures/images; 4) identify spring bone chains referencing
  nodes in this part's subtree; 5) identify expressions that bind to this
  part's meshes."
  [doc]
  (let [gltf (:gltf doc)
        nodes (:nodes gltf)
        meshes (:meshes gltf)
        materials (:materials gltf)
        textures-doc (:textures gltf)
        ;; Group mesh nodes by category, preserving first-encounter order
        ;; (mirrors the original Rust's `Vec` + `iter_mut().find()` scan).
        {:keys [order groups]}
        (reduce
         (fn [{:keys [order groups] :as acc} [node-idx node]]
           (if-let [mesh-idx (:mesh node)]
             (let [mesh (get meshes mesh-idx)]
               (when-not mesh (throw (ex-info "part error: node references missing mesh"
                                               {:node node-idx :mesh mesh-idx})))
               (let [mesh-name (or (:name mesh) "")
                     node-name (or (:name node) "")
                     mat-names (keep (fn [prim] (some-> (:material prim) materials :name))
                                      (:primitives mesh))
                     category (classify-mesh mesh-name mat-names node-name)]
                 (if (contains? groups category)
                   (update-in acc [:groups category]
                              (fn [g] (-> g
                                          (update :mesh-indices conj mesh-idx)
                                          (update :node-indices conj node-idx))))
                   (-> acc
                       (update :order conj category)
                       (assoc-in [:groups category]
                                 {:name (if (seq mesh-name) mesh-name (name category))
                                  :mesh-indices [mesh-idx]
                                  :node-indices [node-idx]})))))
             acc))
         {:order [] :groups {}}
         (map-indexed vector nodes))]
    (vec
     (for [category order
           :let [{:keys [name mesh-indices node-indices]} (get groups category)]]
       (let [material-indices
             (vec (distinct
                   (mapcat (fn [mi]
                             (keep :material (:primitives (get meshes mi))))
                           mesh-indices)))
             texture-indices
             (reduce (fn [ts mat-idx] (collect-material-textures (get materials mat-idx) ts))
                     [] material-indices)
             image-indices
             (vec (distinct
                   (keep (fn [tex-idx] (:source (get textures-doc tex-idx))) texture-indices)))
             all-nodes
             (loop [all-nodes (vec node-indices) i 0]
               (if (>= i (count all-nodes))
                 all-nodes
                 (let [ni (nth all-nodes i)
                       node (get nodes ni)
                       new-children (remove (set all-nodes) (:children node))]
                   (recur (into all-nodes new-children) (inc i)))))
             node-set (set all-nodes)
             spring-bone-indices
             (vec (keep-indexed
                   (fn [i chain] (when (some #(node-set (:node %)) (:joints chain)) i))
                   (:spring-bones doc)))
             collider-indices
             (vec (keep-indexed
                   (fn [i c] (when (node-set (:node c)) i))
                   (:spring-bone-colliders doc)))
             mesh-set (set mesh-indices)
             mat-idx-set (set material-indices)
             expression-indices
             (vec (keep-indexed
                   (fn [i expr]
                     (when (or (some #(mesh-set (:mesh-index %)) (:morph-target-binds expr))
                               (some #(mat-idx-set (:material-index %)) (:material-color-binds expr)))
                       i))
                   (:expressions doc)))]
         {:category category
          :name name
          :mesh-indices mesh-indices
          :material-indices material-indices
          :texture-indices texture-indices
          :image-indices image-indices
          :node-indices all-nodes
          :spring-bone-indices spring-bone-indices
          :collider-indices collider-indices
          :expression-indices expression-indices})))))
