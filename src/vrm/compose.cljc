(ns vrm.compose
  "Part composition: merge multiple VrmParts into a single VrmDocument.
  Restored from `kami-vrm/src/compose.rs` (kotoba-lang/kami-engine, deleted
  PR #82) as part of the clj-wgsl migration (ADR-2607010930,
  com-junkawasaki/root).

  `PartSource` = `{:part VrmPart :doc VrmDocument}`. `ComposeConfig` =
  `{:skeleton-base index}` (index into `sources` whose skeleton is the
  canonical armature)."
  (:require [vrm.gltf-types :as gt]
            [vrm.vrm-types :as vt]))

(defn- find-parent-in-remap*
  "Walk up `doc`'s node tree from `node-idx` to find a mapped ancestor for
  `src-idx` in `node-remap` (`{[src-idx old-node-idx] new-node-idx}`),
  recursing through unmapped ancestors (faithful port of the Rust original's
  recursive walk)."
  [doc node-idx src-idx node-remap]
  (let [nodes (:nodes (:gltf doc))
        parent-idx (some (fn [[i n]] (when (some #{node-idx} (:children n)) i))
                          (map-indexed vector nodes))]
    (when parent-idx
      (if-let [mapped (get node-remap [src-idx parent-idx])]
        mapped
        (find-parent-in-remap* doc parent-idx src-idx node-remap)))))

(defn compose
  "Compose multiple VRM parts (`sources`, a seq of `PartSource`) into a
  single VrmDocument, per `config` (`ComposeConfig`). Phases: skeleton
  unification -> buffer merge -> mesh merge -> joint remap -> material merge
  -> spring bone merge -> expression merge -> rebuild."
  [sources config]
  (when (empty? sources) (throw (ex-info "part error: no sources provided" {})))
  (let [sources (vec sources)
        skeleton-base (:skeleton-base config)
        base-src (get sources skeleton-base)]
    (when-not base-src (throw (ex-info "part error: skeleton_base out of range" {})))
    (let [base-doc (:doc base-src)
          base-gltf (:gltf base-doc)

          ;; ── Phase 1: skeleton unification ──
          base-node-count (count (:nodes base-gltf))
          node-remap0 (into {} (for [i (range base-node-count)] [[skeleton-base i] i]))
          base-bone-map (into {} (map (fn [hb] [(:bone hb) (:node hb)])) (:human-bones (:humanoid base-doc)))

          unify-source
          (fn [[unified-nodes node-remap] src-idx src]
            (if (= src-idx skeleton-base)
              [unified-nodes node-remap]
              (let [src-doc (:doc src)
                    src-gltf (:gltf src-doc)
                    src-bone-map (into {} (map (fn [hb] [(:bone hb) (:node hb)])) (:human-bones (:humanoid src-doc)))
                    ;; Map humanoid bones to base.
                    node-remap (reduce
                                (fn [nr [bone-name src-node]]
                                  (if-let [base-node (get base-bone-map bone-name)]
                                    (assoc nr [src-idx src-node] base-node)
                                    nr))
                                node-remap
                                src-bone-map)
                    ;; Append non-humanoid nodes.
                    [unified-nodes node-remap]
                    (reduce
                     (fn [[unified-nodes node-remap] ni]
                       (if (contains? node-remap [src-idx ni])
                         [unified-nodes node-remap]
                         (let [parent-new (find-parent-in-remap* src-doc ni src-idx node-remap)
                               new-node (-> (get (:nodes src-gltf) ni)
                                            (assoc :children [] :mesh nil))
                               new-idx (count unified-nodes)
                               unified-nodes (conj unified-nodes new-node)
                               node-remap (assoc node-remap [src-idx ni] new-idx)
                               unified-nodes (if parent-new
                                               (update unified-nodes parent-new
                                                       #(update % :children (fnil conj []) new-idx))
                                               unified-nodes)]
                           [unified-nodes node-remap])))
                     [unified-nodes node-remap]
                     (:node-indices (:part src)))
                    ;; Re-link children for appended nodes.
                    unified-nodes
                    (reduce
                     (fn [unified-nodes ni]
                       (if-let [new-ni (get node-remap [src-idx ni])]
                         (if (>= new-ni (count (:nodes base-gltf)))
                           (let [old-children (:children (get (:nodes src-gltf) ni))
                                 new-children (vec (keep #(get node-remap [src-idx %]) old-children))]
                             (assoc-in unified-nodes [new-ni :children] new-children))
                           unified-nodes)
                         unified-nodes))
                     unified-nodes
                     (:node-indices (:part src)))]
                [unified-nodes node-remap])))

          [unified-nodes node-remap]
          (reduce (fn [acc [src-idx src]] (unify-source acc src-idx src))
                   [(vec (:nodes base-gltf)) node-remap0]
                   (map-indexed vector sources))

          ;; ── Phase 2: buffer merging ──
          buffer-merge
          (reduce
           (fn [{:keys [bin buffer-view-remap accessor-remap buffer-views accessors] :as acc} [src-idx src]]
             (let [src-doc (:doc src)
                   src-gltf (:gltf src-doc)
                   base-offset (count bin)
                   bin (into bin (:bin src-doc))
                   pad (mod (- 4 (mod (count bin) 4)) 4)
                   bin (into bin (repeat pad 0))
                   [buffer-view-remap buffer-views]
                   (reduce
                    (fn [[bvr bvs] [bv-idx bv]]
                      (let [new-bv-idx (count bvs)]
                        [(assoc bvr [src-idx bv-idx] new-bv-idx)
                         (conj bvs {:buffer 0
                                    :byteOffset (+ (or (:byteOffset bv) 0) base-offset)
                                    :byteLength (:byteLength bv)
                                    :byteStride (:byteStride bv)
                                    :target (:target bv)})]))
                    [buffer-view-remap buffer-views]
                    (map-indexed vector (:bufferViews src-gltf)))
                   [accessor-remap accessors]
                   (reduce
                    (fn [[ar accs] [acc-idx acc-val]]
                      (let [new-acc-idx (count accs)
                            new-bv (when-let [bv (:bufferView acc-val)] (get buffer-view-remap [src-idx bv]))]
                        [(assoc ar [src-idx acc-idx] new-acc-idx)
                         (conj accs {:bufferView new-bv
                                     :componentType (:componentType acc-val)
                                     :count (:count acc-val)
                                     :type (:type acc-val)
                                     :byteOffset (or (:byteOffset acc-val) 0)
                                     :min (:min acc-val)
                                     :max (:max acc-val)
                                     :normalized (boolean (:normalized acc-val))})]))
                    [accessor-remap accessors]
                    (map-indexed vector (:accessors src-gltf)))]
               {:bin bin :buffer-view-remap buffer-view-remap :accessor-remap accessor-remap
                :buffer-views buffer-views :accessors accessors}))
           {:bin [] :buffer-view-remap {} :accessor-remap {} :buffer-views [] :accessors []}
           (map-indexed vector sources))
          {:keys [bin buffer-view-remap accessor-remap buffer-views accessors]} buffer-merge

          ;; ── Phase 3/4: mesh/material/texture/image merging ──
          merge-result
          (reduce
           (fn [{:keys [images image-remap samplers textures texture-remap
                        materials material-remap meshes mesh-remap unified-nodes] :as acc}
                [src-idx src]]
             (let [src-doc (:doc src)
                   src-gltf (:gltf src-doc)
                   ;; Images
                   [images image-remap]
                   (reduce
                    (fn [[imgs ir] [img-idx img]]
                      (if (contains? ir [src-idx img-idx])
                        [imgs ir]
                        (let [new-idx (count imgs)
                              new-img (cond-> img
                                        (:bufferView img)
                                        (assoc :bufferView (get buffer-view-remap [src-idx (:bufferView img)])))]
                          [(conj imgs new-img) (assoc ir [src-idx img-idx] new-idx)])))
                    [images image-remap]
                    (map-indexed vector (:images src-gltf)))
                   sampler-base (count samplers)
                   samplers (into samplers (:samplers src-gltf))
                   ;; Textures
                   [textures texture-remap]
                   (reduce
                    (fn [[txs tr] [tex-idx tex]]
                      (if (contains? tr [src-idx tex-idx])
                        [txs tr]
                        (let [new-idx (count txs)]
                          [(conj txs {:sampler (when (:sampler tex) (+ (:sampler tex) sampler-base))
                                      :source (when (:source tex) (get image-remap [src-idx (:source tex)]))})
                           (assoc tr [src-idx tex-idx] new-idx)])))
                    [textures texture-remap]
                    (map-indexed vector (:textures src-gltf)))
                   ;; Materials
                   [materials material-remap]
                   (reduce
                    (fn [[mats mr] mat-idx]
                      (if (contains? mr [src-idx mat-idx])
                        [mats mr]
                        (if-let [mat (get (:materials src-gltf) mat-idx)]
                          (let [new-idx (count mats)
                                remap-tex (fn [ti] (get texture-remap [src-idx ti] ti))
                                new-mat (cond-> mat
                                          (get-in mat [:pbrMetallicRoughness :baseColorTexture :index])
                                          (update-in [:pbrMetallicRoughness :baseColorTexture :index] remap-tex)
                                          (get-in mat [:pbrMetallicRoughness :metallicRoughnessTexture :index])
                                          (update-in [:pbrMetallicRoughness :metallicRoughnessTexture :index] remap-tex))]
                            [(conj mats new-mat) (assoc mr [src-idx mat-idx] new-idx)])
                          [mats mr])))
                    [materials material-remap]
                    (:material-indices (:part src)))
                   ;; Meshes
                   [meshes mesh-remap unified-nodes]
                   (reduce
                    (fn [[ms mr nodes] mi]
                      (if-let [mesh (get (:meshes src-gltf) mi)]
                        (let [new-mi (count ms)
                              remap-attr-map
                              (fn [attrs]
                                (into {} (map (fn [[k v]]
                                                (if (number? v)
                                                  [k (get accessor-remap [src-idx v] v)]
                                                  [k v])))
                                      attrs))
                              new-mesh (update mesh :primitives
                                               (fn [prims]
                                                 (mapv (fn [prim]
                                                         (cond-> prim
                                                           true (update :attributes remap-attr-map)
                                                           (:indices prim) (update :indices #(get accessor-remap [src-idx %] %))
                                                           (:material prim) (update :material #(get material-remap [src-idx %] %))
                                                           (seq (:targets prim)) (update :targets #(mapv remap-attr-map %))))
                                                       prims)))
                              ms (conj ms new-mesh)
                              mr (assoc mr [src-idx mi] new-mi)
                              ;; Attach mesh to correct node.
                              owning-node (some (fn [[ni n]] (when (= (:mesh n) mi) ni))
                                                 (map-indexed vector (:nodes src-gltf)))
                              nodes (if (and owning-node (some #{owning-node} (:node-indices (:part src))))
                                      (if-let [new-ni (get node-remap [src-idx owning-node])]
                                        (if (< new-ni (count nodes))
                                          (assoc-in nodes [new-ni :mesh] new-mi)
                                          nodes)
                                        nodes)
                                      nodes)]
                          [ms mr nodes])
                        [ms mr nodes]))
                    [meshes mesh-remap unified-nodes]
                    (:mesh-indices (:part src)))]
               {:images images :image-remap image-remap :samplers samplers
                :textures textures :texture-remap texture-remap
                :materials materials :material-remap material-remap
                :meshes meshes :mesh-remap mesh-remap :unified-nodes unified-nodes}))
           {:images [] :image-remap {} :samplers [] :textures [] :texture-remap {}
            :materials [] :material-remap {} :meshes [] :mesh-remap {} :unified-nodes unified-nodes}
           (map-indexed vector sources))
          {:keys [images samplers textures texture-remap materials material-remap
                  meshes mesh-remap unified-nodes]} merge-result

          ;; ── Phase 5: skin rebuild ──
          base-skin (first (:skins base-gltf))
          unified-skin
          (when base-skin
            (let [joint-set
                  (reduce
                   (fn [joints [src-idx src]]
                     (if (= src-idx skeleton-base)
                       joints
                       (if-let [src-skin (first (:skins (:gltf (:doc src))))]
                         (reduce (fn [joints old-joint]
                                   (if-let [new-joint (get node-remap [src-idx old-joint])]
                                     (if (some #{new-joint} joints) joints (conj joints new-joint))
                                     joints))
                                 joints (:joints src-skin))
                         joints)))
                   (vec (:joints base-skin))
                   (map-indexed vector sources))
                  ibm-acc-idx (when-let [ibm-idx (:inverseBindMatrices base-skin)]
                                (get accessor-remap [skeleton-base ibm-idx]))]
              {:name (:name base-skin)
               :joints joint-set
               :inverseBindMatrices ibm-acc-idx
               :skeleton (when-let [s (:skeleton base-skin)] (get node-remap [skeleton-base s]))}))
          unified-nodes
          (if unified-skin
            (mapv (fn [n] (if (:mesh n) (assoc n :skin 0) n)) unified-nodes)
            unified-nodes)

          ;; ── Phase 6: spring bone merging ──
          spring-merge
          (reduce
           (fn [{:keys [colliders collider-remap collider-groups collider-group-remap spring-bones] :as acc}
                [src-idx src]]
             (let [src-doc (:doc src)
                   [colliders collider-remap]
                   (reduce
                    (fn [[cs cr] ci]
                      (if-let [collider (get (:spring-bone-colliders src-doc) ci)]
                        (let [new-ci (count cs)]
                          [(conj cs (assoc collider :node (get node-remap [src-idx (:node collider)] (:node collider))))
                           (assoc cr [src-idx ci] new-ci)])
                        [cs cr]))
                    [colliders collider-remap]
                    (:collider-indices (:part src)))
                   [collider-groups collider-group-remap]
                   (reduce
                    (fn [[cgs cgr] [gi group]]
                      (if (some (set (:collider-indices (:part src))) (:colliders group))
                        (let [new-gi (count cgs)]
                          [(conj cgs {:name (:name group)
                                      :colliders (vec (keep #(get collider-remap [src-idx %]) (:colliders group)))})
                           (assoc cgr [src-idx gi] new-gi)])
                        [cgs cgr]))
                    [collider-groups collider-group-remap]
                    (map-indexed vector (:spring-bone-collider-groups src-doc)))
                   spring-bones
                   (reduce
                    (fn [sbs sbi]
                      (if-let [chain (get (:spring-bones src-doc) sbi)]
                        (conj sbs
                              {:name (:name chain)
                               :joints (mapv (fn [j] (assoc j :node (get node-remap [src-idx (:node j)] (:node j))))
                                             (:joints chain))
                               :collider-groups (vec (keep #(get collider-group-remap [src-idx %]) (:collider-groups chain)))
                               :center (when-let [c (:center chain)] (get node-remap [src-idx c]))})
                        sbs))
                    spring-bones
                    (:spring-bone-indices (:part src)))]
               {:colliders colliders :collider-remap collider-remap
                :collider-groups collider-groups :collider-group-remap collider-group-remap
                :spring-bones spring-bones}))
           {:colliders [] :collider-remap {} :collider-groups [] :collider-group-remap {} :spring-bones []}
           (map-indexed vector sources))
          {:keys [colliders collider-groups spring-bones]} spring-merge

          ;; ── Phase 7: expression merging ──
          expressions
          (reduce
           (fn [unified [src-idx src]]
             (let [src-doc (:doc src)]
               (reduce
                (fn [unified ei]
                  (if-let [expr (get (:expressions src-doc) ei)]
                    (let [existing-idx (when (:preset expr)
                                          (some (fn [[i e]] (when (= (:preset e) (:preset expr)) i))
                                                (map-indexed vector unified)))]
                      (if existing-idx
                        (-> unified
                            (update-in [existing-idx :morph-target-binds]
                                       into (map (fn [b] (assoc b :mesh-index (get mesh-remap [src-idx (:mesh-index b)] (:mesh-index b))))
                                                 (:morph-target-binds expr)))
                            (update-in [existing-idx :material-color-binds]
                                       into (map (fn [b] (assoc b :material-index (get material-remap [src-idx (:material-index b)] (:material-index b))))
                                                 (:material-color-binds expr))))
                        (conj unified
                              (-> expr
                                  (update :morph-target-binds
                                          (fn [bs] (mapv #(assoc % :mesh-index (get mesh-remap [src-idx (:mesh-index %)] (:mesh-index %))) bs)))
                                  (update :material-color-binds
                                          (fn [bs] (mapv #(assoc % :material-index (get material-remap [src-idx (:material-index %)] (:material-index %))) bs)))
                                  (update :texture-transform-binds
                                          (fn [bs] (mapv #(assoc % :material-index (get material-remap [src-idx (:material-index %)] (:material-index %))) bs)))))))
                    unified))
                unified
                (:expression-indices (:part src)))))
           []
           (map-indexed vector sources))

          ;; ── Phase 8: MToon material merging ──
          mtoon
          (reduce
           (fn [unified [src-idx src]]
             (let [src-doc (:doc src)]
               (into unified
                     (comp
                      (filter #(some #{(:material-index %)} (:material-indices (:part src))))
                      (map (fn [m]
                             (-> m
                                 (assoc :material-index (get material-remap [src-idx (:material-index m)] (:material-index m)))
                                 (update :shade-multiply-texture #(when % (get texture-remap [src-idx %])))
                                 (update :rim-multiply-texture #(when % (get texture-remap [src-idx %])))
                                 (update :matcap-texture #(when % (get texture-remap [src-idx %])))))))
                     (:mtoon-materials src-doc))))
           []
           (map-indexed vector sources))

          ;; ── Build unified scene ──
          scene-nodes (or (some-> (:scenes base-gltf) first :nodes) [0])
          unified-gltf
          (gt/gltf-document
           {:asset (gt/asset {:generator "kami-vrm"})
            :scene 0
            :scenes [{:nodes scene-nodes}]
            :nodes unified-nodes
            :meshes meshes
            :accessors accessors
            :bufferViews buffer-views
            :buffers [{:byteLength (count bin)}]
            :materials materials
            :textures textures
            :images images
            :samplers samplers
            :skins (if unified-skin [unified-skin] [])
            :animations []
            :extensionsUsed ["VRMC_vrm" "VRMC_springBone" "VRMC_materials_mtoon"]
            :extensionsRequired []
            :extensions nil})]
      (vt/vrm-document
       {:gltf unified-gltf
        :bin bin
        :version :v1-0
        :meta (:meta base-doc)
        :humanoid (vt/vrm-humanoid
                   (mapv (fn [hb] (vt/vrm-human-bone (:bone hb) (get node-remap [skeleton-base (:node hb)] (:node hb))))
                         (:human-bones (:humanoid base-doc))))
        :expressions expressions
        :spring-bones spring-bones
        :spring-bone-colliders colliders
        :spring-bone-collider-groups collider-groups
        :mtoon-materials mtoon
        :look-at (:look-at base-doc)
        :first-person (:first-person base-doc)
        :node-constraints []}))))
