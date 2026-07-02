(ns vrm
  "Zero-dep portable CLJC. Restored from the legacy kami-engine/kami-vrm Rust
  crate (deleted in kotoba-lang/kami-engine PR #82 'Remove Rust workspace
  from kami-engine') as part of the clj-wgsl migration (ADR-2607010930,
  com-junkawasaki/root).

  VRM (Virtual Reality Model, a glTF 2.0 extension) avatar part composition:
  parse, decompose, compose, export — plus the runtime systems the original
  crate defined data for but didn't apply (spring-bone physics, node
  constraints, expression resolution, first-person visibility).

  Pipeline:
  ```
  GLB bytes  -> vrm.parse/parse-vrm    -> VrmDocument
  VrmDocument -> vrm.part/decompose     -> [VrmPart ...]
  [VrmPart ...] -> vrm.compose/compose  -> VrmDocument
  VrmDocument -> vrm.export/export-glb  -> GLB bytes
  ```

  Modules: `vrm.glb` (GLB binary container), `vrm.gltf-types` (glTF 2.0
  constants/defaults), `vrm.vrm-types` (VRM data shapes), `vrm.parse` (VRM
  1.0 parser), `vrm.compat` (VRM 0.x -> 1.0 conversion), `vrm.humanoid`
  (bone mapping <-> skeleton), `vrm.part` (decomposition), `vrm.compose`
  (part merging), `vrm.export` (GLB writer), `vrm.convert` (accessor ->
  vertex/index buffers), `vrm.spring` (spring-bone physics), `vrm.constraint`
  (node constraint solver), `vrm.expression` (expression weight resolver),
  `vrm.firstperson` (first-person visibility resolver). Plus supporting
  infrastructure not present in the Rust original (which used external
  crates): `vrm.json` (dependency-free JSON parser/serializer) and
  `vrm.math` (glam-equivalent Vec3/Quat/Mat4, matching
  `kotoba-lang/skeleton`'s `skeleton/math.cljc` conventions).

  Errors are raised via `ex-info` with a `:vrm/error` key (see `error-info`)
  in place of the original `VrmError` enum — callers can `ex-data` to
  inspect `{:vrm/error <keyword> ...}`. Native execution (wgpu / wasmtime /
  wasmi) stays substrate; this namespace owns the CLJC contracts / data
  interpreters / EDN IR for the domain."
  (:require [vrm.glb :as glb]
            [vrm.gltf-types :as gltf-types]
            [vrm.vrm-types :as vrm-types]
            [vrm.parse :as parse]
            [vrm.compat :as compat]
            [vrm.humanoid :as humanoid]
            [vrm.part :as part]
            [vrm.compose :as compose]
            [vrm.export :as export]
            [vrm.convert :as convert]
            [vrm.spring :as spring]
            [vrm.constraint :as constraint]
            [vrm.expression :as expression]
            [vrm.firstperson :as firstperson]))

;; ── Re-exports for convenience (mirrors the original `pub use` set) ──

(def parse-vrm parse/parse-vrm)
(def decompose part/decompose)
(def compose compose/compose)
(def export-glb export/export-glb)

(def new-expression-manager expression/new-manager)
(def resolve-expression expression/resolve-expression)

(def new-first-person-resolver firstperson/new-resolver)
(def first-person-node-visible? firstperson/node-visible?)

(def to-kami-skeleton humanoid/to-kami-skeleton)

;; ── Error convention ──

(defn error-info
  "Build an `ex-info` matching the original `VrmError` enum's shape:
  `(error-info :invalid-glb \"reason\")` etc. `kind` is one of
  `:invalid-glb :json :missing-extension :accessor-out-of-range
  :buffer-view-out-of-range :incompatible-skeleton :unsupported-version
  :part`."
  ([kind msg] (error-info kind msg {}))
  ([kind msg data]
   (ex-info msg (assoc data :vrm/error kind))))
