(ns vrm.glb
  "GLB binary container parse/write (zero-copy on read where the host platform
  allows it). Restored from `kami-vrm/src/glb.rs` (kotoba-lang/kami-engine,
  deleted PR #82 'Remove Rust workspace from kami-engine') as part of the
  clj-wgsl migration (ADR-2607010930, com-junkawasaki/root).

  Bytes are represented as a portable byte-int sequence (`(vec bytes)`, each
  element 0-255) so this namespace stays platform-neutral; callers on the JVM
  can `byte-array` the result, cljs callers can `js/Uint8Array.` it.")

;; GLB magic: "glTF" in little-endian.
(def glb-magic 0x46546C67)
;; GLB version 2.
(def glb-version 2)
;; JSON chunk type.
(def chunk-json 0x4E4F534A)
;; BIN chunk type.
(def chunk-bin 0x004E4942)

(defn- u32-at
  "Read a little-endian u32 from byte-int vector `data` at offset `off`."
  [data off]
  (bit-or (nth data off)
          (bit-shift-left (nth data (+ off 1)) 8)
          (bit-shift-left (nth data (+ off 2)) 16)
          (bit-shift-left (nth data (+ off 3)) 24)))

(defn u32->le-bytes
  "u32 -> 4 little-endian byte ints (0-255), matching Rust's `u32::to_le_bytes`."
  [x]
  (let [x (bit-and (long x) 0xFFFFFFFF)]
    [(bit-and x 0xFF)
     (bit-and (bit-shift-right x 8) 0xFF)
     (bit-and (bit-shift-right x 16) 0xFF)
     (bit-and (bit-shift-right x 24) 0xFF)]))

(defn string->byte-seq
  "UTF-8 encode a string to a vector of byte ints (0-255)."
  [s]
  #?(:clj (mapv #(bit-and (int %) 0xFF) (.getBytes ^String s "UTF-8"))
     :cljs (vec (.encode (js/TextEncoder.) s))))

(defn byte-seq->string
  "Decode a vector (or seq) of byte ints (0-255) as UTF-8."
  [bytes]
  #?(:clj (String. (byte-array (map unchecked-byte bytes)) "UTF-8")
     :cljs (.decode (js/TextDecoder.) (js/Uint8Array. (clj->js (vec bytes))))))

(defn pad-len
  "Number of padding bytes needed to round `n` up to a 4-byte boundary."
  [n]
  (mod (- 4 (mod n 4)) 4))

;; ---------------------------------------------------------------------------
;; Parse
;; ---------------------------------------------------------------------------

(defn parse-glb
  "Parse raw GLB bytes (byte-int vector) into `{:json [byte-ints] :bin (nilable
  [byte-ints])}`. Throws `ex-info` on malformed input, mirroring `VrmError::InvalidGlb`."
  [data]
  (let [data (vec data)
        n (count data)]
    (when (< n 12) (throw (ex-info "invalid GLB: too short for GLB header" {})))
    (let [magic (u32-at data 0)]
      (when (not= magic glb-magic) (throw (ex-info "invalid GLB: invalid magic" {}))))
    (let [version (u32-at data 4)]
      (when (not= version glb-version) (throw (ex-info "invalid GLB: unsupported GLB version" {}))))
    (let [total-len (u32-at data 8)]
      (when (< n total-len) (throw (ex-info "invalid GLB: data shorter than declared length" {}))))
    (when (< n 20) (throw (ex-info "invalid GLB: too short for JSON chunk header" {})))
    (let [json-len (u32-at data 12)
          json-type (u32-at data 16)]
      (when (not= json-type chunk-json) (throw (ex-info "invalid GLB: first chunk is not JSON" {})))
      (let [json-end (+ 20 json-len)]
        (when (< n json-end) (throw (ex-info "invalid GLB: JSON chunk truncated" {})))
        (let [json (subvec data 20 json-end)
              bin (when (>= n (+ json-end 8))
                    (let [bin-len (u32-at data json-end)
                          bin-type (u32-at data (+ json-end 4))]
                      (when (= bin-type chunk-bin)
                        (let [bin-start (+ json-end 8)
                              bin-end (+ bin-start bin-len)]
                          (when (>= n bin-end) (subvec data bin-start bin-end))))))]
          {:json json :bin bin})))))

;; ---------------------------------------------------------------------------
;; Write
;; ---------------------------------------------------------------------------

(defn write-glb
  "Write GLB bytes (byte-int vector) from JSON bytes + binary buffer (both
  byte-int vectors)."
  [json bin]
  (let [json (vec json)
        bin (vec bin)
        json-pad (pad-len (count json))
        json-chunk-len (+ (count json) json-pad)
        bin-pad (pad-len (count bin))
        bin-chunk-len (+ (count bin) bin-pad)
        total-len (+ 12 8 json-chunk-len 8 bin-chunk-len)]
    (vec (concat
          (u32->le-bytes glb-magic)
          (u32->le-bytes glb-version)
          (u32->le-bytes total-len)
          (u32->le-bytes json-chunk-len)
          (u32->le-bytes chunk-json)
          json
          (repeat json-pad (int \space))
          (u32->le-bytes bin-chunk-len)
          (u32->le-bytes chunk-bin)
          bin
          (repeat bin-pad 0)))))
