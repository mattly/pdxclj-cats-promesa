(ns preso
  (:require [clojure.string :as str]))
;; Getting Declarative with Cats and Manifold
;; by Matthew Lyon aka @mattly
;; https://github.com/mattly/pdxclj-cats-promesa

;; Lisp - Code is Data, Data is Code
;;      One data type, the S-Expression
;; Haskell - Code is Computation, Computation is Code
;;      Turn code into mathematics, offers ability to reason through type guarantees

;; Category Theory - study of patterns and how things relate to each other
;;      "the study of abstract bull****" -- some guy on Hacker News
;;      But, lots of useful tools for thinking about composition
;; Great tutorial series:
;; https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/

;; Cats - Category Theory abstraction library for clojure
;; https://github.com/funcool/cats
;; Promesa - Unified clj/cljs interface for non-streaming async computation
;; https://github.com/funcool/promesa
;; lots of other good libraries, under active development

;; The what isn't important unless you understand the *how*
;; how can this help you write cleaner code?

;; A problem: dividing numbers from user input
(/ (Float. "5") (Float. "10"))
(/ (Float. "5") (Float. ""))
(/ (Float. "5") (Float. "foo"))
(/ (Float. "5") (Float. "0"))

(try (/ (Float. "5") (Float. "foo"))
     (catch Exception e nil))

;; perhaps safer way: build up primitive functions
(def is-number-re #"^[\d\.]+$")

(defn ->float [input]
  (when (re-find is-number-re input)
    (Float. input)))

(defn guarded-div [x y]
  (when (and x y (not (zero? y)))
    (/ x y)))

(guarded-div (->float "5") (->float "10"))
(guarded-div (->float "5") (->float ""))
(guarded-div (->float "5") (->float "foo"))
(guarded-div (->float "5") (->float "0"))

;; you'll need to wrap all math functions, though
(try (-> (->float "2")
         (guarded-div  (->float "0"))
         (+ (->float "5")))
     (catch Exception e e))

(let [numer (->float "5")
      divisor (->float "0")
      other (->float "3")]
  (when (and (every? number? [numer divisor other])
             (not (zero? divisor)))
    (-> numer
        (guarded-div divisor)
        (+ other))))

;; Maybe, and composing computation
;; You're probably heard of Maybe,
;;  it's what happens in a strongly-typed system that can't have nil values
;;  because "nil value" is an oxymoron

;; yes, this is unweildy at first, but bear with me

(require '[cats.core :as m]
         '[cats.monad.maybe :refer [just nothing] :as maybe])

(defn mfloat [input]
  (if (re-find is-number-re input)
    (just (Float. input))
    (nothing)))

(mfloat "5")
(mfloat "foo")

;; ignore the fapply, curry, and filter parts for now
(defn mdiv [x y]
  (m/fapply (just (m/curry 2 /))
            x
            (m/filter (comp not zero?) y)))

(mdiv (mfloat "10") (mfloat "5"))
(mdiv (mfloat "10") (mfloat ""))
(mdiv (mfloat "10") (mfloat "foo"))
(mdiv (mfloat "10") (mfloat "0"))

;; ok cool but what if we want a more semantic error instead of 'nil' ?

;; Either: Left & Right
;; Left is a failure, Right is a Success
(require '[cats.monad.either :as either :refer [left right]])
(defn efloat [input]
  (if (re-find is-number-re input)
    (right (Float. input))
    (left [:not-number input])))

(efloat "3")
(efloat "foo")
(def eten (efloat "10"))

;; alternately,
(either/try-either (Float. "3"))
(let [result (either/try-either (Float. "foo"))]
  [(either/left? result) (-> result m/extract .getMessage)])

;; not all numbers are divisors -- so let's make the validity of
;; a divisor its own concern
(defn edivisor [num]
  (if (zero? num)
    (left :zero-divisor)
    (right num)))

(m/bind (efloat "2") edivisor)
(m/bind (efloat "0") edivisor)
(m/bind (efloat "foo") edivisor)

;; let's "apply" those to /
(m/ap / eten (m/bind (efloat "2") edivisor))
(m/ap / eten (m/bind (efloat "0") edivisor))
;; it's a variadic apply, as well
(m/ap / eten (m/bind (efloat "2") edivisor) (m/bind (efloat "2") edivisor))
(m/ap / eten (m/bind (efloat "0") edivisor) (m/bind (efloat "2") edivisor))

(defn ediv1 [x y]
  (m/mlet [numer (efloat x)
           divis (efloat y)
           divis (edivisor divis)]
          (m/return (/ numer divis))))
(ediv1 "10" "2")
(ediv1 "10" "0")

(defn ediv2 [x & ys]
  (m/mlet [numer (efloat x)
           divis (m/mapseq efloat ys)
           divis (m/mapseq edivisor divis)]
          (m/return (apply / numer divis))))
(ediv2 "10" "2" "2")
(ediv2 "10" "2" "foo")
(ediv2 "10" "2" "0")

;; a toolbox -
;; many small functions that operate on contexts

;; === fmap ==================
;; applies a wrapped value to a "pure" function, preserving context
(m/fmap inc eten)
(m/fmap inc (efloat "foo"))
(def minc (m/fmap inc))
(minc eten)
(minc (efloat "foo"))
(minc (nothing))
;; immediate value - use with builtin / nil!
(require '[cats.builtin])
(minc nil)
(minc [1 2])
;; can't do this though
(minc 3)
;; same basic idea as test-check:
(require '[clojure.test.check.generators :as gen])
(def gen-square (gen/fmap #(* % %) gen/int))
(gen/sample gen-square)

;; === bind ==================
;; applies the unwrapped value to a function which returns a (hopefully) wrapped value
(m/bind (just 3) inc)
(m/bind (nothing) inc)
(m/bind (just 3) (comp just inc))
(m/bind (nothing) (comp just inc))
(m/bind (efloat "3") (comp right inc))
(m/bind (efloat "foo") (comp right inc))
(m/bind (right 3) (comp just inc))
(m/bind nil (comp just inc))
;; again, similar to test.check:
(def gen-square2 (gen/bind gen/int #(gen/return (* % %))))
(gen/sample gen-square2)

;; what if you don't know the context?
(def ret-inc (comp m/return inc))
(m/bind (just 3) ret-inc)
(m/bind (right 3) ret-inc)
;; only works inside a context-provider such as bind:
(try (ret-inc 3)
     (catch Exception e (.getMessage e)))

;; === mlet ==================
;; syntax sugar over bind that works similar to clojure's let
;; if any "assignment" computation fails, short-circuits with the failure
;; analagous to Haskell's "do" notation

(m/mlet [x (efloat "3")
         y (efloat "1")]
        (m/return (+ x y)))
;; de-sugars to effectively:
(m/bind (efloat "3")
        (fn [x]
          (m/bind (efloat "1")
                  (fn [y]
                    (m/return (+ x y))))))

;; You can't mix and match:
(try (m/mlet [x (just 1)
              y (right 2)]
             (m/return (+ x y)))
     (catch java.lang.AssertionError e (.getMessage e)))

;; you can have non-monadic lets:
(m/mlet [x (efloat "3")
         :let [y 1]]
        (m/return (+ x y)))
(m/mlet [x (nothing)
         :let [y (throw (ex-data "Won't get here" {}))]]
        (m/return (+ x y)))

;; and have non-monadic guard clauses / filters
;; though with either you lose the power of values
(m/mlet [x (efloat "3")
         y (efloat "0")
         :when (not (zero? y))]
        (/ x y))


;; === fapply ==================
;; takes a *wrapped*, *fixed-arity* function and executes it
;; with unwrapped arguments, provided all arguments are success.
;; Also known as <*>
(m/fapply (just inc) (just 2))
(m/fapply (just inc) (nothing))
(m/fapply (just (m/curry 2 /)) (just 10) (just 2))
(m/fapply (right inc) (right 1))
(m/fapply (right (m/curry 2 /)) (right 10) (right 2))
(m/fapply (just inc) nil)

;; === curry ==================
;; provides automatic currying for when a function receives less than the
;; expected number of arguments
(def inc-list ((m/curry 2 map) inc))
(inc-list [1 2 3])

;; === lift-m ==================
;; returns a function that works similar to fapply, but keeping context
(def ldiv (m/lift-m 2 /))
(ldiv (mfloat "10") (mfloat "5"))
(ldiv (efloat "10") (efloat "5"))
(ldiv (efloat "10") (efloat "foo"))
(ldiv (efloat "10") (m/bind (efloat "0") edivisor))

;; === mapseq ==================
;; Applies a context-returning function to a sequence of values,
;; if they're all successes, "raises" the context to the whole list

(m/mapseq efloat ["1" "2" "3"])
(m/mapseq efloat ["1" "foo" "3"])

;; many more, not worth getting into yet

;; towards idiomatic clojure
(def ladd (m/lift-m 2 +))

;; threading
(defn some-mcalc [numer divis add-to]
  (-> (efloat numer)
      (ldiv (-> divis efloat (m/bind edivisor)))
      (ladd (efloat add-to))))

(some-mcalc "5" "10" "2")
(some-mcalc "5" "0" "foo")
(some-mcalc "Foo" "Bar" "Baz")

;; === Promesa ======
;; A promise library for Clojure/ClojureScript
;; provides a unified interface to CompleteableFuture and Bluebird
(require '[promesa.core :as p])

(p/promise 3)
@(p/promise 3) ;; can't deref in cljs

(def q (atom nil))
(-> (p/promise 3)
    (p/then #(reset! q %))) ;; example usage in cljs
(deref q)

(p/resolved 3)
(p/rejected (ex-info "Oops" {}))

(-> (p/promise 3)
    (p/then inc)
    deref)
(-> (p/promise (ex-info "oops" {}))
    (p/catch (fn [err] (.getMessage err)))
    deref)

(defn p-odd? [n]
  (-> (p/promise
       (fn [resolve reject]
         (Thread/sleep 1000)
         (if (odd? n)
           (resolve n)
           (reject "Not Odd!" {:value n}))))
      (p/then inc)
      (p/catch (fn [e] nil))
      (p/then (fn [n] (reset! q n))))
  :done)

(deref q)
(p-odd? 3)
(p-odd? 4)

(do (reset! q nil)
    (-> (p/delay 1000 10)
        (p/then inc)
        (p/then (partial reset! q)))
    (deref q))(defn p-div [x y]
  (p/promise (fn [resolve reject]
               (resolve (/ (Float. x) (Float. y))))))
(-> (p-div "10" "2") deref)
(-> (p-div "10" "foo") (p/catch (constantly nil)))
(-> (p-div "10" "0") (p/catch (constantly nil)))

(require '[cats.labs.promise])
(defn pfloat [x]
  (p/promise (fn [res _]
               (res (Float. x)))))

(do (reset! q nil)
    (m/mlet [x (pfloat "foo")
             y (do (reset! q :nope) (pfloat "3"))]
            (+ x y))
    (deref q))


;; =========
;; Solving more complex problems
;; =========

;; hooking business logic up to external services

;; simulate pulling from an unreliable external source to satisfy a request
;; these are entirely arbitrary - but reflect an actual flaky API I've used
(defn get-item [id]
  (Thread/sleep 25)
  (case id
    :one {:id :one}
    :two {:id :two}
    :three (throw (Exception. "oops"))
    :four :forbidden
    nil))

(defn get-subitems [item-id]
  (Thread/sleep 100)
  (case item-id
    :one [1 2 3 4]
    :three [1 2]
    nil))

;; workaround for that cider isn't showing me stdout anymore
(defmacro time-and [& body]
  `(let [start-time# (System/currentTimeMillis)
         result# ~@body]
    [(- (System/currentTimeMillis) start-time#) result#]))

(time-and (get-item :one))
(time-and (get-item :two))
(time-and (get-item :three))

(defn display-item [id]
  (let [item? (try (get-item id)
                   (catch Exception e nil))]
    (when (map? item?)
      {:item item? :subitems (or (get-subitems id) [])})))


(time-and (display-item :one))
(time-and (display-item :four))
(time-and (mapv display-item [:one :two :four]))
(time-and (mapv display-item [:one :three :five]))

(defn m-get-item [id]
  (m/mlet [res (either/try-either (get-item id))]
          (cond (map? res) (right res)
                (nil? res) (left [:not-found])
                :else (left [res]))))

(m-get-item :one)
(either/branch (m-get-item :three)
               (fn [err] (.getMessage err))
               (fn [res] :wont-get-here))
(m-get-item :four)
(m-get-item :five)

(defn m-display-item [id]
  (m/mlet [item (m-get-item id)]
          (m/return {:item item :subitem (get-subitems id)})))

(time-and (m-display-item :one))
(time-and (m-display-item :three))
(time-and (m-display-item :four))
(time-and (mapv m-display-item [:one :two]))
(time-and (mapv m-display-item [:one :two :three :four :five]))


(defn p-get-item [id]
  (-> (p/promise (fn [resolve reject]
                   (let [res (get-item id)]
                     (cond (map? res) (resolve res)
                           (nil? res) (reject (ex-info "Not Found" {:id id}))
                           :else (reject (ex-info "Oops" {:id id :reason res}))))))
      (p/then (fn [item]
                {:item item :subitems (or (get-subitems id) [])}))))

(time-and (deref (p-get-item :one)))
(time-and (deref (p-get-item :three)))
(time-and (deref (p-get-item :four)))

(time-and (deref (m/mapseq p-get-item [:one :two])))
(time-and (p/extract (m/mapseq p-get-item [:one :two :four])))

(defn page-info []
  (m/mlet [one (p-get-item :one)
           two (p-get-item :two)]
          (m/return {:items [one two]})))

(time-and (deref (page-info)))

;; === alet
(defn page-info2 []
  (m/alet [one (p-get-item :one)
           two (p-get-item :two)]
          {:items [one two]}))

(time-and (deref (page-info2)))

(defn wait-int [x] (p/delay x x))

(time-and
 (deref
  (m/alet [a (wait-int 100)
           b (wait-int 50)
           c (wait-int (+ b 100))
           d (wait-int (+ a b))
           z (wait-int 50)]
          (+ a b c d z))))
