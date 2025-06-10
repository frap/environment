#!/usr/bin/env bb

(ns bb-fzf
  (:require
   [babashka.process :as p]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [babashka.deps :as deps]
   [clojure.java.io :as io]))

(deps/add-deps '{:deps {io.github.paintparty/bling {:mvn/version "0.5.0"}}})

(require '[bling.core :refer [bling callout]])

(defn find-git-root []
  (try
    (let [{:keys [exit out]} (p/shell {:out :string
                                       :err :capture}
                                      "git rev-parse --show-toplevel")]
      (when (zero? exit)
        (str/trim out)))
    (catch Exception _e
      ;; Return nil if git command fails (e.g., not a repo, git not installed)
      nil)))

(defn list-tasks
  "List public task symbols from a tasks map."
  [tasks-map]
  (when tasks-map
    (filter (every-pred (fn public? [s] (not= "-" (subs (str s) 0 1)))
                        symbol?)
            (keys tasks-map))))

(defn task-requires-arg?
  "Checks if the task definition string includes *command-line-args*."
  ;; TODO: Add support for multiple args and optional better handling with
  ;;       babashka/cli.
  [tasks-map task-name]
  (when tasks-map
    (let [task-def (str (get tasks-map (symbol task-name)))]
      (str/includes? task-def "*command-line-args*"))))

(defn get-task-arg [task-name]
  (println (str "Enter argument for task " task-name ":"))
  (str/trim (read-line)))

(defn select-task
  [tasks-str]
  (try
    (-> (p/process ["fzf"] {:in tasks-str :out :string})
        (p/check)
        :out
        (str/trim))
    (catch clojure.lang.ExceptionInfo ex
      (if (str/blank? (:out (:data ex)))
        ;; in case user cancels (presses esc) in the fzf ui
        ""
        (throw ex)))))

(defn run-task-and-display-result
  "Executes a bb task command string using p/shell in a specific directory and displays the result."
  [cmd-str work-dir]
  (let [{:keys [out exit err] :as _result} (p/shell {:out :string
                                                     :err :string
                                                     :continue true
                                                     :dir work-dir}
                                                    cmd-str)
        label (case exit
                0 "Success"
                1 "Error"
                "Warning") ;; Use Warning for non-0/1 exit codes
        label-type (case exit
                     0 :positive
                     1 :error
                     :warning)
        label-content (str/trim (case exit
                                  0 out
                                  err))]
    ;; Only display callout if there's content or it's an error
    (when (or (not (str/blank? label-content)) (= label-type :error))
      (callout {:label label
                :type label-type}
               label-content))))

(defn main []
  (let [work-dir (or (find-git-root) ".") ; Use git root or current dir
        bb-edn-path (str work-dir "/bb.edn")
        bb-edn-file (io/file bb-edn-path)]
    (if-not (.exists bb-edn-file)
      (println (bling [:error "Error:"]) (str "bb.edn not found at " bb-edn-path))
      (let [tasks-map (try
                        (-> (slurp bb-edn-path)
                            (edn/read-string)
                            :tasks)
                        (catch Exception e
                          (println (bling [:error "Error reading or parsing bb.edn:"]) (.getMessage e))
                          nil))
            tasks (when tasks-map (list-tasks tasks-map))
            tasks-str (when tasks (str/join "\n" tasks))]
        (when (and tasks-map tasks (not (str/blank? tasks-str)))
          (let [selected-task (select-task tasks-str)]
            (when-not (empty? selected-task)
              (println (bling [:blue.bold "Running task:"])
                       (bling [:bold selected-task])
                       (bling [:grey (str "(in " work-dir ")")]))
              (if (task-requires-arg? tasks-map selected-task)
                (let [task-arg (get-task-arg selected-task)
                      ;; Construct the command string, handling empty input for the arg
                      cmd-str (if (empty? task-arg)
                                (str "bb " selected-task)
                                (str "bb " selected-task " " task-arg))]
                  (run-task-and-display-result cmd-str work-dir)) ; Pass work-dir
                ;; Task does not require an argument
                (run-task-and-display-result (str "bb " selected-task) work-dir))))))))) ; Pass work-dir

(main)
