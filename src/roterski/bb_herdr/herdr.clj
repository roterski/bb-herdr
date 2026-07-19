(ns roterski.bb-herdr.herdr
  (:require [roterski.bb-herdr.utils :refer [join-lines]]
            [babashka.process :as bp]
            [jsonista.core :as j]
            [clojure.string :as str]))

(defn herdr
  [& cmd]
  (let [cmd (str "herdr " (str/join " " cmd))
        {:keys [exit out err]} (bp/sh cmd)
        success? (zero? exit)
        output (if success?
                 out
                 err)]
    (println "$" cmd)
    (merge {:success? success?}
           (try
             (j/read-value output j/keyword-keys-object-mapper)
             (catch Exception _e
               (println out err)
               {:result output})))))

(defn dir->pane-id
  [dir]
  (->> (herdr "pane list")
       :result
       :panes
       (filter (fn [{:keys [cwd]}]
                 (= cwd dir)))
       first
       :pane_id))

(defn label->workspace-id
  [workspace-label]
  (->> (herdr "workspace list")
       :result
       :workspaces
       (filter #(= workspace-label (:label %)))
       first
       :workspace_id))

(defn label->workspace-id!
  [dir workspace-label]
  (if-let [workspace-id (label->workspace-id workspace-label)]
    workspace-id
    (do (herdr "workspace create"
               "--cwd" dir
               "--label" workspace-label)
        (label->workspace-id workspace-label))))

(defn tab->pane-id
  [workspace-id tab-label]
  (let [tab-id (->> (herdr "tab list --workspace" workspace-id)
                    :result :tabs
                    (filter #(= tab-label (:label %)))
                    first
                    :tab_id)
        pane-id (->> (herdr "pane list --workspace" workspace-id)
                     :result :panes
                     (filter #(= tab-id (:tab_id %)))
                     first
                     :pane_id)]
    pane-id))

(defn tab->pane-id!
  [dir workspace-id tab-label]
  (if-let [pane-id (tab->pane-id workspace-id tab-label)]
    pane-id
    (do (herdr "tab create"
               "--workspace" workspace-id
               "--cwd" dir
               "--label" tab-label)
        (tab->pane-id workspace-id tab-label))))

(defn agent-name->pane-id
  [agent-name]
  (->> (herdr "agent" "get" agent-name)
       :result
       :agent
       :pane_id))

(defn agent-name->pane-id!
  [dir agent-name workspace-id]
  (if-let [pane-id (agent-name->pane-id agent-name)]
    pane-id
    (do (herdr "agent start" agent-name
               "--workspace" workspace-id
               "--cwd" dir
               "--" "claude --permission-mode auto")
        (agent-name->pane-id agent-name))))

(defn close-all
  []
  (->> (herdr "workspace list")
       :result
       :workspaces
       (run! (fn [{:keys [workspace_id]}]
               (herdr "workspace close" workspace_id)))))

(defn ->input-prompt
  [pane-id]
  (->> (herdr "pane" "read" pane-id)
       :result
       str/split-lines
       (drop-while (fn [line] (not (str/starts-with? line "────────────"))))
       (drop 1)
       (take-while (fn [line] (not (str/starts-with? line "────────────"))))))

(defn ->blank-input-prompt?
  [pane-id]
  (-> (apply join-lines (->input-prompt pane-id))
      (str/replace-first "❯" "")
      str/trim
      str/blank?))

(defn ensure-prompt-sent!
  [pane-id]
  (loop [i 0]
    (when (or (not (->blank-input-prompt? pane-id))
              (< i 10))
      (herdr "pane" "send-keys" pane-id "enter")
      (Thread/sleep 100)
      (recur (inc i)))))

(defn pane-run!
  [pane-id text]
  (herdr "pane" "run" pane-id text)
  (ensure-prompt-sent! pane-id))

(defn agent-run!
  [{:keys [dir agent-name label]} prompt]
  (pane-run! (agent-name->pane-id! dir
                                   agent-name
                                   (label->workspace-id! dir label))
             prompt))
