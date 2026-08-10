(ns roterski.bb-herdr.herdr
  (:require [babashka.process :as bp]
            [jsonista.core :as j]
            [clojure.string :as str]))

(def ^:dynamic *props* {:print-cmd? true})

(defn herdr
  [& cmd]
  (let [cmd (str "herdr " (str/join " " cmd))
        {:keys [extra-env print-cmd? print-output?]} *props*
        {:keys [exit out err]} (bp/sh {:extra-env extra-env} cmd)
        success? (zero? exit)
        output (if success?
                 out
                 err)
        parsed-output (merge {:success? success?}
                             (try
                               (j/read-value output j/keyword-keys-object-mapper)
                               (catch Exception _e
                                 (println out err)
                                 {:result output})))]
    (when print-cmd? (println "$" cmd))
    (when print-output? (println ">" parsed-output))
    parsed-output))

(defn dir->pane-id
  {:malli/schema [:=>
                  [:cat [:map [:dir :string]]]
                  [:maybe :string]]}
  [{:keys [dir]}]
  (->> (herdr "pane list")
       :result
       :panes
       (filter (fn [{:keys [cwd]}]
                 (= cwd dir)))
       first
       :pane_id))

(defn ->workspace-label
  {:malli/schema [:=>
                  [:cat [:map
                         [:label {:optional true} :string]
                         [:workspace-label {:optional true} :string]]]
                  :string]}
  [{:keys [label workspace-label]}]
  (or workspace-label label))

(defn ->tab-label
  {:malli/schema [:=>
                  [:cat [:map
                         [:label {:optional true} :string]
                         [:agent-name {:optional true} :string]
                         [:tab-label {:optional true} :string]]]
                  :string]}
  [{:keys [agent-name label tab-label]}]
  (or tab-label agent-name label))

(defn label->workspace-id
  [props]
  (->> (herdr "workspace list")
       :result
       :workspaces
       (filter #(= (->workspace-label props) (:label %)))
       first
       :workspace_id))

(defn ->workspace-id!
  {:malli/schema [:=>
                  [:cat [:map [:dir :string]]]
                  :string]}
  [{:keys [dir] :as props}]
  (if-let [workspace-id (label->workspace-id props)]
    workspace-id
    (do (herdr "workspace create"
               "--cwd" dir
               "--label" (->workspace-label props))
        (label->workspace-id props))))

(defn tab->pane-id
  [props]
  (let [workspace-id (->workspace-id! props)
        tab-id (->> (herdr "tab list --workspace" workspace-id)
                    :result :tabs
                    (filter #(= (->tab-label props) (:label %)))
                    first
                    :tab_id)
        pane-id (->> (herdr "pane list --workspace" workspace-id)
                     :result :panes
                     (filter #(= tab-id (:tab_id %)))
                     first
                     :pane_id)]
    pane-id))

(defn tab->pane-id!
  {:malli/schema [:=>
                  [:cat [:map [:dir :string]]]
                  :string]}
  [{:keys [dir] :as props}]
  (if-let [pane-id (tab->pane-id props)]
    pane-id
    (do (herdr "tab create"
               "--workspace" (->workspace-id! props)
               "--cwd" dir
               "--label" (->tab-label props)
               "--focus")
        (Thread/sleep 1000) ;; must wait for newly created panes to become responsive , TODO try to avoid blocking thread
        (tab->pane-id props))))

(defn agent-name->pane-id
  {:malli/schema [:=>
                  [:cat [:map [:agent-name :string]]]
                  [:maybe :string]]}
  [{:keys [agent-name]}]
  (->> (herdr "agent" "get" agent-name)
       :result
       :agent
       :pane_id))

(defn agent-name->pane-id!
  {:malli/schema [:=>
                  [:cat [:map
                         [:agent-name :string]
                         [:permission-mode [:enum "auto" "acceptEdits" "bypassPermissions" "manual" "dontAsk" "plan"]]]]
                  :string]}
  [{:keys [agent-name permission-mode] :as props}]
  (if-let [pane-id (agent-name->pane-id props)]
    pane-id
    (let [pane-id (tab->pane-id! props)]
      (herdr "agent start" agent-name
             "--pane" pane-id
             "--kind" "claude"
             "--"
             "--permission-mode" permission-mode)
      (agent-name->pane-id props))))

(defn close-all
  []
  (->> (herdr "workspace list")
       :result
       :workspaces
       (run! (fn [{:keys [workspace_id]}]
               (herdr "workspace close" workspace_id)))))

(defn agent-run!
  {:malli/schema [:=>
                  [:cat [:map [:agent-name :string]] :string]
                  [:map]]}
  [{:keys [agent-name] :as props} prompt]
  (agent-name->pane-id! props)
  (herdr "agent prompt" agent-name
         (str "'" prompt "'")))
