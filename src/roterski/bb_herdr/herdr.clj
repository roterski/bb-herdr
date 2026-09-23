(ns roterski.bb-herdr.herdr
  (:require [roterski.bb-herdr.utils :refer [join-lines]]
            [babashka.process :as bp]
            [jsonista.core :as j]
            [clojure.string :as str]
            [malli.core :as ma]
            [malli.transform :as mt]))

(def ^:dynamic *props* {:print-cmd? true})

(defn wrapped-sh
  "Runs `argv` (a vector, passed to the process as-is) and parses its JSON output."
  [props argv]
  (let [{:keys [exit out err]} (apply bp/sh props argv)
        success? (zero? exit)
        output (if success?
                 out
                 err)]
    (merge {:success? success?}
           (try
             (j/read-value output j/keyword-keys-object-mapper)
             (catch Exception _e
               (println out err)
               {:result output})))))

(defn cmds->herdr-args
  "A single string is a command line, tokenized like a shell would (a leading
     `herdr` is optional, so commands paste straight from the docs). Anything
     else is argv: each element is one argument, never joined or tokenized, so
     values may contain spaces and quotes. Nested sequences are spliced, nils dropped."
  [cmds]
  (if (and (= 1 (count cmds)) (string? (first cmds)))
    (let [tokens (bp/tokenize (first cmds))]
      (cond-> tokens (= "herdr" (first tokens)) rest))
    (->> cmds flatten (remove nil?))))

^:rct/test
(comment
  (cmds->herdr-args ["ls -lha path"])
  ;;=> ["ls" "-lha" "path"]
  (cmds->herdr-args ["ls" "-lha" "path"])
  ;;=> ("ls" "-lha" "path")
  )

(defn herdr
  "Runs herdr with the args described in `cmds->herdr-args`. An optional
   leading map holds babashka.process opts plus :print-cmd?/:print-output?."
  [& args]
  (let [[props cmds] (if (map? (first args))
                       [(first args) (rest args)]
                       [{} args])
        argv (into ["herdr"] (cmds->herdr-args cmds))
        {:keys [print-cmd? print-output?]} (merge *props* props)
        parsed-output (wrapped-sh props argv)]
    (when print-cmd? (println "$" (str/join " " argv)))
    (when print-output? (println ">" parsed-output))
    parsed-output))

(defn dir->pane-id
  {:malli/schema [:=>
                  [:cat [:map [:dir :string]]]
                  [:maybe :string]]}
  [{:keys [dir]}]
  (->> (herdr "pane" "list")
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
  (->> (herdr "workspace" "list")
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
    (do (herdr "workspace" "create"
               "--cwd" dir
               "--label" (->workspace-label props))
        (label->workspace-id props))))

(defn tab->pane-id
  [props]
  (let [workspace-id (->workspace-id! props)
        tab-id (->> (herdr "tab" "list" "--workspace" workspace-id)
                    :result :tabs
                    (filter #(= (->tab-label props) (:label %)))
                    first
                    :tab_id)
        pane-id (->> (herdr "pane" "list" "--workspace" workspace-id)
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
    (do (herdr "tab" "create"
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

(def AgentProps
  [:map
   [:agent-name :string]
   [:agent-kind {:optional true
                 :default "claude"} :string]
   ;; a string is tokenized like a shell command line;
   ;; a vector is passed as argv elements, untouched
   [:agent-opts {:optional true} [:or :string [:vector :string]]]])

(def coerce-agent-props
  (ma/coercer AgentProps (mt/transformer
                          mt/string-transformer
                          (mt/default-value-transformer {::mt/add-optional-keys true}))))

(defn agent-name->pane-id!
  {:malli/schema [:=>
                  [:cat AgentProps]
                  :string]}
  [props]
  (let [{:keys [agent-name agent-kind agent-opts] :as props} (coerce-agent-props props)]
    (if-let [pane-id (agent-name->pane-id props)]
      pane-id
      (let [pane-id (tab->pane-id! props)]
        (herdr "agent" "start" agent-name
               "--pane"      pane-id
               "--kind"      agent-kind
               "--"
               (cond-> agent-opts
                 (string? agent-opts) bp/tokenize))
        (agent-name->pane-id props)))))

(defn close-workspace!
  [props]
  (when-let [workspace-id (label->workspace-id props)]
    (herdr "workspace" "close" workspace-id)))

(defn close-all
  []
  (->> (herdr "workspace" "list")
       :result
       :workspaces
       (run! (fn [{:keys [workspace_id]}]
               (herdr "workspace" "close" workspace_id)))))

(defn ->input-prompt
  [text]
  (->> text
       str/split-lines
       (drop-while (fn [line] (not (str/starts-with? line "────────────"))))
       (drop 1)
       (take-while (fn [line] (not (str/starts-with? line "────────────"))))))

(defn ->blank-input-prompt?
  [text]
  (-> (apply join-lines (->input-prompt text))
      (str/replace-first "❯" "")
      str/trim
      str/blank?))

(defn ensure-agent-prompt-sent!
  [agent-name]
  (loop [i 0]
    (let [{text :result} (herdr "agent" "read" agent-name)]
      (when (or (not (->blank-input-prompt? text))
                (< i 10))
        (herdr "agent" "send-keys" agent-name "enter")
        (Thread/sleep 100)
        (recur (inc i))))))

(defn agent-run!
  {:malli/schema [:=>
                  [:cat [:map
                         [:agent-name :string]
                         [:ensure-prompt-sent? {:optional true
                                                :default false} :boolean]] :string]
                  [:map]]}
  [{:keys [agent-name ensure-prompt-sent?] :as props} prompt]
  (agent-name->pane-id! props)
  (herdr "agent" "prompt" agent-name prompt)
  (when ensure-prompt-sent?
    (ensure-agent-prompt-sent! agent-name)))
