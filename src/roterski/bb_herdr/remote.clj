(ns roterski.bb-herdr.remote
  (:require [roterski.bb-herdr.herdr :as h]
            [babashka.process :as bp]
            [clojure.string :as str]))

(def herdr-env
  "Extra env for every herdr CLI call, e.g.
   {\"HERDR_SOCKET_PATH\" \"/tmp/herdr-remote.sock\"} to target a
   remote server through a forwarded socket (see `remote-connect!`)."
  (atom {}))

(defn- remote-home
  [ssh-target port]
  (-> (bp/sh ["ssh" "-o" "BatchMode=yes" "-p" (str port)
              ssh-target "printf %s \"$HOME\""])
      :out
      str/trim))

(defn remote-connect!
  "Forward a remote herdr socket to a local path over ssh and point
   all subsequent `herdr` calls at it. `:remote-sock` must be an
   absolute path; when omitted it is resolved to
   <remote $HOME>/.config/herdr/herdr.sock. Returns the tunnel process;
   call `remote-disconnect!` with it (or just `(reset! herdr-env {})`)
   to go back to the local server."
  [{:keys [ssh-target port remote-sock local-sock]
    :or {port 22
         local-sock "/tmp/herdr-remote.sock"}}]
  (let [remote-sock (or remote-sock
                        (str (remote-home ssh-target port)
                             "/.config/herdr/herdr.sock"))
        proc (bp/process ["ssh" "-N"
                          "-o" "StreamLocalBindUnlink=yes"
                          "-o" "ExitOnForwardFailure=yes"
                          "-o" "ServerAliveInterval=30"
                          "-p" (str port)
                          "-L" (str local-sock ":" remote-sock)
                          ssh-target])]
    (reset! herdr-env {"HERDR_SOCKET_PATH" local-sock})
    proc))

(defn remote-disconnect!
  [tunnel-proc]
  (reset! herdr-env {})
  (bp/destroy-tree tunnel-proc))
