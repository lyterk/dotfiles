#!/user/bin/env bb
(ns remote-copy
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn home "/home/lyterk")

(def files
  {:atuin [{:from [home "hosting/atuin-docker-compose.yaml"]
            :to [home ".local/share/atuin/atuin-docker-compose.yaml"]}]})

(def dotfiles-dir "/home/lyterk/dotfiles")

(def)
