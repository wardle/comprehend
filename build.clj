(ns build
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'com.eldrix/comprehend)
(def version (format "0.0.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-lib-%s.jar" (name lib) version))

(defn clean [_]
      (b/delete {:path "target"}))

(defn jar
      "Create a library jar file."
      [_]
      (println "Building" lib version)
      (b/write-pom {:class-dir class-dir
                    :lib       lib
                    :version   version
                    :basis     basis
                    :src-dirs  ["src"]
                    :scm       {:url                 "https://github.com/wardle/concierge"
                                :tag                 (str "v" version)
                                :connection          "scm:git:git://github.com/wardle/concierge.git"
                                :developerConnection "scm:git:ssh://git@github.com/wardle/concierge.git"}})
      (b/copy-dir {:src-dirs   ["src" "resources"]
                   :target-dir class-dir})
      (b/jar {:class-dir class-dir
              :jar-file  jar-file}))

(defn install
      "Install library to local maven repository."
      [_]
      (clean nil)
      (jar nil)
      (println "Installing" lib version)
      (b/install {:basis     basis
                  :lib       lib
                  :version   version
                  :jar-file  jar-file
                  :class-dir class-dir}))

(defn deploy
      "Deploy library to clojars.
      Environment variables CLOJARS_USERNAME and CLOJARS_PASSWORD must be set."
      [_]
      (clean nil)
      (jar nil)
      (dd/deploy {:installer :remote
                  :artifact  jar-file
                  :pom-file  (b/pom-path {:lib       lib
                                          :class-dir class-dir})}))