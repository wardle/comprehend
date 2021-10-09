(ns com.eldrix.comprehend.core
  (:require [cognitect.aws.client.api :as aws]
            [cognitect.aws.credentials :as credentials]
            [com.eldrix.hermes.core :as hermes]
            [com.wsscode.pathom3.connect.operation :as pco]))

(deftype Svc [hermes comprehend])

(defn ^:private make-constraint [{:keys [Category Traits Type]}]
  (cond
    (= "MEDICAL_CONDITION" Category)
    "<404684003"
    (= "MEDICATION" Category)
    "<373873005"
    (= Category "TEST_TREATMENT_PROCEDURE")
    "<71388002"
    (= Category "ANATOMY")
    "<123037004"))

(defn ^:private entity->snomed [hermes {:keys [Text] :as entity}]
  (let [ecl (make-constraint entity)]
    (vector entity (hermes/search hermes {:s Text :constraint ecl :max-hits 1 :fallback-fuzzy 2}))))

(defn simple-parse
  "Parse free text for clinical conditions and medications."
  [^Svc svc s]
  (->> (aws/invoke (.-comprehend svc) {:op      :DetectEntitiesV2
                                       :request {:Text s}})
       :Entities
       (filter #(#{"MEDICATION" "MEDICAL_CONDITION"} (:Category %))) ;; only keep medications and conditions
       (remove #((set (map :Name (:Traits %))) "NEGATION")) ;; remove any with trait NEGATION
       (map #(entity->snomed (.-hermes svc) %))))


(defn open
  "Open a comprehend service using the configuration specified.
  Parameters:
  - config - configuration including keys:
           |- :hermes - a hermes terminology service
           |- :access-key-id - access key for basic credentials
           |- :secret-access-key - secret access key."
  [{hermes            :com.eldrix/hermes
    access-key-id     :access-key-id
    secret-access-key :secret-access-key :as config}]
  (->Svc hermes
         (aws/client (cond-> (merge config {:api :comprehendmedical})
                             (and access-key-id secret-access-key)
                             (assoc :credentials-provider (credentials/basic-credentials-provider {:access-key-id     access-key-id
                                                                                                   :secret-access-key secret-access-key}))))))

(pco/defmutation parse
  [{::keys [svc]} {s :s}]
  {::pco/op-name 'info.snomed/parse
   ::pco/params  [:s]
   ::pco/output  [:info.snomed.Description/id
                  :info.snomed.Concept/id
                  :info.snomed.Description/term
                  {:info.snomed.Concept/preferredDescription [:info.snomed.Description/term]}]}
  (->> (simple-parse svc s)
       (filter identity)
       (map (fn [result] {:info.snomed.Description/id               (:id result)
                          :info.snomed.Concept/id                   (:conceptId result)
                          :info.snomed.Description/term             (:term result)
                          :info.snomed.Concept/preferredDescription {:info.snomed.Description/term (:preferredTerm result)}}))))


(comment
  (def comprehend (aws/client {:api :comprehendmedical}))
  (def comprehend (aws/client {:api                  :comprehendmedical
                               :credentials-provider (credentials/basic-credentials-provider
                                                       {:access-key-id     "access-key"
                                                        :secret-access-key "secret-key"})}))
  (def svc (open {:com.eldrix/hermes hermes
                  :access-key-id     "access-key"
                  :secret-access-key "secret-key"}))
  svc

  (keys (aws/ops comprehend))
  (aws/validate-requests comprehend))