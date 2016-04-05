;;;; Aleksander Eskilson
;;;; EECS 660, Homework 4, Problem 4
(ns hw-4.core)

;; Returns list of job items with an added 'latest possible start time'
(defn get-latest-starts
  [jobs]
  (map (fn [job] (assoc job :start (- (:deadline job) (:duration job)))) jobs))

;; Schedules a job onto a machine, that is, allocates a number of cells equal to the duration
(defn schedule-job
  [machine job]
  (reduce conj machine (replicate (:duration job) (:id job))))

;; Attempts to allocate a job to the machines, if it cannot schedule, allocate a new machine
(defn allocate-job
  [machines job]
  (loop [ret [] allocated? false ms machines]
    (if (seq ms) ; machines exist to still be checked
      ; does latest start date for job fall over an already allocated day on machine?
      (if (and (not allocated?) (not (get (first ms) (:start job))))        
        (recur (conj ret (schedule-job (first ms) job)) true (rest ms)) ; job can fit on machine
        (recur (conj ret (first ms)) allocated? (rest ms)))             ; job cannot fit on machine
      (if (not allocated?) ; we've checked all provisioned machines
        (conj ret (schedule-job [] job))  ; job was never allocated to any machine
        ret))))                           ; job was successfully allocated to some machine

;; Determines how a collection of jobs should be allocated to minimize required machines
(defn greedy-machines
  [jobs]
  (loop [Q (sort-by :start (get-latest-starts jobs))  ; Q, a selection set sorted by earliest start
         M (vector [])]                               ; M, a vector of machines
    (if-let [job (first Q)] ; selects job with earliest required start, if one exists
      (recur (rest Q)
             (allocate-job M job))  ; add the job to the Machines, provisioning may be necessary
      M)))  ; no more jobs, returns Machines with day-by-day schedule

(def jobs1 ; all jobs require their own machine
  [{:deadline 2 :duration 2 :id 1}
   {:deadline 2 :duration 2 :id 2}
   {:deadline 3 :duration 3 :id 3}
   {:deadline 4 :duration 4 :id 4}])

(def jobs2 ; distributed allocation
  [{:deadline 5 :duration 1 :id 1}
   {:deadline 9 :duration 3 :id 2}
   {:deadline 2 :duration 2 :id 3}
   {:deadline 6 :duration 4 :id 4}
   {:deadline 7 :duration 2 :id 5}])

(def jobs3 ; each on one machine
  [{:deadline 14 :duration 3 :id 1}
   {:deadline 5 :duration 4 :id 2}
   {:deadline 6 :duration 1 :id 3}
   {:deadline 13 :duration 5 :id 4}])

;; Runs greedy algorithm on the Jobs set, prints the schedule across machines
;; Schedule is a ordered list of machines, and each machine is an ordered list, where
;; each cell in the list is a day of operation on the job with an ID given in the cell
;; Days are indexed from 0
(defn run
  []
  (println "Jobs 1")
  (doseq [machine (greedy-machines jobs1)]
    (println machine))
  (println "Jobs 2")
  (doseq [machine (greedy-machines jobs2)]
    (println machine))
  (println "Jobs 3")
  (doseq [machine (greedy-machines jobs3)]
    (println machine)))

;; Results
(comment
user=> (run)
Jobs 1
[1 1]
[2 2]
[3 3 3]
[4 4 4 4]
Jobs 2
[3 3 4 4 4 4 2 2 2]
[1 5 5]
Jobs 3
[2 2 2 2 3 4 4 4 4 4 1 1 1]
)
