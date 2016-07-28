#lang racket

(require racket/async-channel)
(require racket/serialize)
(require operational-transformation)
(require operational-transformation/text/simple-document)

(struct state (server-state connections filename) #:prefab)
(struct connection (thread seen-up-to) #:prefab)

(define (run-server #:port port #:filenames filenames)
  (define listener (tcp-listen port 10 #t))
  (log-info "listening on port ~v" port)
  (define server-threads
    (for/hash [(filename filenames)] (values filename (run-one-server filename))))
  (let loop ((threads (hash)))
    (apply sync
           (handle-evt (tcp-accept-evt listener)
                       (lambda (ports)
                         (match-define (list i o) ports)
                         (define id (gensym 'conn))
                         (log-info "~a: connected" id)
                         (define t (thread (lambda () (connection-main id i o))))
                         (thread-send t server-threads #f)
                         (loop (hash-set threads id t))))
           (for/list [((id t) (in-hash threads))]
             (handle-evt (thread-dead-evt t)
                         (lambda (_dummy)
                           (log-info "~a: disconnected" id)
                           (loop (hash-remove threads id))))))))

(define (run-one-server filename)
  (thread
   (lambda ()
     (let loop ((s (state (make-server (simple-document
                                        (if (file-exists? filename)
                                            (begin (log-info "loading ~v" filename)
                                                   (file->string filename))
                                            (begin (log-info "will create ~v" filename)
                                                   ""))))
                          (hash)
                          filename)))
       (apply sync
              (handle-evt (thread-receive-evt)
                          (lambda (_dummy)
                            (match (thread-receive)
                              [(list id (? thread? t))
                               (log-info "thread ~a attached to file ~a" id filename)
                               (let* ((snap (extract-snapshot (state-server-state s)))
                                      (c (connection t (server-snapshot-revision snap)))
                                      (s (struct-copy state s
                                                      [connections
                                                       (hash-set (state-connections s) id c)]))
                                      (s (send-to s id snap)))
                                 (loop s))]
                              [(? symbol? id)
                               (log-info "thread ~a detached from file ~a" id filename)
                               (loop (handle-disconnection id s))]
                              [(list id (? number? new-seen-up-to))
                               (loop (forget-history (bump-seen-up-to s id new-seen-up-to)))]
                              [(list id (? pending-operation? p))
                               ;; (log-info "~a: sent us ~v" id p)
                               (loop (save (broadcast-operation (do-operation s p))))])))
              (for/list [((id c) (in-hash (state-connections s)))]
                (handle-evt (thread-dead-evt (connection-thread c))
                            (lambda (_dummy)
                              (log-info "thread ~a died while attached to file ~a" id filename)
                              (loop (handle-disconnection id s))))))))))

(define (handle-disconnection id s)
  (forget-history (struct-copy state s [connections (hash-remove (state-connections s) id)])))

(define (do-operation s p)
  (struct-copy state s [server-state
                        (incorporate-operation-from-client (state-server-state s) p)]))

(define (broadcast-operation s)
  (define p (extract-operation (state-server-state s)))
  (if p
      (for/fold [(s s)] [(id (in-hash-keys (state-connections s)))]
        (send-to s id p))
      s))

(define (bump-seen-up-to s id new-seen-up-to)
  (define c (hash-ref (state-connections s) id))
  (struct-copy state s [connections (hash-set (state-connections s) id
                                              (struct-copy connection c
                                                           [seen-up-to new-seen-up-to]))]))

(define (forget-history s)
  (define min-rev
    (for/fold [(rev (server-state-revision (state-server-state s)))]
              [(c (in-hash-values (state-connections s)))]
      (min rev (connection-seen-up-to c))))
  (struct-copy state s [server-state (forget-operation-history (state-server-state s) min-rev)]))

(define (send-to s id v)
  ;; (log-info "~a: sending them ~v" id v)
  (thread-send (connection-thread (hash-ref (state-connections s) id)) v #f)
  s)

(define (save s)
  (display-to-file (simple-document-text (server-state-document (state-server-state s)))
                   (state-filename s)
                   #:exists 'replace)
  s)

(define (write-serialized v o)
  (write (serialize v) o)
  (newline o)
  (flush-output o))

(define (connection-main id i o)
  (define (shutdown!)
    (with-handlers [(values void)] (close-input-port i))
    (with-handlers [(values void)] (close-output-port o)))
  (with-handlers [(values (lambda (e)
                            (shutdown!)
                            (raise e)))]
    (let loop ((server-threads (hash))
               (selected-filename #f))
      (define (send-server filename msg)
        (when (hash-has-key? server-threads filename)
          (thread-send (hash-ref server-threads filename) msg #f)))
      (sync (handle-evt (thread-receive-evt)
                        (lambda (_dummy)
                          (match (thread-receive)
                            [(? hash? new-server-threads)
                             (write-serialized (hash-keys new-server-threads) o)
                             (loop new-server-threads selected-filename)]
                            [other
                             (write-serialized other o)
                             (loop server-threads selected-filename)])))
            (handle-evt (read-line-evt i 'any)
                        (lambda (line)
                          (if (eof-object? line)
                              (shutdown!)
                              (match (deserialize (read (open-input-string line)))
                                [(? string? new-filename)
                                 (send-server selected-filename id)
                                 (send-server new-filename (list id (current-thread)))
                                 (loop server-threads new-filename)]
                                [(? number? new-seen-up-to)
                                 (send-server selected-filename (list id new-seen-up-to))
                                 (loop server-threads selected-filename)]
                                [(? pending-operation? p)
                                 (send-server selected-filename (list id p))
                                 (loop server-threads selected-filename)]))))))))

(module+ main
  (define port 5889)
  (command-line
   #:once-each
   [("-p" "--port") server-port ((format "Server port (default ~v)" port))
    (set! port (string->number server-port))]
   #:args filenames
   (run-server #:port port
               #:filenames filenames)))
