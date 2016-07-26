#lang racket

(require racket/async-channel)
(require racket/serialize)
(require operational-transformation)
(require operational-transformation/text/simple-document)

(struct state (server-state connections filename) #:prefab)
(struct connection (thread seen-up-to) #:prefab)

(define (run-server #:port port #:filename filename)
  (define listener (tcp-listen port 10 #t))
  (define ch (make-async-channel))
  (log-info "listening on port ~v" port)
  (let loop ((s (state (make-server (simple-document (if (file-exists? filename)
                                                         (begin (log-info "loading ~v" filename)
                                                                (file->string filename))
                                                         (begin (log-info "will create ~v" filename)
                                                                ""))))
                       (hash)
                       filename)))
    (sync (handle-evt (tcp-accept-evt listener)
                      (lambda (ports)
                        (match-define (list i o) ports)
                        (define id (gensym 'conn))
                        (log-info "~a: connected" id)
                        (define t (thread (lambda () (connection-main id i o ch))))
                        (let* ((snap (extract-snapshot (state-server-state s)))
                               (s (struct-copy state s
                                               [connections
                                                (hash-set (state-connections s) id
                                                          (connection t (server-snapshot-revision
                                                                         snap)))]))
                               (s (send-to s id filename))
                               (s (send-to s id snap)))
                          (loop s))))
          (handle-evt ch
                      (match-lambda
                        [(? symbol? id)
                         (log-info "~a: disconnected" id)
                         (loop (forget-history
                                (struct-copy state s
                                             [connections
                                              (hash-remove (state-connections s) id)])))]
                        [(list id (? number? new-seen-up-to))
                         (loop (forget-history (bump-seen-up-to s id new-seen-up-to)))]
                        [(list id (? pending-operation? p))
                         ;; (log-info "~a: sent us ~v" id p)
                         (loop (save (broadcast-operation (do-operation s p))))])))))

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

(define (connection-main id i o ch)
  (define (shutdown!)
    (with-handlers [(values void)] (close-input-port i))
    (with-handlers [(values void)] (close-output-port o))
    (async-channel-put ch id))
  (with-handlers [(values (lambda (e)
                            (shutdown!)
                            (raise e)))]
    (let loop ()
      (sync (handle-evt (thread-receive-evt)
                        (lambda (_dummy)
                          (write (serialize (thread-receive)) o)
                          (newline o)
                          (flush-output o)
                          (loop)))
            (handle-evt (read-line-evt i 'any)
                        (lambda (line)
                          (if (eof-object? line)
                              (shutdown!)
                              (begin (async-channel-put
                                      ch
                                      (list id (deserialize (read (open-input-string line)))))
                                     (loop)))))))))

(module+ main
  (define port 5888)
  (command-line
   #:once-each
   [("-p" "--port") server-port ((format "Server port (default ~v)" port))
    (set! port server-port)]
   #:args (filename)
   (run-server #:port port
               #:filename filename)))
