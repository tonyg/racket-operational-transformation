#lang racket/gui

(require racket/cmdline)
(require racket/match)
(require racket/serialize)
(require racket/tcp)
(require operational-transformation)
(require operational-transformation/text)
(require operational-transformation/text/simple-document)

(define ot-text%
  (class text%
    (init-field [insert-callback (lambda (loc str) (void))]
                [delete-callback (lambda (loc len) (void))])
    (super-new)
    (inherit get-text
             select-all
             clear
             insert
             delete)
    (define remote-edit? #f)
    (define/augment (after-delete loc len)
      (when (not remote-edit?)
        (delete-callback loc len)))
    (define/augment (after-insert loc len)
      (when (not remote-edit?)
        (insert-callback loc (get-text loc (+ loc len)))))
    (define/public (replace-contents new-contents)
      (set! remote-edit? #t)
      (select-all)
      (clear)
      (insert new-contents 0)
      (set! remote-edit? #f))
    (define/public (document-perform op #:remote? remote?)
      ;; (printf "document-perform: ~v ~v\n" op remote?) (flush-output)
      (set! remote-edit? remote?)
      (when remote?
        (match op
          [(insertion loc str)
           (insert str loc)]
          [(deletion loc len)
           (delete loc (+ loc len))]))
      (set! remote-edit? #f)
      this)))

(define ot-editor-canvas%
  (class editor-canvas%
    (super-new)
    (inherit get-editor)
    (define/override (on-char e)
      (cond
        [(send e get-control-down)
         (case (send e get-key-code)
           [(#\q #\Q) (send (send this get-top-level-window) show #f)]
           [(#\a #\A) (send (get-editor) select-all)]
           [(#\x #\X) (send (get-editor) cut)]
           [(#\c #\C) (send (get-editor) copy)]
           [(#\v #\V) (send (get-editor) paste)]
           [else (void)])]
        [else (super on-char e)]))))

(define (write-msg v o)
  (write (serialize v) o)
  (newline o)
  (flush-output o))

(define (read-msg i)
  (deserialize (read i)))

(define (run-client #:port port #:host server-host)
  (define-values (i o) (tcp-connect server-host port))

  (define (disconnect!)
    (send f show #f))

  (define handle-server-event!
    (match-lambda
      [(list filenames ...)
       (send fns clear)
       (for [(n filenames)] (send fns append n))
       (select-filename fns #f)]
      [(server-snapshot rev (simple-document content))
       (send t replace-contents content)
       (set! c (make-client (server-snapshot rev t)))
       (send e enable #t)
       (send e focus)]
      [(? pending-operation? p)
       (maybe-send! (incorporate-operation-from-server c p))
       (write-msg (client-state-server-revision c) o)]))

  (define (on-insertion loc str)
    (send-to-server! (insertion loc str)))

  (define (on-deletion loc len)
    (send-to-server! (deletion loc len)))

  (define (maybe-send! updated-c)
    (define-values (p new-c) (flush-buffered-operation updated-c))
    (set! c new-c)
    (when p (write-msg p o)))

  (define (send-to-server! op)
    (maybe-send! (incorporate-local-operation c op)))

  (define (select-filename _widget _evt)
    (define selected-filename (send fns get-string-selection))
    (send e enable #f)
    (write-msg selected-filename o))

  (define f (new frame%
                 [label "Racket OT Client"]
                 [width 640]
                 [height 480]))
  (define t (new ot-text%
                 [insert-callback on-insertion]
                 [delete-callback on-deletion]))
  (define fns (new choice%
                   [label "File "]
                   [choices '()]
                   [parent f]
                   [min-width 300]
                   [stretchable-width #t]
                   [callback select-filename]))
  (define e (new ot-editor-canvas%
                 [parent f]
                 [editor t]))

  (define c #f)

  (thread (lambda ()
            (with-handlers [(values (lambda (e) (queue-callback disconnect!)))]
              (let loop ()
                (define v (read-msg i))
                (if (eof-object? v)
                    (queue-callback disconnect!)
                    (begin (queue-callback (lambda () (handle-server-event! v)))
                           (loop)))))))

  (send e enable #f)
  (send f show #t))

(module+ main
  (define port 5889)
  (command-line
   #:once-each
   [("-p" "--port") server-port ((format "Server port (default ~v)" port))
    (set! port (string->number server-port))]
   #:args (server-hostname)
   (run-client #:port port #:host server-hostname)))
