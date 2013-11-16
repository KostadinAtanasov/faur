#!/usr/bin/env racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; faur.scm
;;
;; Copyright (C) 2013 Kostadin Atanasov <kdatanasov@gmail.com>
;;
;; This file is part of faur.
;;
;; faur is free software: you can redistribute it and/or modify
;; if under the terms of GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.
;;
;; faur is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should received a copy of the GNU General Public License
;; along with faur. If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require openssl)
(require json)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Detect if we are in reple mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define inrepl?
  (regexp-match? "racket" (find-system-path 'run-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some global constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define AURURL "aur.archlinux.org")
(define AURPORT 443)
(define GETURL "GET /rpc.php?type=")
(define GETEND " HTTP/1.0\r\n")
(define TYPEPAR "type=")
(define ARGPAR "&arg=")
(define HTTPHOST "Host: aur.archlinux.org\r\n\r\n")
(define HTTPEND "\r\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String and Bytes helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (String Number) -> List-of-Strings
(define (trim-lines str num)
  (let loop ((s str))
    (if (<= (string-length s) num)
        (list s)
        (cons (substring s 0 num) (loop (substring s num))))))

; (Bytes Bytes) -> Number or '()
(define (bytes-find b sb)
  (define len (bytes-length b))
  (define len1 (bytes-length sb))
  (define (loop n ind)
    (if (> n (- len len1))
        '()
        (if (= (bytes-ref b n) (bytes-ref sb ind))
            (if (= ind (- len1 1))
                (+ n 1)
                (loop (+ n 1) (+ ind 1)))
            (loop (+ n 1) 0))))
  (loop 0 0))

; (Bytes) -> List-of-List-of-Bytes
(define (bytes-to-line-list b)
  (let* ((lst '())
         (reminder
          (foldl
           (lambda (x y)
             (if (= x 10)
                 (begin
                   (set! lst (append lst (list (reverse (cons x y)))))
                   '())
                 (cons x y)))
           '()
           (bytes->list b))))
    (if (empty? reminder)
        lst
        (append lst (list reminder)))))

; (Bytes Lambda) -> Bytes or '()
(define (bytes-get-line b pred)
  (let ((res (filter pred (map
                           (lambda (x) (list->bytes x))
                           (bytes-to-line-list b)))))
    (if (empty? res)
        res
        (car res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General port reading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (Input-Port Lambda) -> List-of-Strings
(define (read-response in f)
  (let loop ((l (read-line in)))
    (if (eof-object? l)
        '()
        (cons (f l) (loop (read-line in))))))

; (Input-Port) -> List-of-Strings
(define (read-to-line-list in)
  (read-response in (lambda (x) x)))

; (Input-Port) -> Bytes
(define (read-bytes-response in)
  (define (loop)
    (let ((line (read-bytes-line in)))
      (if (not (eof-object? line))
          (bytes-append line (make-bytes 1 10) (loop))
          (make-bytes 0))))
  (loop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUR handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (List-of-Strings) -> Bool
(define (http-code-ok? resp)
  (let ((l (string-split (list-ref resp 0))))
    (if (string=? (car (cdr l)) "200")
        #t
        #f)))

; (Bytes) -> Bool
(define (bytes-http-code-ok? resp)
  (http-code-ok? (list (bytes->string/locale (subbytes resp 0 16)))))

;; Get only the JSON part of response
; (List-of-Strings) -> String
(define (response-get-json response)
  (if (http-code-ok? response)
      (letrec ((response-line
                (lambda (n)
                  (if (>= n (length response))
                      '()
                      (let ((l (list-ref response n)))
                        (if (string=? l "\r")
                            (list-ref response (+ n 1))
                            (response-line (+ n 1))))))))
        (response-line 1))
      '()))

;; Perform desired RPC request
; (String String) -> Jsexpr
(define (perform-rpc rpctype rpcarg)
  (let-values (((in out) (ssl-connect AURURL AURPORT)))
    (let ((g (string-append GETURL rpctype ARGPAR rpcarg GETEND HTTPHOST)))
      (display g out))
    (let ((jsres (response-get-json (read-to-line-list in))))
      (close-input-port in)
      (close-output-port out)
      (string->jsexpr jsres))))

; (Hash) -> Void called for side effects
(define (print-search s)
  (for-each (lambda (x)
              (let ((name (hash-ref x 'Name))
                    (desc (hash-ref x 'Description))
                    (ver (hash-ref x 'Version)))
                (display (format "~a\t~a\n" name ver))
                (for-each (lambda (d)
                            (display (format "\t~a\n" d)))
                          (trim-lines desc 70))))
            s))

; (Hash) -> Void called for side effects
(define (print-info h)
  (for-each
   (lambda (x)
     (display (format "~a: ~a\n" (car x) (cdr x))))
   (hash->list h)))

; (Jsexpr) -> String
(define (extract-package-aur-version jsres)
  (let ((res (hash-ref jsres 'results)))
    (if (hash? res)
        (hash-ref res 'Version)
        "0")))

; (Bytes) -> Number or '()
(define (extract-contentline-size b)
  (let-values (((f s) (splitf-at
                       (bytes->list b)
                       (lambda (x) (not (= x 58))))))
    (if (not (empty? s))
        (let* ((cl (map (lambda (x) (integer->char x)) s))
               (nl (filter (lambda (x) (string->number (string x))) cl)))
          (string->number (list->string nl)))
        '())))

; (Bytes) -> Bytes
(define (extract-gz http-response)
  (if (bytes-http-code-ok? http-response)
      (let ((pos (bytes-find http-response (bytes 13 10 13 10)))
            (size-line (bytes-get-line http-response
                                       (lambda (x)
                                         (regexp-match? "Content-Length" x)))))
        (set! size-line (extract-contentline-size size-line))
        (if (and (not (empty? pos)) (not (empty? size-line)))
            (subbytes http-response pos (+ pos size-line))
            (make-bytes 0)))
      (make-bytes 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command line handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parsedcmd '())
;; Accepted command line switches
(define sync? (make-parameter #f))
(define search? (make-parameter #f))
(define info? (make-parameter #f))
(define update? (make-parameter #f))
(define badoption? (make-parameter #f))

; (String) -> Void called for side effects
(define (parse-one-group gr)
  (let ((fc (string-ref gr 0)))
    (if (eq? fc #\-)
        (let ((len (string-length gr)))
          (let loop ((n 1))
            (when (not (= n len))
              (let ((c (string-ref gr n)))
                (cond ((eq? c #\S) (sync? #t))
                      ((eq? c #\s) (search? #t))
                      ((eq? c #\u) (update? #t))
                      ((eq? c #\i) (info? #t))
                      ((eq? c #\y) (void)) ;; refresh doesn't make sense
                      (#t (badoption? #t))))
              (loop (+ n 1)))))
        (set! parsedcmd gr))))

(define (parse-cmd cmdline)
  (vector-map parse-one-group cmdline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pacman handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (String) -> List-of-Strings
(define (call-pacman args)
  (let ((pacman-control (process (string-append "pacman " args)))
        (pacman-response '()))
    ((list-ref pacman-control 4) 'wait)
    (close-output-port (list-ref pacman-control 1))
    (close-input-port (list-ref pacman-control 3))
    (set! pacman-response (read-to-line-list (list-ref pacman-control 0)))
    (close-input-port (list-ref pacman-control 0))
    pacman-response))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ABS handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define temp-dir '())

; () -> String
(define (get-temp-dir)
  (if (empty? temp-dir)
      (let ((sys-tmp (path->string (find-system-path 'temp-dir)))
            (now (number->string (current-seconds))))
        (set! temp-dir (string-append sys-tmp "/faur_" now))
        temp-dir)
      temp-dir))

; () -> Void called for side effects
(define (make-temp-dir)
  (when (not (directory-exists? (get-temp-dir)))
    (make-directory (get-temp-dir))))

(define delete-temp-dir-pred #t)

; () -> Bool
(define (delete-temp-dir?)
  delete-temp-dir-pred)

; (Bool) -> Void called for side effects
(define (set-delete-temp-dir! b)
  (set! delete-temp-dir-pred b))

; (String) -> String
(define (urlpath->filename url)
  (let ((l (regexp-split "/" url)))
    (list-ref l (- (length l) 1))))

; (Hash) -> Void called for side effects
(define (start-makepkg package jsres)
  (let ((url (hash-ref (hash-ref jsres 'results) 'URLPath)))
    (make-temp-dir)
    (define-values (in out) (ssl-connect AURURL AURPORT))
    (define g (string-append "GET " url GETEND HTTPHOST))
    (display g out)
    (define targz-raw (extract-gz (read-bytes-response in)))
    (close-input-port in)
    (close-output-port out)
    (define r (open-output-file
               (string-append (get-temp-dir) "/" (urlpath->filename url))))
    (write-bytes targz-raw r)
    (close-output-port r)
    (display
     (format "package ~a dowlnloaded in ~a\n" package (get-temp-dir)))
    (set-delete-temp-dir! #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Our interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (String String) -> Void called for side effects
(define (update-package package installed-version)
  (let ((jsres (perform-rpc "info" package)))
    (if (> (hash-ref jsres 'resultcount) 0)
        (let ((version (extract-package-aur-version jsres)))
          (when (string>? version installed-version)
            (start-makepkg package jsres)))
        (display (format "No such ~a in AUR\n" package)))))

; (List-of-Strings) -> Void called for side effects
(define (update-installed-packages pacman-Qm)
  (for-each (lambda (line)
              (let ((lst (string-split line)))
                (update-package (car lst) (car (cdr lst)))))
            pacman-Qm))

; (String) -> Void called for side effects
(define (perform-update package)
  (define pacman-Qm (call-pacman "-Qm"))
  (if (empty? package)
      (update-installed-packages pacman-Qm)
      (let ((info (filter (lambda (x)
                            (if (string=? (car (string-split x)) package)
                                #t
                                #f))
                          pacman-Qm)))
        (if (not (empty? info))
            (update-package package (car (cdr (string-split (car info)))))
            (display (format "No such package ~a\n" package))))))

; (String) -> Void called for side effects
(define (main package)
  (cond ((badoption?) (display "Bad option on command line\n"))
        ((sync?)
         (cond ((search?)
                (if (not (empty? package))
                    (let ((jsres (perform-rpc "search" package)))
                      (when (> (hash-ref jsres 'resultcount) 0)
                        (print-search (hash-ref jsres 'results))))
                    (display "some name required for searching\n")))
               ((info?)
                (if (not (empty? package))
                    (let ((jsres (perform-rpc "info" package)))
                      (when (> (hash-ref jsres 'resultcount) 0)
                        (print-info (hash-ref jsres 'results))))
                    (display "package name required to get info about it\n")))
               ((update?)
                (perform-update package))
               (#t
                (if (not (empty? package))
                    (update-package package "0")
                    (display
                     "Install - at least one package name required\n")))))
        (inrepl? (void))
        (#t (display "You should provide at least one operation\n")))
  (when (and
         (directory-exists? (get-temp-dir))
         (delete-temp-dir?))
    (system (string-append "rm -r " (get-temp-dir)))))

(when (not inrepl?)
  (parse-cmd (current-command-line-arguments))
  (main parsedcmd))
