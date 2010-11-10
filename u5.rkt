(define albums '((U2        
                  Pop
                  (Discothèque
                   Do-You-Feel-Loved
                   Mofo
                   If-God-Will-Send-His-Angels
                   Staring-at-the-Sun
                   Last-Night-on-Earth
                   Gone
                   Miami
                   The-Playboy-Mansion
                   If-You-Wear-That-Velvet-Dress
                   Please
                   Wake-up-Dead-Man)
                  Pop-Rock
                  1997)
                 (Allan-Holdsworth
                  Metal-Fatigue
                  (Metal-Fatigue Home Devil-Take-the-Hindmost Panic-Station The-Un-Merry-Go-Round In-the-Mystery)
                  Fusion
                  1985)
                 (John-Coltrane A-Love-Supreme (Acknowledgement
                                                Resolution Pursuance) Jazz 1964)))

(define artist
  (lambda (album)
    (first album)))

(define album-name
  (lambda (album)
    (second album)))

(define track-list
  (lambda (album)
    (third album)))

(define genre
  (lambda (album)
    (fourth album)))

(define year
  (lambda (album)
    (fifth album)))

(define make-album
  (lambda (artist name tracklist genre year)
    (list artist name tracklist genre year)))

(define add-album
  (lambda (album albums)
    (let ((alb-len (length albums)))
      (build-list (+ 1 alb-len)
                  (lambda (x)
                    (if (< x alb-len) (list-ref albums x) album))))))

(define add-album2
  (lambda (album albums)
    (append albums (list album))))

(define remove-album
  (lambda (index albums)
    (build-list (- (length albums) 1)
               (lambda (x)
                 (if (< x index)
                    (list-ref albums x)
                    (list-ref albums (+ x 1)))))))

(define projection
  (lambda (albums columns)
    (map (lambda (album)
           (map (lambda (selector)
                  (selector album))
               columns))
        albums)))

(define track-counts
  (lambda (albums)
    (map (lambda (album)
           (list (album-name album) (length (track-list album)))) albums)))

(define track-counts2
  (lambda (albums)
    (let ((data (projection albums (list album-name track-list))))
      (map (lambda (row)
             (list (car row) (length (cadr row)))) data))))

(define print-tracks
  (lambda (tracklist)
    (let ((nums (build-list (length tracklist)
                           (lambda (x) (+ x 1)))))
      (map (lambda (num track)
             (display num)
             (display ". ")
             (display track)
             (display "\n")) nums tracklist))))

(define print-album
  (lambda (album)
    (display "Artist: ")
    (display (artist album))
    (display "\nAlbum Title: ")
    (display (album-name album))
    (display "\nGenre: ")
    (display (genre album))
    (display "\nYear of Release: ")
    (display (year album))
    (display "\nTracklist:\n")
    (print-tracks (track-list album))))

(define print-albums
  (lambda (albums)
    (map (lambda (album)
           (print-album album)
           (display "--------------------------------------\n")) albums)))


; testy
;(artist (first albums))
;(album-name (first albums))
;(track-list (first albums))
;(genre (first albums))
;(year (first albums))

;(make-album 'Miles-Davis 'In-A-Silent-Way '(Shhh/Peaceful In-a-Silent-Way) 'Jazz 1969)
;(add-album (make-album 'eva-a-vasek 'zaziva-mrtvi '(1 2 3) 'kdeco 2007) albums)
;(remove-album 0 albums)
;(projection albums (list album-name artist))
;(track-counts albums)
;(track-counts2 albums)
;(print-album (third albums))
;(print-albums albums)