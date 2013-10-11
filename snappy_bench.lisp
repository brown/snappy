

(defconst +benchmark-files+
  '(("html" "html")
    ("urls" "urls.10K")
    ("jpg" "house.jpg")
    ("pdf" "mapreduce-osdi-1.pdf")
    ("html4" "html_x_4")
    ("cp" "cp.html")
    ("c" "fields.c")
    ("lsp" "grammar.lsp")
    ("xls" "kennedy.xls")
    ("txt1" "alice29.txt")
    ("txt2" "asyoulik.txt")
    ("txt3" "lcet10.txt")
    ("txt4" "plrabn12.txt")
    ("bin" "ptt5")
    ("sum" "sum")
    ("man" "xargs.1")
    ("pb" "geo.protodata")
    ("gaviota" "kppkn.gtb")))

(defun read-file (file-name)
  ;; XXXX Catch any file error ...
  (with-open-file (stream file-name
                          :element-type 'com.google.base:octet
                          :if-does-not-exist :error)
    (let* ((octet-count (file-length stream))
           (data (com.google.base:make-octet-vector octet-count))
           (octets-read (read-sequence data stream)))
      (if (= octets-read octet-count)
        (values data t)
        (values (com.google.base:make-octet-vector 0) nil)))))
