
build:
	sbcl --non-interactive \
	     --load alaman.asd \
             --eval '(ql:quickload alaman)' \
             --eval '(asdf:make alaman)'
