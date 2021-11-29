all: *.lisp *.asd
	sbcl --eval '(ql:quickload :tower-pull)' \
	     --eval '(asdf:make :tower-pull)' \
	     --eval '(quit)'
