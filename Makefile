VARIABLE = $(shell cat ./build_system/ml_files.txt)
MODULES= state play authors draw human input minMax player screen settings \
utils window
OBJECTS= $(MODULES:=.cmo)
MLS= $(MODULES:=.ml)
MLIS= $(MODULES:=.mli)
TEST= test.byte
PLAY= play.byte
OCAMLBUILD = ocamlbuild -use-ocamlfind -tag thread

default: | add build

# add files to list of files, TODO: can replace with single command?
add: 
	./build_system/add_files.sh

# compile files in ml_files
build: 
	$(OCAMLBUILD) $(VARIABLE)

check: | add build 
	$(OCAMLBUILD) $(VARIABLE) check.byte && ./check.byte

play:
	$(OCAMLBUILD) $(VARIABLE) $(PLAY) && ./$(PLAY)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

docs: docs-public docs-private

docs-public: | add build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package yojson,graphics \
		-html -stars -d doc.public $(MLIS)

docs-private: | add build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package yojson,graphics \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

zip:
	zip -r functional_connect_four.zip *.ml *.mli _tags build_system logos Makefile