OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
OCAMLC=ocamlc

TARGET=lithp_parser

all: $(TARGET)

run: $(TARGET)
	./$(TARGET) input.txt > output.txt

grammar.ml grammar.mli: grammar.mly
	$(OCAMLYACC) grammar.mly

lexer.ml: lexer.mll grammar.ml
	$(OCAMLLEX) lexer.mll

$(TARGET): names.mli symbol.ml bigint.ml variable.ml ast.ml type_support.ml type_rules.ml type.ml grammar.ml lexer.ml main.ml
	$(OCAMLC) -c names.mli
	$(OCAMLC) -c symbol.ml
	$(OCAMLC) -c bigint.ml
	$(OCAMLC) -c variable.ml
	$(OCAMLC) -c ast.ml
	$(OCAMLC) -c type_support.ml
	$(OCAMLC) -c type_rules.ml
	$(OCAMLC) -c type.ml
	$(OCAMLC) -c grammar.mli
	$(OCAMLC) -c grammar.ml
	$(OCAMLC) -c lexer.ml
	$(OCAMLC) -c main.ml
	$(OCAMLC) -o $(TARGET) symbol.cmo bigint.cmo variable.cmo ast.cmo type_support.cmo type_rules.cmo type.cmo grammar.cmo lexer.cmo main.cmo

clean:
	rm -f $(TARGET) *.cmi *.cmo grammar.ml grammar.mli lexer.ml output.txt output2.txt
