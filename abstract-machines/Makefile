OCAMLC := ocamlc
TARGET := lambda_interp
SOURCES := declarations.ml krivine.ml secd.ml main.ml
OBJECTS := declarations.cmo krivine.cmo secd.cmo input.cmo main.cmo
INTERFACES := declarations.cmi krivine.cmi secd.cmi input.cmi main.cmi

.PHONY: all run clean rebuild

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(OCAMLC) -o $@ $(OBJECTS)

declarations.cmo: declarations.ml
	$(OCAMLC) -c declarations.ml

krivine.cmo: declarations.cmo krivine.ml
	$(OCAMLC) -c krivine.ml

secd.cmo: declarations.cmo secd.ml
	$(OCAMLC) -c secd.ml

input.cmo: declarations.cmo input.txt
	$(OCAMLC) -c -impl input.txt

main.cmo: declarations.cmo krivine.cmo secd.cmo input.cmo main.ml
	$(OCAMLC) -c main.ml

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(TARGET) $(OBJECTS) $(INTERFACES)

rebuild: clean all
