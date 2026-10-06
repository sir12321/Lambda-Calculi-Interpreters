SUBDIRS := abstract-machines lithp-type-inference

.PHONY: all clean test $(SUBDIRS)

all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean:
	for d in $(SUBDIRS); do $(MAKE) -C $$d clean || exit 1; done

test:
	for d in $(SUBDIRS); do $(MAKE) -C $$d test || exit 1; done
