
all: build test

build:
	jbuilder build @install

test:
	jbuilder runtest --no-buffer --force

clean:
	jbuilder clean

doc:
	jbuilder build @doc

benchs:
	jbuilder build $(addprefix bench/, $(BENCH_TARGETS))

VERSION=$(shell awk '/^version:/ {print $$2}' oseq.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" src/*.ml src/*.mli
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" src/*.ml src/*.mli

reindent:
	@which ocp-indent || ( echo "require ocp-indent" ; exit 1 )
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -type f -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -type f -print0 | xargs -0 ocp-indent -i

watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		sleep 0.2; \
		make all; \
	done

.PHONY: benchs tests examples update_next_tag watch
