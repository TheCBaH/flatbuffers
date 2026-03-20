SUBMODULE := flatbuffers
PATCH := patches/ocaml-integration.patch
FLATC := flatc

.PHONY: all patch flatc deps test generate generate-check bench clean clean-flatc rebuild-patch

all: flatc test

patch:
	@if ! git -C $(SUBMODULE) diff --quiet; then \
		echo "Submodule already has local changes, skipping patch"; \
	else \
		cp src/bfbs_gen_ocaml.cpp src/bfbs_gen_ocaml.h $(SUBMODULE)/src/; \
		git -C $(SUBMODULE) apply --3way ../$(PATCH); \
		echo "Patch applied"; \
	fi

rebuild-patch:
	@if git -C $(SUBMODULE) diff --quiet; then \
		echo "Error: submodule has no local changes to capture" >&2; \
		exit 1; \
	fi
	git -C $(SUBMODULE) diff > $(PATCH)
	@echo "Patch rebuilt: $(PATCH)"

flatc: patch
	mkdir -p $(SUBMODULE)/build
	cd $(SUBMODULE)/build && cmake .. -DFLATBUFFERS_BUILD_TESTS=OFF
	$(MAKE) -C $(SUBMODULE)/build -j$$(nproc) flatc
	cp $(SUBMODULE)/build/flatc $(FLATC)
	./$(FLATC) --version

deps:
	opam install . --deps-only -t -y

test: flatc
	opam exec -- dune test --root . --ignore-promoted-rules

generate: flatc
	opam exec -- dune build --root . --force @gen-sample @gen-string-union @gen-more-defaults @gen-casing-test

generate-check: generate
	@if ! git diff --quiet -- ocaml/test/generated/; then \
		echo "Error: generated files are out of date:" >&2; \
		git diff --stat -- ocaml/test/generated/; \
		exit 1; \
	fi
	@echo "Generated files are up to date"

bench: flatc
	opam exec -- dune exec --root . --profile=release --display=quiet ocaml/test/bench/fb_bench.exe

clean:
	opam exec -- dune clean
	rm -rf $(SUBMODULE)/build $(FLATC)
	git -C $(SUBMODULE) checkout -- . 2>/dev/null || true

clean-flatc:
	rm -rf $(SUBMODULE)/build $(FLATC)
	git -C $(SUBMODULE) checkout -- . 2>/dev/null || true
