SUBMODULE := flatbuffers
PATCH := patches/ocaml-integration.patch
FLATC := flatc

.PHONY: all patch flatc deps test bench clean clean-flatc

all: flatc test

patch:
	@if ! git -C $(SUBMODULE) diff --quiet; then \
		echo "Submodule already has local changes, skipping patch"; \
	else \
		cp src/bfbs_gen_ocaml.cpp src/bfbs_gen_ocaml.h $(SUBMODULE)/src/; \
		git -C $(SUBMODULE) apply ../$(PATCH); \
		echo "Patch applied"; \
	fi

flatc: patch
	mkdir -p $(SUBMODULE)/build
	cd $(SUBMODULE)/build && cmake .. -DFLATBUFFERS_BUILD_TESTS=OFF
	$(MAKE) -C $(SUBMODULE)/build -j$$(nproc) flatc
	cp $(SUBMODULE)/build/flatc $(FLATC)
	./$(FLATC) --version

deps:
	opam install . --deps-only -t -y

test: flatc
	opam exec -- dune test --root .

bench: flatc
	opam exec -- dune exec --root . --profile=release --display=quiet ocaml/test/bench/fb_bench.exe

clean:
	opam exec -- dune clean
	rm -rf $(SUBMODULE)/build $(FLATC)
	git -C $(SUBMODULE) checkout -- . 2>/dev/null || true

clean-flatc:
	rm -rf $(SUBMODULE)/build $(FLATC)
	git -C $(SUBMODULE) checkout -- . 2>/dev/null || true
