SUBMODULE := flatbuffers
PATCH := patches/ocaml-integration.patch
FLATC := flatc.ocaml
FLATBUFFERS_ARCHIVE := flatbuffers.tar.gz

.PHONY: all patch flatc deps test test-jsoo test-melange generate generate-check bench clean clean-flatc rebuild-patch update-opam check-opam

all: flatc test

patch:
	@if [ -d $(SUBMODULE)/.git ] || [ -f $(SUBMODULE)/.git ]; then \
		if ! git -C $(SUBMODULE) diff HEAD --quiet; then \
			echo "Submodule already has local changes, skipping patch"; \
		else \
			cp src/bfbs_gen_ocaml.cpp src/bfbs_gen_ocaml.h $(SUBMODULE)/src/; \
			git -C $(SUBMODULE) apply --3way ../$(PATCH); \
			echo "Patch applied"; \
		fi; \
	elif [ -f $(FLATBUFFERS_ARCHIVE) ]; then \
		mkdir -p $(SUBMODULE) && tar xzf $(FLATBUFFERS_ARCHIVE) -C $(SUBMODULE) --strip-components=1; \
		cp src/bfbs_gen_ocaml.cpp src/bfbs_gen_ocaml.h $(SUBMODULE)/src/; \
		patch -p1 -d $(SUBMODULE) < $(PATCH); \
		echo "Patch applied from archive"; \
	else \
		echo "Error: $(SUBMODULE) is not a git repo and no $(FLATBUFFERS_ARCHIVE) found" >&2; \
		exit 1; \
	fi

rebuild-patch:
	@if git -C $(SUBMODULE) diff HEAD --quiet; then \
		echo "Error: submodule has no local changes to capture" >&2; \
		exit 1; \
	fi
	git -C $(SUBMODULE) diff HEAD > $(PATCH)
	@echo "Patch rebuilt: $(PATCH)"

flatc: patch
	mkdir -p $(SUBMODULE)/build
	cd $(SUBMODULE)/build && cmake .. -DFLATBUFFERS_BUILD_TESTS=OFF
	$(MAKE) -C $(SUBMODULE)/build -j$$(nproc 2>/dev/null || sysctl -n hw.ncpu || echo 1) flatc
	cp $(SUBMODULE)/build/flatc $(FLATC)
	./$(FLATC) --version

deps:
	opam install . --deps-only -t -y

test: flatc
	opam exec -- dune test --root . --ignore-promoted-rules

generate: flatc
	opam exec -- dune build --root . --force @gen-sample @gen-string-union @gen-more-defaults @gen-casing-test

generate-check: generate check-opam
	@if ! git diff --quiet -- ocaml/test/generated/ flatbuffers-tools.opam; then \
		echo "Error: generated files are out of date:" >&2; \
		git diff --stat -- ocaml/test/generated/ flatbuffers-tools.opam; \
		exit 1; \
	fi
	@echo "Generated files are up to date"

check-opam:
	@set -eu; \
	SHA=$$(git rev-parse HEAD:flatbuffers); \
	grep -q "$$SHA" flatbuffers-tools.opam || \
	  { printf 'Error: flatbuffers-tools.opam SHA out of sync (expected %s).\nRun: make update-opam\n' \
	    "$$SHA" >&2; exit 1; }

update-opam:
	@set -eu; \
	SHA=$$(git rev-parse HEAD:flatbuffers); \
	URL="https://github.com/google/flatbuffers/archive/$$SHA.tar.gz"; \
	if command -v sha256sum > /dev/null 2>&1; then \
	  CHECKSUM=$$(curl -fsSL "$$URL" | sha256sum | awk '{print $$1}'); \
	else \
	  CHECKSUM=$$(curl -fsSL "$$URL" | shasum -a 256 | awk '{print $$1}'); \
	fi; \
	[ -n "$$CHECKSUM" ] || { echo "Error: failed to compute checksum (curl/shasum failed?)" >&2; exit 1; }; \
	sed -i.bak -E \
	  -e "s|/archive/[a-f0-9]+\.tar\.gz|/archive/$$SHA.tar.gz|" \
	  -e "s|sha256=[a-f0-9]+|sha256=$$CHECKSUM|" \
	  flatbuffers-tools.opam; \
	rm -f flatbuffers-tools.opam.bak; \
	echo "Updated flatbuffers-tools.opam: $$SHA"

test-jsoo:
	opam exec -- dune build --root . --ignore-promoted-rules ocaml/jsoo/test_jsoo.bc.js
	node _build/default/ocaml/jsoo/test_jsoo.bc.js

test-melange:
	opam exec -- dune build --root . --ignore-promoted-rules ocaml/melange/
	node _build/default/ocaml/melange/output/ocaml/melange/test_melange.js

bench: flatc
	opam exec -- dune exec --root . --profile=release --display=quiet ocaml/test/bench/fb_bench.exe

clean:
	opam exec -- dune clean
	rm -rf $(SUBMODULE)/build $(FLATC)
	git -C $(SUBMODULE) checkout HEAD -- . 2>/dev/null || true

clean-flatc:
	rm -rf $(SUBMODULE)/build $(FLATC)
	git -C $(SUBMODULE) checkout HEAD -- . 2>/dev/null || true
