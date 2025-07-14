SHELL         := /bin/bash
BUILD_DIR     := build
CABAL_VERSION := 3.14.2.0
GHC_VERSION   := 9.12.2
EMSDK_VERSION := 3.1.74

.PHONY: build http copy-statics clean init-tools

build: copy-statics
	mkdir -p $(BUILD_DIR)

	cabal v2-build --project-file cabal-js.project --allow-newer

	@INPUT_JS=$(shell echo $$(cabal list-bin ItemAmountCalculator-exe --project-file cabal-js.project --allow-newer) | grep -o '/[^"]*').jsexe/all.js && \
	OUTPUT_JS="$(BUILD_DIR)/index.js" && \
	if [ "$(PRODUCTION)" = "1" ]; then \
		node-minify --compressor uglify-js --input "$$INPUT_JS" --output "$$OUTPUT_JS"; \
	else \
		cp -v "$$INPUT_JS" "$$OUTPUT_JS"; \
	fi

	@echo ""
	@echo "BUILD SUCCESSFUL."
	@echo ""

debug: build
ifeq (, $(shell which http-server))
	sudo npm install -g http-server
endif

	http-server $(BUILD_DIR)

copy-statics:
	mkdir -p $(BUILD_DIR)

	cp -v -r static/* $(BUILD_DIR)

	@echo ""
	@echo "COPY-STATICS SUCCESSFUL."
	@echo ""

clean:
	cabal v2-clean
	rm -rf $(BUILD_DIR)

	@echo ""
	@echo "CLEAN SUCCESSFUL."
	@echo ""

init-tools:
ifeq (, $(shell which npm))
	$(error Please install npm)
endif
ifeq (, $(shell which curl))
	$(error Please install curl)
endif
	
	export BOOTSTRAP_HASKELL_NONINTERACTIVE=1; \
	export BOOTSTRAP_HASKELL_MINIMAL=1; \
	export BOOTSTRAP_HASKELL_ADJUST_BASHRC=1; \
	curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

	bash -c "source ~/.ghcup/env && ghcup install cabal $(CABAL_VERSION)"

	git clone https://github.com/emscripten-core/emsdk.git ~/.emsdk
	~/.emsdk/emsdk install $(EMSDK_VERSION)
	~/.emsdk/emsdk activate $(EMSDK_VERSION)

	bash -c "source ~/.ghcup/env && ghcup config add-release-channel cross"
	bash -c "source ~/.ghcup/env && source ~/.emsdk/emsdk_env.sh && emconfigure ghcup install ghc --set javascript-unknown-ghcjs-$(GHC_VERSION)"

	sudo npm install -g @node-minify/cli @node-minify/uglify-js

	@echo ""
	@echo "INIT-ENV SUCCESSFUL."
	@echo "Please run 'source ~/.bashrc' before 'make'."
	@echo ""
