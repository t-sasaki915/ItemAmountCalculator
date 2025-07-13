BUILD_DIR := build

.PHONY: build http copy-statics clean

build: copy-statics
	mkdir -p $(BUILD_DIR)

	cabal v2-build --project-file cabal-js.project --allow-newer
	cp -v $(shell echo $$(cabal list-bin ItemAmountCalculator-exe --project-file cabal-js.project --allow-newer) | grep -o '/[^"]*').jsexe/all.js $(BUILD_DIR)/index.js

	@echo ""
	@echo "BUILD SUCCESSFUL."
	@echo ""

http: build
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
