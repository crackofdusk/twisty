SRC = src
ASSETS = assets
BUILD = build

build: build-directory
	elm make $(SRC)/Main.elm --output $(BUILD)/app.js
	cp $(SRC)/index.html $(BUILD)
	cp -r $(ASSETS) $(BUILD)

build-directory:
	mkdir -p $(BUILD)

clean:
	rm -rf $(BUILD)
