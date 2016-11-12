SRC = src
BUILD = build

build: build-directory
	elm make $(SRC)/Main.elm --output $(BUILD)/app.js
	cp $(SRC)/index.html $(BUILD)

build-directory:
	mkdir -p $(BUILD)
