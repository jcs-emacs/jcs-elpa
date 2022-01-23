SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

.PHONY: generate_archive_json generate_badges

generate_archive_json:
	@echo "Generating archive.json file..."
	@$(EMACS) -nw --batch -l "./bin/generate-archive-json.el"

generate_badges:
	@echo "Generating badges.."
	@$(EMACS) -nw --batch -l "./bin/generate-badges.el"
