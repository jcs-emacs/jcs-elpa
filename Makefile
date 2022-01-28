SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

.PHONY: generate_archive_json generate_badges_version generate_badges_downloads

generate_archive_json:
	@echo "Generating archive.json file..."
	@$(EMACS) -nw --batch -l "./bin/generate-archive-json.el"

generate_badges_version:
	@echo "Generating version badges.."
	@$(EMACS) -nw --batch -l "./bin/generate-badges-version.el"

generate_badges_downloads:
	@echo "Generating downloads badges.."
	@$(EMACS) -nw --batch -l "./bin/generate-badges-downloads.el"
