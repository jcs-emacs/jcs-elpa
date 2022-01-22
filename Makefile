SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

.PHONY: startup

build:
	@echo "Building archive json..."
	@$(EMACS) -nw --batch -l "./docs/scripts/generate-archive-json.el"
