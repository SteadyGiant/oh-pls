SHELL := /usr/bin/env bash
.DEFAULT_GOAL := data

.PHONY: data

data:
	Rscript --vanilla clean.r
