# SPDX-FileCopyrightText: 2021 Arthur Breitman
# SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

# Ligo executable
LIGO ?= ligo

# Compile code
BUILD = $(LIGO) compile-contract --syntax cameligo

# Where to put build files
OUT ?= out

.PHONY: all

all: \
	$(OUT)/segmented_cfmm.tz

# Compile LIGO contract into its michelson representation.
$(OUT)/segmented_cfmm.tz: ligo/**
	mkdir -p $(OUT)
	# ============== Compiling contract ============== #
	$(BUILD) ligo/main.mligo main --output-file $(OUT)/segmented_cfmm.tz

test: all
	$(MAKE) -C haskell test PACKAGE=segmented-cfmm-ligo-meta \
	SEGMENTED_CFMM_LIGO_PATH=../$(OUT)/segmented_cfmm.tz
