# SPDX-FileCopyrightText: 2021 Arthur Breitman
# SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

# Ligo executable
LIGO ?= ligo

# Compile code
BUILD = $(LIGO) compile-contract --syntax cameligo

# Compile storage
BUILD_STORAGE = $(LIGO) compile-storage --syntax cameligo

# Where to put build files
OUT ?= out

# Where to put typescript files
TS_OUT ?= typescript

# Utility function to escape double quotes
escape_double_quote = $(subst $\",$\\",$(1))

.PHONY: all

all: \
	$(OUT)/segmented_cfmm.tz $(OUT)/default_storage.tz

# Compile LIGO contract into its michelson representation.
$(OUT)/segmented_cfmm.tz: ligo/**
	mkdir -p $(OUT)
	# ============== Compiling contract ============== #
	$(BUILD) ligo/main.mligo main --output-file $(OUT)/segmented_cfmm.tz

$(OUT)/default_storage.tz :
	mkdir -p $(OUT)

	$(BUILD_STORAGE) --output-file $(OUT)/default_storage.tz \
      ligo/main.mligo main "default_storage"


lib: all
	$(MAKE) -C haskell build PACKAGE=segmented-cfmm \
		STACK_DEV_OPTIONS="--fast --ghc-options -Wwarn"

metadata : x_token_symbol = x
metadata : x_token_name = "Token X"
metadata : x_token_decimals = 1
metadata : y_token_symbol = y
metadata : y_token_name = "Token Y"
metadata : y_token_decimals = 1
metadata : output = metadata.json
metadata: lib all
	$(MAKE) -C haskell exec PACKAGE=segmented-cfmm \
		EXEC_ARGUMENTS="print-metadata \
		--x-token-symbol $(x_token_symbol) --x-token-name $(call escape_double_quote,$(x_token_name)) \
		--x-token-decimals $(x_token_decimals) \
		--y-token-symbol $(y_token_symbol) --y-token-name $(call escape_double_quote,$(y_token_name)) \
		--y-token-decimals $(y_token_decimals) \
		" EXEC_OUTPUT=$(output)

test: all
	$(MAKE) -C haskell test PACKAGE=segmented-cfmm \
		SEGMENTED_CFMM_PATH=../$(OUT)/segmented_cfmm.tz

typescript: all
	$(MAKE) -C haskell build PACKAGE=segmented-cfmm \
		STACK_DEV_OPTIONS="--fast --ghc-options -Wwarn" \
		SEGMENTED_CFMM_PATH=../$(OUT)/segmented_cfmm.tz

	rm -rf $(TS_OUT)/segmented-cfmm/src/generated/*
	stack exec -- segmented-cfmm generate-typescript --target=$(TS_OUT)/segmented-cfmm/src/generated/