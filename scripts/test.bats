#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2021 Arthur Breitman
# SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

ligo_exe="baseDAO-ligo-meta"

@test "Print LIGO metadata" {
  "$ligo_exe" print-metadata --frozen-token-symbol=POG > metadata.json
}
