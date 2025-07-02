#!/usr/bin/env bash
set -euo pipefail

BUILD_DIR=build

if [[ ! -d "$BUILD_DIR" ]]; then
  echo "⏳ Creating '$BUILD_DIR' directory…"
  mkdir "$BUILD_DIR"
fi

cd "$BUILD_DIR"

echo "⚙️  Running CMake…"
cmake -G Ninja .. -DCMAKE_BUILD_TYPE=Debug

echo "🏗️  Building with Ninja…"
ninja -j"$(nproc)"

echo "✅ Build complete."