#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
declgl_preset="${DECLGL_LINUX_PRESET:-linux-release}"
export DECLGL_BUILD_DIR="${DECLGL_BUILD_DIR:-$repo_root/declgl-desktop/build/$declgl_preset}"
packages=(ml_regl_core regl_backend regl_desktop regl_js)

"$repo_root/build.sh"

cd "$repo_root"
eval "$(opam env)"

for package in "${packages[@]}"; do
    echo "==> Pinning $package to $repo_root"
    opam pin add -y --no-action "$package" "$repo_root"
done

echo "==> Installing pinned packages: ${packages[*]}"
opam install -y "${packages[@]}"
