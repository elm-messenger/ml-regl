#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
declgl_preset="${DECLGL_LINUX_PRESET:-linux-release}"
declgl_build_dir="${DECLGL_BUILD_DIR:-$repo_root/declgl-desktop/build/$declgl_preset}"
packages=(ml_regl_core regl_backend regl_desktop regl_js)
opam_deps=(dune ocaml-protoc-plugin js_of_ocaml js_of_ocaml-ppx)

if ! command -v opam >/dev/null 2>&1; then
    echo "error: opam was not found" >&2
    exit 1
fi

if [[ ! -f "$declgl_build_dir/libdeclgl.a" ]]; then
    echo "==> Building declgl-desktop dependency"
    DECLGL_LINUX_PRESET="$declgl_preset" "$repo_root/declgl-desktop/scripts/build_linux.sh"
fi

if [[ ! -f "$declgl_build_dir/libdeclgl.a" ]]; then
    echo "error: expected desktop archive was not found: $declgl_build_dir/libdeclgl.a" >&2
    exit 1
fi

missing_deps=()
for package in "${opam_deps[@]}"; do
    if ! opam list --installed --short "$package" 2>/dev/null | grep -Fxq "$package"; then
        missing_deps+=("$package")
    fi
done

if (( ${#missing_deps[@]} > 0 )); then
    echo "==> Installing missing opam packages: ${missing_deps[*]}"
    opam install -y "${missing_deps[@]}"
fi

echo "==> Building ml-regl packages"
cd "$repo_root"
eval "$(opam env)"
DECLGL_BUILD_DIR="$declgl_build_dir" dune build --profile release --for-release-of-packages "$(IFS=,; echo "${packages[*]}")" @install
