#!/usr/bin/env bash
# Install Emacs 30.1 from source on Amazon Linux 2023
# Usage: bash install-emacs.sh

set -euo pipefail

EMACS_VERSION="30.1"
TREE_SITTER_VERSION="0.24.7"
EMACS_URL="https://ftp.gnu.org/gnu/emacs/emacs-${EMACS_VERSION}.tar.xz"
TREE_SITTER_URL="https://github.com/tree-sitter/tree-sitter/archive/refs/tags/v${TREE_SITTER_VERSION}.tar.gz"
BUILD_DIR="/tmp/emacs-build"

# tree-sitter installs to /usr/local, which pkg-config doesn't search by default on AL2023
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:${PKG_CONFIG_PATH:-}"
export LD_LIBRARY_PATH="/usr/local/lib:${LD_LIBRARY_PATH:-}"

echo "==> Installing build dependencies..."
sudo dnf groupinstall -y "Development Tools"
sudo dnf install -y \
  ncurses-devel \
  gnutls-devel \
  libxml2-devel \
  jansson-devel \
  texinfo

# Build tree-sitter from source (not in AL2023 repos)
if ! pkg-config --exists tree-sitter 2>/dev/null; then
  echo "==> Building tree-sitter ${TREE_SITTER_VERSION} from source..."
  mkdir -p "$BUILD_DIR"
  cd "$BUILD_DIR"
  curl -L "$TREE_SITTER_URL" -o tree-sitter.tar.gz
  tar xf tree-sitter.tar.gz
  cd "tree-sitter-${TREE_SITTER_VERSION}"
  make -j"$(nproc)"
  sudo make install
  echo '/usr/local/lib' | sudo tee /etc/ld.so.conf.d/local.conf >/dev/null
  sudo ldconfig
  cd /
  echo "==> tree-sitter installed: $(pkg-config --modversion tree-sitter)"
else
  echo "==> tree-sitter already installed: $(pkg-config --modversion tree-sitter)"
fi

echo "==> Downloading Emacs ${EMACS_VERSION}..."
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"
curl -LO "$EMACS_URL"
tar xf "emacs-${EMACS_VERSION}.tar.xz"
cd "emacs-${EMACS_VERSION}"

echo "==> Configuring (terminal-only, no GUI)..."
./configure \
  --without-x \
  --without-sound \
  --without-xpm \
  --without-jpeg \
  --without-tiff \
  --without-gif \
  --without-png \
  --without-rsvg \
  --without-imagemagick \
  --without-xft \
  --without-libotf \
  --without-m17n-flt \
  --without-toolkit-scroll-bars \
  --without-xaw3d \
  --without-xim \
  --without-gpm \
  --with-gnutls \
  --with-json \
  --with-xml2 \
  --with-tree-sitter \
  --with-modules

echo "==> Building (this takes a few minutes)..."
make -j"$(nproc)"

echo "==> Installing..."
sudo make install

echo "==> Cleaning up..."
rm -rf "$BUILD_DIR"

echo ""
echo "Done! Emacs version:"
emacs --version | head -1
