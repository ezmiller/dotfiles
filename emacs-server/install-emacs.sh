#!/usr/bin/env bash
# Install Emacs 30.1 from source on Amazon Linux 2023
# Usage: bash install-emacs.sh

set -euo pipefail

EMACS_VERSION="30.1"
EMACS_URL="https://ftp.gnu.org/gnu/emacs/emacs-${EMACS_VERSION}.tar.xz"
BUILD_DIR="/tmp/emacs-build"

echo "==> Installing build dependencies..."
sudo dnf groupinstall -y "Development Tools"
sudo dnf install -y \
  ncurses-devel \
  gnutls-devel \
  libxml2-devel \
  jansson-devel \
  texinfo \
  libtree-sitter-devel || true  # may not be in AL2023 repo

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
