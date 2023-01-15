#!/bin/sh

. .github/scripts/env.sh

ecabal() {
	cabal "$@"
}

nonfatal() {
	"$@" || "$* failed"
}

raw_eghcup() {
	"$GHCUP_BIN/ghcup${ext}" -v -c "$@"
}

eghcup() {
	if [ "${OS}" = "Windows" ] ; then
		"$GHCUP_BIN/ghcup${ext}" -c -s "file:/$CI_PROJECT_DIR/data/metadata/ghcup-${JSON_VERSION}.yaml" "$@"
	else
		"$GHCUP_BIN/ghcup${ext}" -c -s "file://$CI_PROJECT_DIR/data/metadata/ghcup-${JSON_VERSION}.yaml" "$@"
	fi
}

sha_sum() {
	if [ "${OS}" = "FreeBSD" ] ; then
		sha256 "$@"
	else
		sha256sum "$@"
	fi

}

git_describe() {
	git config --global --get-all safe.directory | grep '^\*$' || git config --global --add safe.directory "*"
	git describe --always
}

install_ghcup() {
	curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 BOOTSTRAP_HASKELL_INSTALL_NO_STACK=yes sh
}

