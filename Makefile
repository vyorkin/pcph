build:
		rm -f result
		nix-build release.nix
clean:
		rm -f .ghc.environment*
		nix-shell --run 'cabal new-clean'
c2n:
		cabal2nix . > project.nix

.PHONY: build clean c2n
