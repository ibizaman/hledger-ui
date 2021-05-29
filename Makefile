.PHONY: backend-release-build
.PHONY: backend-release-run
.PHONY: backend-build
.PHONY: backend-test
.PHONY: backend-run
.PHONY: common-build
.PHONY: common-test
.PHONY: common-release-build
.PHONY: frontend-release-build
.PHONY: frontend-build
.PHONY: frontend-test
.PHONY: frontend-run
.PHONY: frontend-desktop-release-build
.PHONY: frontend-desktop-build
.PHONY: frontend-desktop-run
.PHONY: hoogle-build-backend
.PHONY: hoogle-build-common
.PHONY: hoogle-build-frontend
.PHONY: hoogle-generate
.PHONY: hoogle-serve
.PHONY: hoogle
.PHONY: cachix-enable
.PHONY: cachix-push
.PHONY: clean-cabals
.PHONY: clean


all: backend-release-build backend-build common-release-build common-build frontend-warp-release-build frontend-warp-build frontend-desktop-release-build frontend-desktop-build android-build


common/common.cabal: common/package.yaml
	nix-shell --pure --run 'hpack $<'

backend/backend.cabal: backend/package.yaml
	nix-shell --pure --run 'hpack $<'

frontend/frontend.cabal: frontend/package.yaml
	nix-shell --pure --run 'hpack $<'

cabals: common/common.cabal backend/backend.cabal frontend/frontend.cabal


backend-release-build: cabals
	nix-build --pure -o backend-result -A ghc.backend

backend-release-run:
	./backend-result/bin/backend-exe

backend-build: cabals
	nix-shell --pure --run 'cabal build backend'

backend-test: cabals
	nix-shell --pure --run 'cabal test backend'

backend-run: cabals
	nix-shell --pure --run 'cabal run backend-exe'


common-release-build: cabals
	nix-build --pure -o common-result -A ghc.common

common-build: cabals
	nix-shell --pure --run 'cabal build common'

common-test: cabals
	nix-shell --pure --run 'cabal test common'


frontend-release-build: cabals
	nix-build --pure -o frontend-result -A ghcjs.frontend

frontend-release-run: cabals
	@echo "Open in your browser file://$$PWD/frontend-result/bin/frontend-exe.jsexe/index.html"

frontend-build: cabals
	nix-shell --pure --run "cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs build frontend" shell.ghcjs.nix

frontend-test: cabals
	nix-shell --pure --run "cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs test frontend" shell.ghcjs.nix

frontend-run: cabals
	@echo "Open in your browser file://$$PWD/$$(find dist-ghcjs/ -name index.html)"


frontend-warp-release-build: cabals
	nix-build --pure -o frontend-warp-result --arg useWarp true -A ghc.frontend

frontend-warp-release-run: cabals
	./frontend-warp-result/bin/frontend-exe

frontend-warp-build: cabals
	nix-shell --pure --run "cabal build frontend" --arg useWarp true shell.nix

frontend-warp-run: cabals
	nix-shell --pure --run "cabal run frontend" --arg useWarp true shell.nix


frontend-desktop-release-build: cabals
	nix-build --pure -o frontend-desktop-result --arg useWarp false -A ghc.frontend

frontend-desktop-release-run: cabals
	./frontend-desktop-result/bin/frontend-exe

frontend-desktop-build: cabals
	nix-shell --pure --run "cabal build frontend" --arg useWarp false shell.nix

frontend-desktop-run: cabals
	nix-shell --pure --run "cabal run frontend" --arg useWarp false shell.nix


android-build: cabals
	nix-build -o android-result -A android.frontend --arg config '{system="x86_64-linux";}'

ios-build: cabals
	nix-build -o ios-result -A ios.frontend --arg config '{system="x86_64-darwin";}'


hoogle-build-backend: cabals
	nix-shell --pure --run 'cabal haddock --haddock-hoogle --haddock-html --haddock-all --haddock-quickjump --haddock-hyperlink-source backend'

hoogle-build-common: cabals
	nix-shell --pure --run 'cabal haddock --haddock-hoogle --haddock-html --haddock-all --haddock-quickjump --haddock-hyperlink-source common'

hoogle-build-frontend: cabals
	nix-shell --pure --run "cabal haddock --haddock-hoogle --haddock-html --haddock-all --haddock-quickjump --haddock-hyperlink-source frontend"

hoogle-generate:
	nix-shell --pure --run 'GHC_PACKAGE_PATH=./dist-newstyle/packagedb/ghc-8.6.5/: hoogle generate --quiet --local --database=hoogle/database.hoo'

hoogle-serve:
	nix-shell --pure --run 'hoogle serve --local -p 65000 --database=hoogle/database.hoo'

hoogle: hoogle-build-backend hoogle-build-common hoogle-build-frontend hoogle-generate hoogle-serve


cachix-enable:
	cachix use ibizaman

cachix-push:
	nix-build --pure -o backend-result -A ghc.backend | cachix push ibizaman
	nix-build --pure -o frontend-desktop-result --arg useWarp false -A ghc.frontend | cachix push ibizaman
	nix-build --pure -o frontend-result -A ghcjs.frontend | cachix push ibizaman
	nix-build --pure -o frontend-warp-result --arg useWarp true -A ghc.frontend | cachix push ibizaman
	(nix-build -o android-result -A android.frontend --arg config '{system="x86_64-linux";}' | cachix push ibizaman) || :
	(nix-build -o ios-result -A ios.frontend --arg config '{system="x86_64-darwin";}' | cachix push ibizaman) || :
	nix-build shell.nix | cachix push ibizaman
	nix-build shell.ghcjs.nix | cachix push ibizaman


clean-cabals:
	rm -f common/common.cabal
	rm -f frontend/frontend.cabal
	rm -f backend/backend.cabal

clean-tmp:
	rm -rf backend-result
	rm -rf common-result
	rm -rf frontend-result
	rm -rf frontend-warp-result
	rm -rf frontend-desktop-result
	rm -rf android-result
	rm -rf ios-result
	rm -rf dist-newstyle
	rm -rf dist-ghcjs

clean-doc:
	rm -rf hoogle

clean-db:
	rm sqlite.db*

clean: clean-cabals clean-tmp clean-doc clean-db
