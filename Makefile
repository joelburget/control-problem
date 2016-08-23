.PHONY: doit open

doit:
	mkdir -p gh-pages
	cp -r old/2016-02-02-karpathy-rl.js src/agent.js index.html fonts img style.css gh-pages
	pulp browserify --optimise --to gh-pages/index.js

open: doit
	open gh-pages/index.html
