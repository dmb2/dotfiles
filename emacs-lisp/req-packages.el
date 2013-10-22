(setq my-packages '(acdabbrev acejumpmode acslime ampc archives auctex autocomplete
			      autopair bbdb calfw calfwgcal celdoc clojuremode
			      colorthemesanityinctomorrow concurrent ctable dash deferred diminish
			      ein emms emmsmarkext epc epl fuzzy geiser gh gitcommitmode
			      gitrebasemode helm highlightindentation imagedired+ ipython jabber
			      jedi logito magit magitghpulls magithub melpa minimap multiplecursors
			      notify nrepl pabbrev paredit pcache pkginfo popup powerline
			      rainbowdelimiters rectmark request s smartparens solarizedtheme
			      tabkey2 w3m websocket wgrep yasnippet yasnippetbundle))
(mapc (lambda (name)
	(install-package name)) my-packages)
