all: README.md

README.md: make-readme-markdown.el top-o-the-mornin.el
	emacs --script $< < top-o-the-mornin.el >$@ 2>/dev/null

make-readme-markdown.el:
	wget -q -O $@ https://raw.github.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el

.INTERMEDIATE: make-readme-markdown.el
