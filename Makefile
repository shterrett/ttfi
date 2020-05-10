default: haskell tex
	cp tex-work/ttfi.pdf ./ttfi.pdf

haskell:
	lhs2TeX src/TTFI.lhs > tex-work/ttfi.tex

tex:
	cd tex-work && pdflatex ttfi.tex

clean:
	rm tex-work/*
