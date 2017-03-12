filename=exam1

all: $(filename).pdf

clean:
	rm -rf *.aux *.gz *.log *.tex .Rhistory *~ figure cache

$(filename).pdf: $(filename).tex wheat.txt CN.dat
	pdflatex $(filename).tex
	Rscript --vanilla -e "require(extrafont);embed_fonts('$(filename).pdf')"

$(filename).tex: $(filename).rnw
	Rscript --vanilla -e "require(knitr);knit('$(filename).rnw')"

wheat.txt:

CN.dat:

