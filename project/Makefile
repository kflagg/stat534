paperfilename=paper
presfilename=pres

all: $(paperfilename).pdf

clean:
	rm -rf *.aux *.bbl *.blg *-blx.bib *.gz *.log *.tex *.xml .Rhistory *~ figure cache

$(paperfilename).pdf: $(paperfilename).tex
	pdflatex $(paperfilename).tex
	bibtex $(paperfilename)
	pdflatex $(paperfilename).tex
	pdflatex $(paperfilename).tex
	Rscript --vanilla -e "require(extrafont);embed_fonts('$(paperfilename).pdf')"

$(paperfilename).tex: $(paperfilename).rnw
	Rscript --vanilla -e "require(knitr);knit('$(paperfilename).rnw')"

$(paperfilename).rnw:

