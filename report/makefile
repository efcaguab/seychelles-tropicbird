all: report supplementary

report: report.docx

report.docx: report.md report-template.docx
	pandoc +RTS -K512m -RTS report.md --to docx --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output report.docx --filter /usr/local/bin/pandoc-citeproc --highlight-style tango --bibliography references.bib --reference-docx report-template.docx

report.md: report.Rmd
	Rscript -e "knitr::knit('report.Rmd')"