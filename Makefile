## Compile documentation, manual, and package
package: manual
	cd ..; R CMD build mcmctk

manual: doc
	R CMD Rd2pdf . --title="Package \`mcmctk'" --output=./manual.pdf --force

doc:
	R -e "devtools::document()"

