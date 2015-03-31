doc:
	R --slave -e 'library(roxygen2); roxygenise("pkg")'
	git add --all pkg/man/*.Rd

test:
	R CMD INSTALL --install-tests pkg
	R --slave -e 'library(testthat); setwd(file.path(.libPaths()[1], "rcrunch", "tests")); system.time(test_check("rcrunch", filter="${file}", reporter=ifelse(nchar("${r}"), "${r}", "summary")))'

install-ci:
	R --slave -e 'install.packages(c("jsonlite", "httr", "RJSONIO", "codetools", "testthat", "devtools"), repo="http://cran.at.r-project.org", lib=Sys.getenv("R_LIB"))'
	R CMD INSTALL --install-tests -l $(R_LIB) pkg

test-ci:
	R --slave -e '.libPaths(Sys.getenv("R_LIB")); options(test.user=Sys.getenv("R_TEST_USER"), test.pw=Sys.getenv("R_TEST_PW"), test.api=Sys.getenv("R_TEST_API")); library(testthat); sink(file="rcrunch.tap"); setwd(file.path(.libPaths()[1], "rcrunch", "tests")); test_check("rcrunch", reporter="tap"); sink()'

clean:
	R --slave -e 'options(crunch.api=getOption("test.api"), crunch.email=getOption("test.user"), crunch.pw=getOption("test.pw")); library(rcrunch); login(); rcrunch:::.delete_all_my_datasets()'

check:
	R CMD CHECK --as-cran pkg

vdata:
	cd vignette-data && find *.R | xargs -n 1 R -f
