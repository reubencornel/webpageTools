binDir=${PWD}/bin
srcDir=${PWD}/src

all: pageGen articleGen menuConvert $(binDir)

$(binDir): $(binDir)
	mkdir $(binDir)

pageGen: $(srcDir)/pagegen.lisp
	@echo "Compiling the page generator..."
	@sbcl --eval "(progn (load \"$(srcDir)/pagegen.lisp\") (sb-ext:save-lisp-and-die \"$(binDir)/pagegen\" :toplevel 'main-func :executable t))"  @&>/dev/null

articleGen: $(srcDir)/articleGen.lisp
	@echo "Compiling the article generator...."
	@sbcl --eval "(progn (load \"$(srcDir)/articleGen.lisp\") (sb-ext:save-lisp-and-die \"$(binDir)/articleGen\" :toplevel 'main-func :executable t))"  @&>/dev/null

menuConvert: $(srcDir)/menuconvert.lisp
	@echo "Compiling the menu file convertor...."
	@sbcl --eval "(progn (load \"$(srcDir)/menuconvert.lisp\") (sb-ext:save-lisp-and-die \"$(binDir)/menuconvert\" :toplevel 'main-func :executable t))"  @&>/dev/null

clean:
	@echo "Cleaning up bin dir"
	@rm -f $(binDir)/*
