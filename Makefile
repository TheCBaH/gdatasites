.PHONY: all build clean format install

all: build

build:
	jbuilder build

install:
	jbuilder install

clean:
	rm -rf _build *.install

format:
	ocamlformat --inplace lib/GdataSitesModel.ml
	ocamlformat --inplace lib/GdataSitesService.ml
