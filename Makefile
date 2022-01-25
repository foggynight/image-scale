.PHONY: all
all:
	csc -o image-scale -d0 -strict-types image-scale.scm

.PHONY: eggs
eggs:
	chicken-install -s srfi-4 srfi-141 stb-image stb-image-write
