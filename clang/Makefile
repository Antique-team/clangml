bridge.ml: ast.ml Makefile
	grep -v '^\s*deriving (Show)\s*$$' $< | sed -e 's/\s\+deriving (Show)//;s/= Bridge.\w\+ //' > $@
