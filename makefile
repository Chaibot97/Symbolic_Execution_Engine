export PATH := $(shell pwd)/bin:$(PATH)

all:
	make .hs
	make .py

.hs:
	cabal configure \
	&& cabal build --ghc-options="-Wall"

.py:
	mkdir -p bin
	echo "#!/bin/bash\npython3 src/see.py" '"$$@"' > bin/see
	chmod +x bin/see

clean:
	cabal clean
	rm -rf bin dist-newstyle