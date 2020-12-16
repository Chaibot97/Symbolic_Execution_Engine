export PATH := $(shell pwd)/bin:$(PATH)

see:
	cabal build :see-hs
	mkdir -p bin
	echo "#!/bin/bash\npython3 src/see.py" '"$$@"' > bin/see
	chmod +x bin/see

clean:
	rm -rf bin dist-newstyle