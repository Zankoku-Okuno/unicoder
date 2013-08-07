
.PHONY: all clean install remove

all: unicoder

clean:
	rm unicoder unicoder.o unicoder.hi test_results

install: unicoder /etc/zankoku-okuno/unicoder/
	cp ./symbols.conf /etc/zankoku-okuno/unicoder/
	cp unicoder /usr/bin/

remove:
	rm /etc/zankoku-okuno/unicoder/symbols.conf
	rm /usr/bin/unicoder

unicoder: ./unicoder.hs
	ghc $^

check: unicoder test_results
	./unicoder test_results
	cat test_results


test_results: test
	cp test test_results

/etc/zankoku-okuno/unicoder/: /etc/zankoku-okuno/
	mkdir $@
/etc/zankoku-okuno/:
	mkdir $@
