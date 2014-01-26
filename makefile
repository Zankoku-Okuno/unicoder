
.PHONY: all clean install remove purge

all: unicoder

clean:
	rm unicoder unicoder.o unicoder.hi test_results

install: unicoder
	cp -r zankoku-okuno /etc
	cp unicoder /usr/bin

purge: remove
	rm /etc/zankoku-okuno/unicoder/*.conf
remove:
	rm /usr/bin/unicoder

unicoder: ./unicoder.hs
	ghc $^

check: unicoder test_results
	./unicoder -c zankoku-okuno/unicoder/default.conf test_results
	cat test_results


test_results: test
	cp test test_results
