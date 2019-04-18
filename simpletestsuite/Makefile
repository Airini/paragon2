I_PI_TOKEEP := issue121/ParentParam.pi issue46/S.pi issue52/C.pi

all: Test

%: %.hs *.hs
	ghc -main-is $* -threaded --make $<

clean:
	rm -f Test
	rm -f *.hi *.o
	rm -f *~
	rm -f good/*/*.java
	rm -f issues/*/*.java
	find issues/*/*.pi $(patsubst %,! -path issues/%,$(I_PI_TOKEEP)) | xargs rm

# Repo contains .pi, keep them for now
#rm -f good/$$l/*.pi ; \

test: Test
	./Test
