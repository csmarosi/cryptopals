.PHONY : all clean test ut

SOURCES  = $(wildcard *.hs)
BINARIES = $(patsubst %.hs,%,$(SOURCES))

all: clean $(BINARIES)

clean:
	rm -f $(BINARIES) *hi *o

%: %.hs
	ghc -v0 -o $@ $<

test: all ut
	./s01p01 49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d | grep SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t
	./s01p02 1c0111001f010100061a024b53535009181c 686974207468652062756c6c277320657965 | grep 746865206b696420646f6e277420706c6179
	./s01p03 1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736 | grep bacon
	./s01p04 | grep party
	./hexToAscii 0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f | ./s01p05 ICE | grep Burning
	base64 --decode 6.txt | ./s01p06 | grep -E '\[|ringin'
	base64 --decode 7.txt | ./s01p07 | grep doctor

ut: clean Set1Module_Test
	./Set1Module_Test
