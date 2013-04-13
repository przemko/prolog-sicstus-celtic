all: celtic.exe

celtic.exe: main.sav
	spld --output=celtic.exe --static main.sav -ltcl8.5 -ltk8.5

main.sav: makefile.pl main.pl celtic.pl 
	sicstus -i -f -l makefile.pl

clean:
	rm -f celtic.exe main.sav *~

