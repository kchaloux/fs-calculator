FSC=fsc
FLAGS=--warn:5 --warnon:1182 --optimize+ --tailcalls+ --target:exe --out:fscalc.exe

fscalc:
	$(FSC) "src/calculator.fs" $(FLAGS)
