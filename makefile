FSC=fsc
FLAGS=--warn:5 --warnon:1182 --optimize+ --tailcalls+ --target:exe --out:fscalc.exe

fscalc:
	$(FSC) "src/utility.fs" "src/sign.fs" "src/operators.fs" "src/syntax.fs" "src/expression.fs" "src/calculator.fs" $(FLAGS)

.PHONY: clean
clean:
	rm -rf *.exe
