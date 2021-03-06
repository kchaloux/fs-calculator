FSC=fsc
FLAGS=--warn:5 --warnon:1182 --optimize+ --tailcalls+ --target:exe --out:calculate.exe
FILES = \
	src/utility.fs \
	src/tokenizer.fs \
	src/argument.fs \
	src/sign.fs \
	src/operator.fs \
	src/expression.fs \
	src/infix.fs \
	src/rpn.fs \
	src/main.fs

fscalc:
	$(FSC) $(FILES) $(FLAGS)

.PHONY: clean
clean:
	rm -rf *.exe
