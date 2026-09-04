MAIN:=./src/main.roc

run:
	roc run ${MAIN}

build:
	roc build ${MAIN}

test:
	roc test ${MAIN}

clean:
	@rm ./main
