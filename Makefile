.PHONY: test
test:
	emacs -batch -l ert -l undo-propose.el -l undo-propose-test.el -f ert-run-tests-batch-and-exit
