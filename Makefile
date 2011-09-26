all: compile generate
dist: clean tarball

compile:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; ./rebar compile)

generate:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; ./rebar generate force=1)

tarball:
	@(cd ..; echo "Built tarball $$PWD/bapp.tar.gz"; tar cfz bapp.tar.gz bapp; cd -)

clean:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; ./rebar clean; find . -name "*~" -exec rm {} \;)
