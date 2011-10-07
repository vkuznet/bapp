all: compile generate
dist: clean tarball

compile:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; ./rebar compile)

generate:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; ./rebar generate force=1)

tarball:
	@(cd ..; echo "Tarball $$PWD/bapp.tar.gz"; echo ".git" > bapp.skip; tar cfz bapp.tar.gz -X bapp.skip bapp; rm bapp.skip; cd -)

clean:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; ./rebar clean; find . -name "*~" -exec rm {} \;)
