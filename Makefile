all:
	#INFO: no_ebin_commit STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport_GLURK;
	#INFO: Deleting euinit test applications dirs
	rm -rf log resource_discovery etcd;
	rm -rf inventory;
	rm -rf host_specs catalog application_dir deployment_specs deployment_specs_test catalog_specs_test catalog_specs;
	rm -rf doc;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beams
	rm -rf *~ */*~ */*/*~;
	rm -rf src/*.beam src/*/*.beam;
	rm -rf test/*.beam test/*/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock
	#INFO: Deleting files and dirs created during execution/runtime 
	rm -rf logs;
	rm -rf *_a;
	#INFO: Compile application		
	rebar3 compile;	
	rm -rf _build*;
	rebar3 edoc;
#	git add *;
#	git add -f *;
#	git commit -m $(m);
#	git push;
	rm -rf _build*;
	git status
	#INFO: no_ebin_commit ENDED SUCCESSFUL
	echo Ok there you go!
build:
	#INFO: with_ebin_commit STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport_GLURK;
	#INFO: Deleting euinit test applications dirs
	rm -rf log resource_discovery etcd;
	rm -rf inventory;
	rm -rf host_specs catalog application_dir deployment_specs_test deployment_specs catalog_specs_test catalog_specs;
	rm -rf doc;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beams
	rm -rf *~ */*~ */*/*~;
	rm -rf src/*.beam src/*/*.beam;
	rm -rf test/*.beam test/*/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock
	#INFO: Deleting files and dirs created during execution/runtime 
	rm -rf logs;
	rm -rf *_a;
	rm -rf _build;
	#INFO: Compile application	
	rebar3 compile;
	rm -rf _build*;
	git status
	#INFO: build ENDED SUCCESSFUL
clean:
	#INFO: clean STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport_*;
	#INFO: Deleting euinit test applications dirs
	rm -rf log resource_discovery etcd;
	rm -rf inventory;
	rm -rf catalog host_specs application_dir deployment_specs_test deployment_specs catalog_specs_test catalog_specs;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beams
	rm -rf *~ */*~ */*/*~;
	rm -rf src/*.beam src/*/*.beam;
	rm -rf test/*.beam test/*/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock
	#INFO: Deleting files and dirs created during execution/runtime 
	rm -rf logs;
	rm -rf *_a;
	#INFO: clean ENDED SUCCESSFUL
eunit: 
	#INFO: eunit STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport_GLURK;
	#INFO: Deleting euinit test applications dirs
	rm -rf log resource_discovery etcd;
	rm -rf inventory;
	rm -rf host_specs  catalog application_dir deployment_specs_test deployment_specs catalog_specs_test catalog_specs;
	rm -rf doc;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beams
	rm -rf src/*.beam src/*/*.beam;
	rm -rf test/*.beam test/*/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock
	#INFO: Deleting files and dirs created during execution/runtime 
	rm -rf logs;
	rm -rf *_a;
	#INFO: Creating eunit test code using test_ebin dir;
	mkdir test_ebin;
	#rm test/dependent_apps.erl;
	#cp /home/joq62/erlang/dev_support/dependent_apps.erl test;
	erlc -I include -I /home/joq62/erlang/include -o test_ebin test/*.erl;
	erlc -I include\
	 -I /home/joq62/erlang/include\
	 -I ..//log_service/include\
	 -o test_ebin ../log_service/src/*.erl;
	erlc -I include\
	 -I /home/joq62/erlang/include\
	 -I ../rd_service/include\
	 -o test_ebin ../rd_service/src/*.erl;
	#INFO: Creating Common applications needed for testing
	#INFO: Compile application
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
#	rm -rf _build*;
	#INFO: Starts the eunit testing .................
	erl -pa ebin -pa priv\
	 -pa test_ebin\
	 -sname balcony_service_a\
	 -run $(m) start\
	 -setcookie a
