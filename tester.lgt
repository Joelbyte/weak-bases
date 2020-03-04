:- initialization((
	% minimize compilation reports to the essential ones
	set_logtalk_flag(report, warnings),
	% load necessary library files
	logtalk_load(types(loader)),
	logtalk_load(sets(loader)),
	logtalk_load(meta_compiler(loader)),
	% load the unit test tool
	logtalk_load(lgtunit(loader)),
	% load application files enabling support for code coverage
	logtalk_load([matrix, operators, relations], [source_data(on), debug(on)]),
	% compile the unit tests file expanding it using the "lgtunit" object
	% to preprocess the tests
	logtalk_load(tests, [hook(lgtunit)]),
	% run all the unit tests
	tests::run
)).
