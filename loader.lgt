:- initialization((
	% minimize compilation reports to the essential ones
	set_logtalk_flag(report, warnings),
	% load necessary library files
	logtalk_load(library(meta_compiler_loader)),	
	% load application files
	logtalk_load([matrix, operators], [optimize(on)]),
	logtalk_load(relations, [hook(meta_compiler), optimize(on)])
)).
