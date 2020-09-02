-module(useless).

-export([test/0, reco/0, helper/1]).

-record(bid_device_ext, {
    liveramp_hhid                   :: undefined | integer(),
    liveramp_pid                    :: undefined | integer(),
    devicetype                      :: undefined | integer()
}).

reco() ->
	#bid_device_ext{liveramp_pid=3, liveramp_hhid=8}.

test() ->
	Test = reco(),
	helper(Test).


helper(#bid_device_ext{
		  liveramp_pid = undefined,
		  liveramp_hhid = undefined} = Test) ->
	io:format("here -1");
helper(#bid_device_ext{
		  liveramp_pid = undefined,
		  liveramp_hhid = Hhid} = Test) ->
	io:format("here 0");
helper(#bid_device_ext{
		  liveramp_pid = Pid,
		  liveramp_hhid = Hhid} = Test) ->
	io:format("here 1").
