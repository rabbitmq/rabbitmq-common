# Extended Erlang/OTP supervisor
This file is a copy of supervisor.erl from the R16B Erlang/OTP
distribution, with the following modifications:

1. The module name is supervisor2.

2. A find_child/2 utility function has been added.

3. Added an 'intrinsic' restart type. Like the transient type, this
type means the child should only be restarted if the child exits
abnormally. Unlike the transient type, if the child exits
normally, the supervisor itself also exits normally. If the
child is a supervisor and it exits normally (i.e. with reason of
'shutdown') then the child's parent also exits normally.

4. Child specifications can contain, as the restart type, a tuple
{permanent, Delay} | {transient, Delay} | {intrinsic, Delay}
where Delay >= 0 (see point (4) below for intrinsic). The delay,
in seconds, indicates what should happen if a child, upon being
restarted, exceeds the MaxT and MaxR parameters. Thus, if a
child exits, it is restarted as normal. If it exits sufficiently
quickly and often to exceed the boundaries set by the MaxT and
MaxR parameters, and a Delay is specified, then rather than
stopping the supervisor, the supervisor instead continues and
tries to start up the child again, Delay seconds later.  
Note that if a child is delay-restarted this will reset the
count of restarts towrds MaxR and MaxT. This matters if MaxT >
Delay, since otherwise we would fail to restart after the delay.  
Sometimes, you may wish for a transient or intrinsic child to
exit abnormally so that it gets restarted, but still log
nothing. gen_server will log any exit reason other than
'normal', 'shutdown' or {'shutdown', _}. Thus the exit reason of
{'shutdown', 'restart'} is interpreted to mean you wish the
child to be restarted according to the delay parameters, but
gen_server will not log the error. Thus from gen_server's
perspective it's a normal exit, whilst from supervisor's
perspective, it's an abnormal exit.

5. Normal, and {shutdown, _} exit reasons are all treated the same (i.e. are regarded as normal exits).

6. Rename the module to supervisor3.

7. Introduce post_init callback.
If Callback:init/1 returns 'post_init', Callback:post_init/1 is called to perform the genuine initialization work.
This is to avoid crashes in Callback:init/1 which may prevent the parent supervisor from restarting it.

8. Call os:timestamp/0 and timer:now_diff/2 for timestamps.

9. Ignore delayed retry in MaxR accumulation.

10. Use specs by default

Modifications 1-5 are (C) 2010-2013 GoPivotal, Inc.

Modifications 6-10 are (C) 2015-2016 Klarna AB
