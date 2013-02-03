Epgpb = poolboy + epgsql,
Desc   = "A standalone PostgreSQL database connection pool server 
       	    application, created to support declarative application 
	    composition".

--------------------------------------------------------------------

This code is not much more than the poolboy epgsql example with the
full epgsql API implementated in poolboy workers. A code generator
(see `gen/`) builds this application dynamically from the compiled
epgsql binaries.

To use this with rebar:

 * declare epgpb as a dependency of your application, 
 * define your poolboy/epgsql env config on the epgpb application, 
 * `rebar get-deps compile`
 * start the epgpb application as you would any other.

To build and run the example:

 * `cd example`
 * `rebar get-deps compile`
 * `./start.sh`
 * (in the erl shell) `application:start(epgpb_example).`.