REBOL [ 
	description: {Compiler for a subset of rebol to more static (FP-ish) languages.
		First will be javasript. Something like Go / Ocaml? / C? / Java? could also be
		cool. You create a new language by setting the callbacks.
		}
	subset: {Compiler will work on a limited subset of rebol:
		- SUPPORTS: functions, objects, serries, variables, closures, 
			anonymouse functions
		- DOESN'T SUPPORT: no refinements, static form for functions and 
			everything
		- runtime dynamic befaviour which will not be allowed will be a little
			compensated with compile time macros, and in case of JavaScript Eval
		}
	ideas: {
		- when compiled code is checked for some basic correctness
		- if static langs compilation would use rebol's optional types
			in case if target supports overloading it could create multiple functions 
			if more than 1 type allowed.
		- settings like no silent globals alowed that could be off/on
		- for javascript.. a dummy browser model would be provided. If possible
			it could be made to fully simulate the core of the browser DOM
		- you could have dialects that geenrate code and then compile
		}
	point: {the point is not that I am such rebol fanatic *that* I want to
		code my javascript in rebol. I love REBOL, but such thing also has 
		many negative sides, like indirection, additional step, harder debugging.
		***It is that I want to share the codebase between client and server, and 
		server is written in REBOL***
	}
	TODO: {=Todo for Primo=
		- add support for JSON data structures and see where it brings us in terms
			of support for JS objects too
		- add support for basic if either foreach
		- start experimenting with enabling simple DOM manipulation code
	}
]

accumulate: func [ 'word 'accum start data body ] [
	either none? data [ none ] [
		set :accum start
		foreach :word data compose [ set :accum ( body ) ]
		get :accum
	]
] 

rebs: make object! [ 
	pr: :prin
	debug: false
	get-arg-length: func [ f /local cnt] [ cnt: 0 
		foreach a first get f [ 
			either equal? type? a do 'word! [ cnt: cnt + 1 ] [ return cnt ] 
		] cnt
	]
	
	; callbacks
	lit-string: func [ v ] [ rejoin [{"} v {"}] ]
	lit-integer: func [ v ] [ to-string v ]
	lit-decimal: func [ v ] [ to-string v ]
	variable-def: func [ v ] [ rejoin [ "var " v " = " ] ]
	statement-end: func [ ] [ ";" ]
	func-open: func [ n ] [ rejoin [ n "(" ] ]
	func-close: func [ ] [ ")" ]
	func-arg-sep: func [ ] [ "," ]
	funcdef-args: func [ a /local x acc ] [ 
		rejoin [ "(" accumulate x acc "" a 
		[ rejoin [acc x either equal? x last a [ "" ][ "," ] ] ] ")" ]]
	funcdef-body-open: func [ ] [ "{" ]
	funcdef-body-close: func [ ] [ "}" ]
	funcdef-keyw: func [ ] [ " function" ]
	prc: func [ t ] (either not debug [ [] ] [ [ prin rejoin [ "/*" t "*/" ] ] ])
	
	compile-expr: func [ c lvl /local V W m typ i ] [
		parse c [ 
			[
				set V integer! (pr lit-integer V)
				| set V decimal! (pr lit-decimal V)
				| set V string! (pr lit-string V) 
				| set W word! m: (
					switch/default W [
						func [
							pr funcdef-keyw
							pr funcdef-args first m
							foreach arg first m [ set :arg 0 ]  
							pr funcdef-body-open
								compile second m 0
							pr funcdef-body-close
							m: next next m ]
					] [
						typ: type? get :W
						case compose [ 
							( any [ equal? typ do 'action! equal? typ do 'function! ] ) 
							[
								pr func-open W
								repeat i len: get-arg-length :W [
									m: compile-expr m (+ lvl 1)
									if lesser? i len [ pr func-arg-sep ]
								]
								pr func-close ]
							( equal? typ do 'integer! )
							[ pr to-string W ]
					]]) :m 
				| set W block! ( prin mold W ) ; todo -- add some json generation
		] m: ] m
	]
	
	compile: func [ c lvl /local A m new ] [
		parse c  
		[ some 
			[ 
				set A set-word! (pr variable-def A) m:
				(new: compile-expr m lvl) :new
				(if zero? lvl [ pr statement-end ]) 
				|	
				m: word! (new: compile-expr m lvl) :new
	]]]
]

t1: does [ rebs/compile [ abc: add 10 32 ] 0 ]
t2: does [ rebs/compile [ abc: add 10 32 b: add 3 4 co: join "ja" "ne" print "janko" ] 0 ]
t3: does [ rebs/compile [ abc: add add 30 20 32 ] 0 ]
t4: does [ rebs/compile [ sum: func [ a1 b1 ] [ ret: add 1 3 ] ] 0 ]
t5: does [ rebs/compile [ sum: func [ a1 b1 ] [ ret: add a1 b1 ] ] 0 ]
t6: does [ rebs/compile [ sum: func [ a1 b1 ] [ ret: add a1 join "janko" b1 ] ] 0 ]
t6: does [ rebs/compile [ 
		sum: func [ a1 b1 ] [ ret: add a1 join "janko" b1 ]
		a: mod 100 200
		c: "jaja"
		blk: func [ a ] [ add a 100 ] 
	] 0 
]

rel: does [ do %rebjs.r ]

halt